library('DBI')
library('rjson')
library('RSQLite')
library('lubridate')
library('stringr')

csvData <- function() {
  read.csv('weights.csv', colClasses = c('POSIXct', rep(NA,4)))
}

sqliteSelect <- function(db, query) {
  sqliteDbd <- dbDriver('SQLite')
  con <- dbConnect(sqliteDbd, dbname = db)
  res <- dbGetQuery(con, query)
  dbDisconnect(con)
  res
}

sqlData <- function(user = 1) {
  d <- transform(sqliteSelect('weights.sqlite3',
                 paste("select time, weight, fat_percent from weights where user_id=", user, sep = '')),
                 time = as.POSIXct(time, tz="UTC"))
	d$lean_mass <- (1 - d$fat_percent / 100) * d$weight
	d$fat_mass <- d$weight - d$lean_mass
	d
}

mfpDataFrame <- function() {
  mfpDateTotal <- function(filename) {
    colname <- tolower(str_extract(filename, "[A-Za-z_]+"))
    con <- file(filename)
    j <- fromJSON(readLines(con, warn = FALSE))
    close(con)
    date <- as.POSIXct(sapply(j$data, function(l) { l$date }), format = '%m/%d')
    frame <- as.data.frame(date)
    frame[frame$date > now(),] <- frame[frame$date > now(),] - dyears(1)
    frame$date <- as.Date(frame$date)
    frame[colname] <- as.numeric(sapply(j$data, function(l) { l$total }))
    frame[frame[colname] == 0,][colname] <- NA
    frame
  }
  
  mergeAll <- function(dataFrames, merged = NULL) {
    if (length(dataFrames) == 0) {
      merged
    } else {
      f <- dataFrames[1]
      rest <- dataFrames[-1]
      if (is.null(merged)) {
        mergeAll(rest, f)
      } else {
        mergeAll(rest, merge(merged, f, by=c("date")))
      }
    }
  }
  
  mfp <- mergeAll(lapply(c('Calories.json', 'Net_Calories.json',
                           'Carbs.json', 'Protein.json', 'Fat.json',
                           'Sugar.json', 'Fiber.json',
                           'Sodium.json', 'Potassium.json'),
                         mfpDateTotal))
  mfp <- mfp[!is.na(mfp$calories),]
  row.names(mfp) <- NULL
  mfp
}

lastDays <- function(days, bf = bodyfat, skip = 0) {
  bf[bf$time >= now() - ddays(days + skip) & bf$time <= now() - ddays(skip),]
}

pacificDate <- function(d) {
  as.Date(
    floor_date(
      with_tz(d, "America/Los_Angeles"), "day"))
}

dayMeans <- function(bf = bodyfat) {
  aggregate(bf[,c("weight", "fat_percent", "lean_mass", "fat_mass")],
            by = list(date = pacificDate(bf$time)),
            mean)
}

scaleDiffs <- function(df, lag = 3) {
  firstFrame <- data.frame(df[1:(dim(df)[1] - lag),1])
  names(firstFrame) <- c(names(df)[1])
  diffFrame <- data.frame(diff(as.matrix(df[,-1]), lag = lag))
  names(diffFrame) <- sapply(names(diffFrame), function (n) { paste("diff_", n, sep = '') })
  cbind(firstFrame, diffFrame)
}



bodyfat <- sqlData()
mfp <- mfpDataFrame()
jdf <- merge(mfp, dayMeans(bodyfat))



toIntervals <- function(times, days) {
	length <- days * 3600 * 24
	as.Date(as.POSIXct(length * (as.double(times) %/% length), origin = "1970-01-01"))
}

fatBox <- function(bf = bodyfat, days = 3) {
	with(bf, boxplot(fat_percent ~ toIntervals(time, days)))
	title('Body Fat %')
}

leanBox <- function(bf = bodyfat, days = 3) {
	with(bf, boxplot(lean_mass ~ toIntervals(time, days)))
	title('Lean Mass')
}

allPlots <- function(bf = bodyfat) {
	split.screen(c(1,2))
	screen(1)
  fatBox(bf)
	screen(2)
  leanBox(bf)
  close.screen(all = TRUE)
}



plotDayOfWeek <- function(data = bodyfat, dependent = "fat_percent") {
  with(data, boxplot(formula(sprintf("%s ~ wday(time)", dependent)),
                     ylab = 'Body Fat %', main = 'Body Composition by Day of Week', xaxt="n"))
  axis(1, at = seq(1,7),
       labels = c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat"))
}

ma <- function(ts, n = 5) { filter(ts, rep(1/n, n), sides = 1) }

plotMovingAverage <- function(bf = dayMeans(bodyfat), n = 5) { plot(na.omit(ma(bf$fat_percent, n))) }

toPerWeek <- function(model) {
  model$coefficients["time"] * 3600 * 24 * 7
}

poundsPerWeek <- function(bf = bodyfat) {
	toPerWeek(with(bf, lm(weight ~ time)))
}

meanNetCals <- function(bf = bodyfat) {
  s <- pacificDate(min(bf$time))
  e <- pacificDate(max(bf$time))
  mean(mfp$net_calories[mfp$date >= s & mfp$date <= e])
}

targetCals <- function(bf = bodyfat, targetPounds = 0) {
  meanNetCals(bf) - 500 * poundsPerWeek(bf) + 500 * targetPounds
}

percentT <- function(bf, m = 8) {
  t.test(bf$fat_percent, alternative = "less", mu = m)
}

pVals <- function(lags = seq(1,7)) {
  unlist(lapply(lags, function(l) { percentT(lastDays(l))$p.value }))
}

pPlot <- function(lags = seq(1,7)) {
  x <- pVals(lags)
  plot(x, xlab = '', ylab = '', ylim = c(0,1))
  abline(0.05, 0, lty = "dashed")
}

goalT <- function(lag = 7) {
  t.test(dayMeans(lastDays(lag))$fat_percent, mu = 8, alternative = "less")
}

goalP <- function(lag = 7) {
  goalT(lag)$p.value
}



linRegPlot <- function(data, column) {
  mdl <- lm(paste(column, "time", sep = " ~ "), data)
  rse <- summary(mdl)$sigma
  
  perWeek <- toPerWeek(mdl)
  x <- data[,"time"]
  y <- data[,column]
  
  split.screen(c(1,2))
  
  screen(1)
  plot(x, y, ylab = '', xlab = '',
       main = sprintf("%s (%.2f)", column, perWeek))
  abline(mdl)
  abline(mdl$coefficients[1] + rse, mdl$coefficients[2], lty = 'dashed')
  abline(mdl$coefficients[1] - rse, mdl$coefficients[2], lty = 'dashed')
  
  screen(2)
  hist <- hist(y, main = '', xlab = '', ylab = '')
  
  close.screen(all = TRUE)
  
  list("hist" = hist,
       "lm" = mdl,
       "t.test" = t.test(y, alternative = "less", mu = mean(y)),
       "summary" = summary(y))
}

bfp <- function(days = 10) {
  linRegPlot(lastDays(days), "fat_percent")
}

lean <- function(days = 10) {
  linRegPlot(lastDays(days), "lean_mass")
}

fat <- function(days = 10) {
  linRegPlot(lastDays(days), "fat_mass")
}

all <- function(days = 10) {
  list(bfp = bfp(days), lean = lean(days), fat = fat(days))
}