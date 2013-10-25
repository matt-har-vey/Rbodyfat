library('DBI')
library('RSQLite')
library('lubridate')
library('stringr')
library('xts')

lastDays <- function(days, dfFitbit = fitbit, skip = 0) {
  dfFitbit[dfFitbit$time >= now() - ddays(days + skip) & dfFitbit$time <= now() - ddays(skip),]
}

pacificDate <- function(d) {
  as.Date(
    floor_date(
      with_tz(d, "America/Los_Angeles"), "day"))
}

dayMeans <- function(dfFitbit = fitbit) {
  aggregate(dfFitbit[,c("weight", "fat_percent", "lean_mass", "fat_mass")],
            by = list(date = pacificDate(dfFitbit$time)),
            mean)
}

prettyHist <- function(x, ...) {
  h <- hist(x, ylim = c(0,1), prob = TRUE, main = '', ...)
  lines(density(x))
  rug(x)
  h
}

maDiff <- function(x) {
 filterCoeff <- 1/6*c(0,1,2,3,0,0,0,0,0)-1/8*c(0,1,1,1,1,1,1,1,1)
 xFirst <- x[1]
 xLast <- x[length(x)]
 padLength <- 20
 padded <- c(rep(xFirst, padLength), x, rep(xLast, padLength))
 filter(padded, filterCoeff, method = "conv", sides = 2)[(padLength + 1):(padLength + length(x))]
 
}



readData <- function() {
  sqliteSelect <- function(db, query) {
    sqliteDbd <- dbDriver('SQLite')
    con <- dbConnect(sqliteDbd, dbname = db)
    res <- dbGetQuery(con, query)
    dbDisconnect(con)
    res
  }
  
  weightsFile <- 'weights.sqlite3'
  
  fitbitDataFrame <- function(user = 1) {
    d <- transform(sqliteSelect(weightsFile,
                                paste("select time, weight, fat_percent from weights where user_id=", user, sep = '')),
                   time = as.POSIXct(time, tz="UTC"))
    d$lean_mass <- (1 - d$fat_percent / 100) * d$weight
    d$fat_mass <- d$weight - d$lean_mass
    d
  }
  
  viewsDataFrame <- function(user = 1) {
    transform(sqliteSelect(weightsFile,
                           paste("select viewed_at, ifnull(aggregation, 'none') as aggregation from views where user_id=", user, sep = '')),
              viewed_at = as.POSIXct(viewed_at, tz="UTC"))
  }
  
  mfpDataFrame <- function() {
    transform(sqliteSelect(weightsFile, "select * from food_intakes order by date"),
              date = as.Date(date))
  }
    
  fitbit <<- fitbitDataFrame()
  food <<- mfpDataFrame()
  fitbitAndFood <<- merge(food, dayMeans(fitbit))
  views <<- viewsDataFrame()
}




fitbitDiffs <- function(df, lag = 3) {
  firstFrame <- data.frame(df[1:(dim(df)[1] - lag),1])
  names(firstFrame) <- c(names(df)[1])
  diffFrame <- data.frame(diff(as.matrix(df[,c("weight", "fat_percent", "fat_mass", "lean_mass")]), lag = lag))
  names(diffFrame) <- sapply(names(diffFrame), function (n) { paste("diff_", n, sep = '') })
  cbind(firstFrame, diffFrame)
}
  


allPlots <- function(dfFitbit = fitbit) {
  floorTimes <- function(times, days) {
    length <- days * 3600 * 24
    as.Date(as.POSIXct(length * (as.double(times) %/% length), origin = "1970-01-01"))
  }
  
  fatBox <- function(dfFitbit = dfFitbit, days = 3) {
    with(dfFitbit, boxplot(fat_percent ~ floorTimes(time, days)))
    title('Body Fat %')
  }
  
  leanBox <- function(dfFitbit = dfFitbit, days = 3) {
    with(dfFitbit, boxplot(lean_mass ~ floorTimes(time, days)))
    title('Lean Mass')
  }
  
	split.screen(c(1,2))
	screen(1)
  fatBox(dfFitbit)
	screen(2)
  leanBox(dfFitbit)
  close.screen(all = TRUE)
}



plotDayOfWeek <- function(dfFitbit = fitbit, dependent = "fat_percent") {
  with(dfFitbit, boxplot(formula(sprintf("%s ~ wday(time)", dependent)),
                     ylab = 'Body Fat %', main = 'Body Composition by Day of Week', xaxt="n"))
  axis(1, at = seq(1,7),
       labels = c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat"))
}

ma <- function(ts, n = 5) { filter(ts, rep(1/n, n), sides = 1) }

plotMovingAverage <- function(dfFitbit = dayMeans(fitbit), n = 5) { plot(na.omit(ma(dfFitbit$fat_percent, n))) }

toPerWeek <- function(model) {
  model$coefficients["time"] * 3600 * 24 * 7
}

poundsPerWeek <- function(dfFitbit = fitbit) {
	toPerWeek(with(dfFitbit, lm(weight ~ time)))
}

meanNetCals <- function(dfFitbit = fitbit) {
  s <- pacificDate(min(dfFitbit$time))
  e <- pacificDate(max(dfFitbit$time))
  mean(food$net_calories[food$date >= s & food$date <= e])
}

targetCals <- function(dfFitbit = fitbit, targetPounds = 0) {
  meanNetCals(dfFitbit) - 500 * poundsPerWeek(dfFitbit) + 500 * targetPounds
}

percentT <- function(dfFitbit, m = 8) {
  t.test(dfFitbit$fat_percent, alternative = "less", mu = m)
}

pVals <- function(lags = seq(2,12)) {
  unlist(lapply(lags, goalP))
}

pPlot <- function(lags = seq(2,12)) {
  plot(lags, pVals(lags), xlab = '', ylab = '', ylim = c(0,1))
  abline(0.05, 0, lty = "dashed")
}

goalT <- function(lag = 7) {
  futureGoalT(lag = lag)
}

futureGoalT <- function(..., lag = 7) {
  px <- c(...)
  x <- c(tail(dayMeans()$fat_percent, lag - length(px)), px)
  list(x = x, t.test = t.test(x, alt = "less", mu = 8))
}

goalP <- function(lag = 7) {
  goalT(lag)$t.test$p.value
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
  hist <- prettyHist(y, xlab = '', ylab = '')
  
  close.screen(all = TRUE)
  
  list("hist" = hist,
       "lm" = mdl,
       "t.test" = t.test(y, alternative = "less", mu = mean(y)),
       "summary" = summary(y))
}

lrpPercent <- function(days = 10) {
  linRegPlot(lastDays(days), "fat_percent")
}

lrpLean <- function(days = 10) {
  linRegPlot(lastDays(days), "lean_mass")
}

lrpFat <- function(days = 10) {
  linRegPlot(lastDays(days), "fat_mass")
}

lrpAll <- function(days = 10) {
  list(percent = lrpPercent(days), lean = lrpLean(days), fat = lrpFat(days))
}
