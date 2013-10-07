library('DBI')
library('rjson')
library('RSQLite')
library('lubridate')

sqliteDbd <- dbDriver('SQLite')

csvData <- function() {
  read.csv('weights.csv', colClasses = c('POSIXct', rep(NA,4)))
}

sqlData <- function(user = 1) {
  con <- dbConnect(sqliteDbd, dbname = 'weights.sqlite3')
  d <- transform(dbGetQuery(con, paste("select time, weight, fat_percent from weights where user_id=", user, sep = '')),
                 time = as.POSIXct(time))
	d$lean_mass <- (1 - d$fat_percent / 100) * d$weight
	d$fat_mass <- d$weight - d$lean_mass
	dbDisconnect(con)
	d
}

bodyfat <- sqlData()
recent <- bodyfat[bodyfat$time > as.POSIXct('2013-09-10'),]

lastDays <- function(days, bf = bodyfat) {
  bf[bf$time > (Sys.time() - 3600 * days * 24),]
}

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

dayMean <- function(bf = bodyfat, d = 1) {
	aggregate(bf, by = list(day=toIntervals(bf$time, days = d)), FUN=mean)[c(1,3,4,5,6)]
}

bodyfatDaily <- dayMean()

ma <- function(ts, n = 5) { filter(ts, rep(1/n, n), sides = 1) }

plotMovingAverage <- function(bf = bodyfatDaily, n = 5) { plot(na.omit(ma(bf$fat_percent, n))) }

readCals <- function() {
  con <- file('cals.json')
  j <- fromJSON(readLines(con, warn = FALSE))
  close(con)
  cals <- data.frame(date = unlist(lapply(j$data, function (d) { d$date })), cals = unlist(lapply(j$data, function (d) { d$total })))
  cals$date <- as.POSIXct(cals$date, format = '%m/%d')
	cals
}

percentT <- function(bf, m = 8) {
  t.test(bf$fat_percent, alternative = "less", mu = m)
}

toPerWeek <- function(model) {
  model$coefficients["time"] * 3600 * 24 * 7
}

poundsPerWeek <- function(bf = bodyfat) {
	model <- with(bf, lm(weight ~ time))
	toPerWeek(model)
}

targetCals <- function(bf = bodyfat, targetPounds = 0) {
  s <- min(bf$time)
  e <- max(bf$time)
	cals <- readCals()
  cals <- cals[cals$cals > 0 & cals$date >= s & cals$date <= e,]
  mean(cals$cals) - 500 * poundsPerWeek(bf) / 7 + 500 * targetPounds
}

pVals <- function(lags = seq(1,7)) {
  unlist(lapply(lags, function(l) { percentT(lastDays(l))$p.value }))
}

pPlot <- function(lags = seq(1,7)) {
  x <- pVals(lags)
  plot(x, xlab = '', ylab = '')
  abline(0.05, 0, lty = "dashed")
}

dayMeans <- function(lag = 7) {
  aggregate(fat_percent ~ yday(time), lastDays(lag), mean)$fat_percent
}

goalT <- function(lag = 7) {
  t.test(dayMeans(lag), mu = 8, alternative = "less")
}

goalP <- function(lag = 7) {
  goalT(lag)$p.value
}