bodyfat <- read.csv('weights.csv', colClasses = c('POSIXct', rep(NA,4)))

toIntervals <- function(times, days) {
	length <- days * 3600 * 24
	as.Date(as.POSIXct(length * (as.double(times) %/% length), origin = "1970-01-01"))
}

fatBox <- function(bf, days = 3) {
	with(bf, boxplot(fat_percent ~ toIntervals(time, days)))
	title('Body Fat %')
}

leanBox <- function(bf, days = 3) {
	with(bf, boxplot(lean_mass ~ toIntervals(time, days)))
	title('Lean Mass')
}

allPlots <- function(bf) {
  par(mfrow = c(1,2))
  fatBox(bf)
  leanBox(bf)
}
