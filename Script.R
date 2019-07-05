stats <- read.csv(file = "wc2018stats.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ";", dec = ".", quote = "\"", comment.char = "")

head(stats)

wctable <- read.csv(file = "wctable.csv", header = TRUE, row.names = NULL, encoding = "Ansi", sep = ";", dec = ".", quote = "\"", comment.char = "")

countryflags <- read.csv(file = "flags.csv", header = TRUE, row.names = NULL, encoding = "Ansi", sep = ";", dec = ".", quote = "\"", comment.char = "")

##plot(stats$date, stats$plays)
require(data.table)
dt <- data.table(stats)
setkey(dt, "date")

install.packages("ggplot2")
library(ggplot2)

##ggplot(stats, aes(date, plays)) + geom_line() + scale_x_date(format = "%b-%Y") + xlab("") + ylab("Plays per minute")

##install.packages("PerformanceAnalytics")

##mm <- stats[which(as.POSIXlt(stats$date) >= as.POSIXlt('2018-06-16 11:45') & as.POSIXlt(stats$date) <= as.POSIXlt('2018-06-16 14:15')),]

match1 <- dt[which(as.POSIXlt(dt$date) >= as.POSIXlt('2018-06-16 11:45') & as.POSIXlt(dt$date) <= as.POSIXlt('2018-06-16 14:15')),]


stats16 <- read.csv(file = "wc2018/2018-06-16.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ";", dec = ".", quote = "\"", comment.char = "")

require(data.table)
dt16 <- data.table(stats16)
setkey(dt16, "date")


match1 <- stats16[which(as.POSIXlt(stats16$date) >= as.POSIXlt('2018-06-16 11:45') & as.POSIXlt(stats16$date) <= as.POSIXlt('2018-06-16 14:15')),]
match1 <- dt16[which(as.POSIXlt(dt16$date) >= as.POSIXlt('2018-06-16 11:45') & as.POSIXlt(dt16$date) <= as.POSIXlt('2018-06-16 14:15')),]



install.packages("bit64")
require(bit64)
require(xts)
require(data.table)

stats16 <- read.csv(file = "wc2018/2018-06-16.csv", header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ";", dec = ".", quote = "\"", comment.char = "", colClasses = c("character", "character", "character", "integer", "character", "character", "integer64"))
stats16$date <- as.POSIXct(stats16$date, "%Y-%m-%d %H:%M", tz = "UTC")
dt16 <- data.table(stats16)
setkey(dt16, "date")


match1 <- dt16[which(as.POSIXlt(dt16$date) >= as.POSIXct('2018-06-16 11:45', "%Y-%m-%d %H:%M", tz = "CET") & as.POSIXlt(dt16$date) <= as.POSIXlt('2018-06-16 14:15', "%Y-%m-%d %H:%M", tz = "CET")),]



ggplot(match1, aes(date, plays)) + geom_line() + scale_x_date(format = "%b-%Y") + xlab("") + ylab("Plays per minute")




xts(stats16[,c("viewtime")], order.by = as.POSIXct(stats16$Date))

viewtime16 <- stats16[, c("viewtime", "country", "title")]
plays16 <- stats16[, c("plays", "country", "title")]

kk <- xts(stats16[, c("plays")], order.by = as.POSIXct(stats16$date))






