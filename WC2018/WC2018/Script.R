#install.packages("bit64")
#install.packages("xts")
#install.packages("ggplot2")
#install.packages("grid")
#install.packages("gridExtra")
#install.packages("data.table")
#install.packages("svglite")

require(bit64)
require(xts)
require(data.table)
require(ggplot2)
require(grid)
require(gridExtra)
require(svglite)


wctable <- read.csv(file = "../data/wc2018/wctable.csv", header = TRUE, row.names = NULL, encoding = "Ansi", sep = ";", dec = ".", quote = "\"", comment.char = "")
devices <- read.csv(file = "../data/device2name.csv", header = TRUE, row.names = NULL, encoding = "Ansi", sep = ",", dec = ".", quote = "\"", comment.char = "", colClasses = c("character", "character"))
events <- read.csv(file = "../data/wc2018/events.csv", header = TRUE, row.names = NULL, encoding = "Ansi", sep = ";", dec = ".", quote = "\"", comment.char = "")
events$date <- as.POSIXct(events$Columna2, "%d/%m/%Y %H:%M", tz = "Europe/Paris")

stats <- c()
statsdt <- c()

for (day in unique(substring(wctable$Date, 0, 10))) {
    daytmp <- paste(substring(day, 7, 10), substring(day, 4, 5), substring(day, 0, 2), sep = "-")
    ## title;country;systemid;device;date;plays;viewtime
    fileName <- paste("../data/wc2018/", daytmp, ".csv", sep = "")
    if (file.exists(fileName)) {
        st <- read.csv(file = fileName, header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ";", dec = ".", quote = "\"", comment.char = "", colClasses = c("character", "character", "character", "character", "character", "integer", "integer64"))
        st$date <- as.POSIXct(st$date, "%Y-%m-%d %H:%M", tz = "Europe/London")
        ##st$date <- st$uk
        attributes(st$date)$tzone = "Europe/Paris"

        stats[[daytmp]] <- st

        dt <- data.table(st)
        setkey(dt, "date")

        statsdt[[daytmp]] <- dt
    }
}

##http://www.moeding.net/archives/32-Metric-prefixes-for-ggplot2-scales.html
format_si <- function(...) {
    # Format a vector of numeric values according
    # to the International System of Units.
    # http://en.wikipedia.org/wiki/SI_prefix
    #
    # Based on code by Ben Tupper
    # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
    # Args:
    #   ...: Args passed to format()
    #
    # Returns:
    #   A function to format a vector of strings using
    #   SI prefix notation
    #

    function(x) {
        limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9, 1e-6, 1e-3, 1e0, 1e3,
                1e6, 1e9, 1e12, 1e15, 1e18,
                1e21, 1e24)
        prefix <- c("y", "z", "a", "f", "p",
                "n", "µ", "m", " ", "k",
                "M", "G", "T", "P", "E",
                "Z", "Y")

        # Vector with array indices according to position in intervals
        i <- findInterval(abs(x), limits)

        # Set prefix to " " for very small values < 1e-24
        i <- ifelse(i == 0, which(limits == 1e0), i)

        paste(format(round(x / limits[i], 1),
                 trim = TRUE, scientific = FALSE, ...),
          prefix[i])
    }
}

byCountry <- NULL
byDevice <- NULL
isFirst <- TRUE
matches <- c()
matchesName <- c()
matchesMax <- c()
matchesAvg <- c()
matchesSum <- c()
matchesMaxTime <- c()
matchesSumTime <- c()
matchesGroup <- c()
matchesDay <- c()
i <- 1
plots <- list()
for (i in 1:nrow(wctable)) {
    day <- as.character(wctable[i, 1])
    daytmp <- paste(substring(day, 7, 10), substring(day, 4, 5), substring(day, 0, 2), sep = "-")
    dt <- statsdt[[daytmp]]
    if (!is.null(dt)) {
        resultStr <- as.character(wctable[i, 4])
        starts <- as.POSIXct(as.character(wctable[i, 6]), "%d/%m/%Y %H:%M", tz = "Europe/Paris", usetz = TRUE)
        ends <- as.POSIXct(as.character(wctable[i, 7]), "%d/%m/%Y %H:%M", tz = "Europe/Paris", usetz = TRUE)

        startsD <- as.POSIXct(day, "%d/%m/%Y %H:%M", tz = "Europe/Paris", usetz = TRUE)
        endsD <- as.POSIXct(as.character(wctable[i, 10]), "%d/%m/%Y %H:%M", tz = "Europe/Paris", usetz = TRUE)
        endsFirst <- as.POSIXct(as.character(wctable[i, 11]), "%d/%m/%Y %H:%M", tz = "Europe/Paris", usetz = TRUE)
        startsSecond <- as.POSIXct(as.character(wctable[i, 12]), "%d/%m/%Y %H:%M", tz = "Europe/Paris", usetz = TRUE)

        Team1 <- as.character(wctable[i, 2])
        Team2 <- as.character(wctable[i, 3])

        group <- as.character(wctable[i, 8])
        name <- as.character(wctable[i, 9])
        nameFile <- paste(group, name, sep = "_")
        namePlot <- paste(day, "-", group, "-", name, " -> [", resultStr, "]", sep = " ")

        detailDataTmp <- dt[which((dt$title == 'FUTBOL' | dt$title == 'WC2018' | dt$title == 'SPORTS') & as.POSIXct(dt$date) >= startsD & as.POSIXct(dt$date) <= endsD),]
        detailTmp <- dt[which((dt$title == 'FUTBOL' | dt$title == 'WC2018' | dt$title == 'SPORTS') & as.POSIXct(dt$date) >= starts & as.POSIXct(dt$date) <= ends),]

        detailDevice <- detailTmp[, list(plays = sum(plays), viewtime = sum(viewtime / 3600.0), match = name, group = group, date = day), by = list(device)]
        detailCountry <- detailTmp[, list(plays = sum(plays), viewtime = sum(viewtime / 3600.0), match = name, group = group, date = day), by = list(country)]

        if (isFirst) {
            byCountry <- detailCountry
            byDevice <- detailDevice
        }
        else {
            byCountry <- rbind(byCountry, detailCountry)
            byDevice <- rbind(byDevice, detailDevice)
        }
        isFirst = FALSE

        detail <- detailTmp[, list(plays = sum(plays)), by = list(date)]

        ##detail <- detailTmp[, list(plays = sum(plays), title = title), by = list(date, title)]

        detailData <- detailDataTmp[, list(plays = sum(plays), viewtime = sum(viewtime / 3600.0)), by = list(date)]

        ##detail$group = ifelse(detail$date >= startsD & detail$date <= endsFirst, "first", ifelse(detail$date >= startsSecond & detail$date <= endsD, "second", "other"))

        matches[[name]] <- detail

        eventsMatch <- events[which(events$`Column1.matches.home` == Team1 & events$`Column1.matches.away` == Team2 & regexpr("goal", events$Column1.matches.actions.eventType) >= 0), c("date", "Column1.matches.actions.team", "Column1.matches.actions.eventType", "Column1.matches.actions.eventTime", "Column1.matches.homeColour", "Column1.matches.awayColour")]

        matchesName[[i]] <- name
        matchesMax[[i]] <- max(detailData$plays)
        matchesSum[[i]] <- sum(detailData$plays)
        matchesAvg[[i]] <- mean(detailData$plays)
        matchesMaxTime[[i]] <- max(detailData$viewtime)
        matchesSumTime[[i]] <- sum(detailData$viewtime)
        matchesGroup[[i]] <- group
        matchesDay[[i]] <- day
        ##https://stackoverflow.com/questions/8317584/r-ggplot-time-series-with-events
        ##https://stackoverflow.com/questions/37713351/formatting-ggplot2-axis-labels-with-commas-and-k-mm-if-i-already-have-a-y-sc
        gr <- ggplot(detail, aes(date, plays)) +
                    geom_line() + xlab("") + ##geom_point(aes(colour = group), size = 0.5) +
        ylab("Concurrent plays") + ggtitle(namePlot) +
                scale_y_continuous(labels = format_si()) +
        geom_segment(aes(x = startsD, xend = endsFirst, y = 100, yend = 100), size = 2, colour = "gray") +
        geom_segment(aes(x = startsSecond, xend = endsD, y = 100, yend = 100), size = 2, colour = "gray")
        if (nrow(eventsMatch) > 0) {
            #gr <- gr + geom_point(data = eventsMatch, mapping = aes(x = date, y = 10, show.legend = FALSE, colour = ifelse(eventsMatch$Column1.matches.actions.team == "home", eventsMatch$Column1.matches.homeColour, eventsMatch$Column1.matches.awayColour)), size = 3)
            gr <- gr + scale_colour_manual(name = "", values = c("home" = as.character(unique(eventsMatch$Column1.matches.homeColour)), "away" = as.character(unique(eventsMatch$Column1.matches.awayColour))))
            gr <- gr + geom_point(data = eventsMatch, mapping = aes(x = date, y = 10, colour = eventsMatch$Column1.matches.actions.team), show.legend = FALSE, size = 3)
        }
        ggsave(file = paste("../results/", nameFile, ".svg", sep = ""), plot = gr, width = 10, height = 8)

        plots[[i]] <- gr
        i <- i + 1
    }
}
result <- data.frame(matchesName, matchesMax, matchesAvg, matchesSum, matchesMaxTime, matchesSumTime, matchesGroup, matchesDay)
write.table(result, file = "../results/playsPerMatch.csv", row.names = FALSE, sep = ";")
write.table(byCountry, file = "../results/byCountry.csv", row.names = FALSE, sep = ";")
write.table(merge(byDevice, devices, by = "device"), file = "../results/byDevice.csv", row.names = FALSE, sep = ";")
##multiplot(plotlist = plots, cols = 3)

evolution <- byCountry[which(byCountry$country == 'ES' | byCountry$country == 'SN' | byCountry$country == 'JP' | byCountry$country == 'AU' | byCountry$country == 'AR'), list(plays = sum(plays), match = max(match), country=country), by = list(date, country)]
evolution$date <- as.POSIXct(evolution$date, "%d/%m/%Y %H:%M", tz = "CET")
ggplot(evolution, aes(date, plays, group = country, fill = country)) +
    geom_line(aes(colour = country), position = "stack") + xlab("") + ##geom_point(aes(colour = group), size = 0.5) +
    ylab("Concurrent plays") + ggtitle(namePlot) + geom_area() +
    scale_y_continuous(labels = format_si())

detailX <- detailTmp[, list(plays = sum(plays), title = title), by = list(date, title)]
##https://stackoverflow.com/questions/12323060/geom-area-plot-with-areas-and-outlines-ggplot
ggplot(detailX, aes(date, plays, group = title, fill = title)) +
    geom_line(aes(colour = title), position = "stack") + xlab("") + ##geom_point(aes(colour = group), size = 0.5) +
    ylab("Concurrent plays") + ggtitle(namePlot) + geom_area() + 
    scale_y_continuous(labels = format_si()) +
    geom_segment(aes(x = startsD, xend = endsFirst, y = 100, yend = 100), size = 2, colour = "gray") +
    geom_segment(aes(x = startsSecond, xend = endsD, y = 100, yend = 100), size = 2, colour = "gray")

#ggplot(evolution[which(evolution$country == "ES")], aes(date, plays)) + geom_line() + xlab("") + ylab("Concurrent plays") + ggtitle(namePlot) + scale_y_continuous(labels = format_si())
#ggplot(evolution[which(evolution$country == "JP")], aes(date, plays)) + geom_line() + xlab("") + ylab("Concurrent plays") + ggtitle(namePlot) + scale_y_continuous(labels = format_si())
#ggplot(evolution[which(evolution$country == "AU")], aes(date, plays)) + geom_line() + xlab("") + ylab("Concurrent plays") + ggtitle(namePlot) + scale_y_continuous(labels = format_si())
#ggplot(evolution[which(evolution$country == "AR")], aes(date, plays)) + geom_line() + xlab("") + ylab("Concurrent plays") + ggtitle(namePlot) + scale_y_continuous(labels = format_si())
#ggplot(evolution[which(evolution$country == "SN")], aes(date, plays)) + geom_line() + xlab("") + ylab("Concurrent plays") + ggtitle(namePlot) + scale_y_continuous(labels = format_si())
##https://stackoverflow.com/questions/15363035/ggplot2-how-to-specify-multiple-fill-colors-for-points-that-are-connected-by-li


ggplot(statsdt[["2018-07-15"]][, list(plays = sum(plays), title = title), by = list(date, title)], aes(date, plays, group = title, fill = title)) +
    geom_line(aes(colour = title), position = "stack") + xlab("") + ##geom_point(aes(colour = group), size = 0.5) +
    ylab("Concurrent plays") + ggtitle("2018-07-15") + geom_area() +
    scale_y_continuous(labels = format_si())


tail(result[order(result$matchesSum),])

fileName <- paste("../data/", daytmp, ".csv", sep = "")
if (file.exists(fileName)) {
    st <- read.csv(file = fileName, header = TRUE, row.names = NULL, encoding = "UTF-8", sep = ";", dec = ".", quote = "\"", comment.char = "", colClasses = c("character", "character", "character", "character", "character", "integer", "integer64"))
    st$date <- as.POSIXct(st$date, "%Y-%m-%d %H:%M", tz = "Europe/London")
    ##st$date <- st$uk
    attributes(st$date)$tzone = "Europe/Paris"

    stats[[daytmp]] <- st

    dt <- data.table(st)
    setkey(dt, "date")

    statsdt[[daytmp]] <- dt
}
daytmp <- "2018-07-03"
ggplot(statsdt[[daytmp]][, list(plays = sum(plays), title = title), by = list(date, title)], aes(date, plays, group = title, fill = title)) +
    geom_line(aes(colour = title), position = "stack") + xlab("") + ##geom_point(aes(colour = group), size = 0.5) +
    ylab("Concurrent plays") + ggtitle(daytmp) + geom_area() +
    scale_y_continuous(labels = format_si())


