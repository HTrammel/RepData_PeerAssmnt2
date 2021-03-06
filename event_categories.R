require(dplyr)
require(ggplot2)
require(lubridate)
require(stringr)


if(!file.exists("./data")) {dir.create("./data")}
dataFile <- "repdata-data-StormData.csv.bz2"
dataRDS <- "./data/storm.rds"

# extract data from file and store in RDS file
if (!file.exists(dataFile)) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                  dataFile, method = "curl", mode = "wb")
    storm_df <- read.csv(bzfile(dataFile))
    saveRDS(storm_df, dataRDS)
} else if (!file.exists(dataRDS)) {
    storm_df <- read.csv(bzfile(dataFile))
    saveRDS(storm_df, dataRDS)
}

# check to see if data frame loaded already
if (exists("storm_df") == FALSE) {
    storm_df <- readRDS(dataRDS)
}

eventCategory <- function (cv) {
    ln <- length(cv)
    for (i in cv[1:ln]) {
        if (grepl("wind",cv[i])) {
            return("Wind Event")
        } else {
            return(cv[i])
        }
    }
}

sdf <- data.frame(2,c("Orig","Cat"))
sevt <- storm_df$EVTYPE
sevt <- str_to_title(as.character(sevt))

sdf <- tbl_df(cbind(sdf, sevt))

#sdf <- data.frame(cbind(sdf, evtCat = eventCategory(sdf)))

