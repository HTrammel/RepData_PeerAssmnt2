require(dplyr)
require(ggplot2)
require(lubridate)


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

storm_df$BGN_DATE <- dmy(as.character(storm_df$BGN_DATE))


s_df <- storm_df %>% group_by(year(BGN_DATE), EVTYPE ) %>% summarise(mean(FATALITIES))
