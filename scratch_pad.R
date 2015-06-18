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

sdf <- select(storm_df, one_of(c("BGN_DATE","EVTYPE","FATALITIES","INJURIES")))

# general conversions to make things better
sdf$EVTYPE <- str_to_title(as.character(sdf$EVTYPE))
sdf$BGN_DATE <- as.character.Date(sdf$BGN_DATE)
sdf$BGN_DATE <- dmy_hms(sdf$BGN_DATE)


evt_cnt <- sdf %>%
    group_by(EVTYPE) %>%
    summarise(cnt = n()) %>%
    arrange(desc(cnt))

