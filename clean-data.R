require(plyr)
require(dplyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(qdap)

if(!file.exists("./data")) {dir.create("./data")}
dataFile <- "repdata-data-StormData.csv.bz2"
dataRDS <- "./data/storm.rds"

# extract data from file and store in RDS file
if (!file.exists(dataFile)) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                  dataFile, method = "curl", mode = "wb")
    storm_df <- read.csv(bzfile(dataFile))
    saveRDS(st%IN%orm_df, dataRDS)
} else if (!file.exists(dataRDS)) {
    storm_df <- read.csv(bzfile(dataFile))
    saveRDS(storm_df, dataRDS)
}

# check to see if data frame loaded already
if (exists("storm_df") == FALSE) {
    storm_df <- readRDS(dataRDS)
}

df <- select(storm_df, one_of(c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES","INJURIES",
"PROPDMG", "PROPDMGEXP", "CROPDMG")))

# general conversions to make things better
df$EVTYPE <- str_to_title(as.character(df$EVTYPE))
df$BGN_DATE <- as.character.Date(df$BGN_DATE)
df$BGN_DATE <- dmy_hms(df$BGN_DATE)
df <- mutate(df, EVT_YR = year(df$BGN_DATE))

df <- filter(df, EVT_YR >= 1994 & EVT_YR <= 2014 )

df$EVTYPE <- Trim(df$EVTYPE)
df$EVTYPE <- multigsub("Tstm", "Thunderstorm", df$EVTYPE)
df$EVTYPE <- multigsub("Tunderstorm", "Thunderstorm", df$EVTYPE)
df$EVTYPE <- multigsub(c("Thunderstorm Wind [0-9][0-9]","Thunderstorm Winds","Thunderstorm Winds [0-9][0-9]"),c("Thunderstorm Wind"), df$EVTYPE)

t <- sort(unique(df$EVTYPE))

