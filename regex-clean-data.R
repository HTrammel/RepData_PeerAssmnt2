require(plyr)
require(dplyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(qdap)

if(!file.exists("./data")) {dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
dataFile <- "repdata-data-StormData.csv.bz2"
dataRDS <- "./data/storm.rds"

# extract data from file and store in RDS file
if (!file.exists(dataFile)) {
    if (Sys.info()['sysname'] == "Windows") {
        # needed for my Windows PC
        download.file(fileURL, destfile=dataFile)
    } else {
        # needed for my Mac
        download.file(fileURL, destfile=dataFile, method = "curl", mode = "wb")
    }
    storm_df <- read.csv(bzfile(dataFile), stringsAsFactors = FALSE)
    saveRDS(storm_df, dataRDS)
} else if (!file.exists(dataRDS)) {
    storm_df <- read.csv(bzfile(dataFile), stringsAsFactors = FALSE)
    saveRDS(storm_df, dataRDS)
}

# check to see if data frame loaded already
if (exists("storm_df") == FALSE) {
    storm_df <- readRDS(dataRDS)
}

df <- select(storm_df, one_of(c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES","INJURIES",
"PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")))

# general conversions to make things better
df$EVTYPE <- str_to_title(as.character(df$EVTYPE))
df$BGN_DATE <- as.character.Date(df$BGN_DATE)
df$BGN_DATE <- dmy_hms(df$BGN_DATE)
df <- mutate(df, EVT_YR = year(df$BGN_DATE))

df <- filter(df, EVT_YR >= 1994 & EVT_YR <= 2014 )
df <- filter(df, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)

# remove leading and laging spaces
df$EVTYPE <- Trim(df$EVTYPE)

# hail
df$EVTYPE <- gsub("(Tstm|Thunderstorm|Thunderst[a-z]+)[ ]*(Wind|Winds|Wins)($|[ ]*[a-zA-Z0-9]+)", "Thunderstorm Wind", df$EVTYPE, fixed = TRUE)
#df$EVTYPE <- gsub("^Flash Flood", "Flash Flood", df$EVTYPE)

# df$EVTYPE <- gsub("Tidal|Beach Fl[a-z]+", c("Coastal Flood"), df$EVTYPE)



t <- sort(unique(df$EVTYPE))


