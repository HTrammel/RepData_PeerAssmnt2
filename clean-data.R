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

df <- select(storm_df, one_of(c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES","INJURIES",
"PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")))

# general conversions to make things better
df$EVTYPE <- str_to_title(as.character(df$EVTYPE))
df$BGN_DATE <- as.character.Date(df$BGN_DATE)
df$BGN_DATE <- dmy_hms(df$BGN_DATE)
df <- mutate(df, EVT_YR = year(df$BGN_DATE))

df <- filter(df, EVT_YR >= 1994 & EVT_YR <= 2014 )

# remove leading and laging spaces
df$EVTYPE <- Trim(df$EVTYPE)

# wind
wnd <- c("Winds","Wnd","W Ind", "Wind.")
df$EVTYPE <- mgsub(wnd, c("Wind"), df$EVTYPE)

# avalance
df$EVTYPE <- mgsub("([^a-zA-Z]+|^|[/]+)Avalanche", "Avalanche", df$EVTYPE)

# coastal flood
cfld <- c("Coastal Flooding", "Coastal Flooding/Erosion"
, "Coastal Surge", "Coastal/Tidal Flood", "Coastalflood"
, "Beach Flood", "Cstl Flooding/Erosion", "Heavy Surf Coastal Flooding"
, "High Winds/Coastal Flood", "Tidal Flooding", "Erosion/Cstl Flood")
df$EVTYPE <- mgsub(cfld, c("Coastal Flood"), df$EVTYPE)

# Extreme Wind Chill 
xwch<- c("Blowing Snow & Extreme Wind Ch", "Blowing Snow/Extreme Wind Chil"
, "Extreme Cold", "Extreme Wind Chill", "Extreme Wind Chill/Blowing Sno"
, "Extreme Wind Chills", "Extreme Windchill", "Bitter Wind Chill"
, "Blowing Snow- Extreme Wind Chi", "Bitter Wind Chill Temperatures"
, "Extreme Windchill Temperatures", "Extreme/Record Cold", "Snow/ Bitter Cold")
df$EVTYPE <- mgsub(xwch, c("Extreme Cold/Wind Chill"), df$EVTYPE)

# Cold/Wind Chill
cwnd <- c("Unseasonable Cold", "Unseasonably Cold", "Unusually Cold"  
, "Cold And Frost" , "Cold And Snow" , "Cold And Wet Conditions"       
, "Cold Temperature", "Cold Temperatures", "Cold Wave", "Cold Weather" 
, "Cold Wind Chill Temperatures", "Cold/Winds", "High Wind/Low Wind Chill" 
, "High Winds And Wind Chill", "Snow- High Wind- Wind Chill")  
#, "Cold", "Wind Chill")
df$EVTYPE <- mgsub(cwnd, c("Cold/Wind Chill"), df$EVTYPE)

# blizzard
blzd <- c("Blizzard And Extreme Wind Chil", "Blizzard Weather"
, "Blizzard And Heavy Snow", "Blizzard/High Wind"
, "High Wind/Blizzard", "High Wind/Blizzard/Freezing Ra"
, "Heavy Snow/Blizzard", "Icestorm/Blizzard")
df$EVTYPE <- mgsub(blzd, c("Blizzard"), df$EVTYPE)


# flood
fld <- c("Breakup Flooding","Flood & Heavy Rain", "Flood/Rain/Wind" 
, "Flood/Rain/Winds", "Flood/Strong Wind", "Flooding"
, "Flooding/Heavy Rain", "Floods" , "Heavy Rain And Flood" 
, "Heavy Rain/Flooding", "Heavy Rain/Small Stream Urban"
, "Heavy Rain/Urban Flood" , "Heavy Rain; Urban Flood Winds;"
, "Heavy Rains/Flooding", "Ice Jam Flooding", "Ice Storm/Flash Flood"
, "Lakeshore Flood", "Local Flood", "Minor Flooding", "River And Stream Flood"
, "River Flood", "River Flooding" , "Rural Flood", "Small Stream And Urban Flood"
, "Small Stream And Urban Floodin", "Small Stream Flood", "Small Stream Flooding" 
, "Small Stream Urban Flood", "Small Stream/Urban Flood", "Sml Stream Fld"
, "Snowmelt Flooding", "Urban And Small Stream", "Urban And Small Stream Flood" 
, "Urban And Small Stream Floodin", "Urban Flood", "Urban Flooding" 
, "Urban Floods", "Urban Small", "Urban Small Stream Flood" 
, "Urban/Small Stream", "Urban/Small StreamFlood", "Urban/Small Stream Flood" 
, "Urban/Small Stream Flooding" , "Urban/Sml Stream Fld" 
, "Thunderstorm Winds/ Flood")
df$EVTYPE <- mgsub(fld, c("Flood"), df$EVTYPE)

# excessive heat 
# aaa<- c()
# df$EVTYPE <- mgsub(aaa, c(), df$EVTYPE)

# Winter Storm 
wstrm <- c()
df$EVTYPE <- mgsub(wstrm, c("Winter Storm"), df$EVTYPE)

# Winter Event 
wevt<- c("Black Ice", "Accumulated Snowfall" 
, "Blowing Snow" 
)
df$EVTYPE <- mgsub(wevt, c("Winter Event"), df$EVTYPE)

# clean up thunderstorm wind
tstm <- c("Tstm", "Thunderstormw", "Tunderstorm"
			, "Thunderestorm", "Thunderestorm", "Thundeerstorm"
			, "Thundertsorm", "Thunderstrom", "Thundertorm"
			, "Thundestorm",  "Thunerstorm",  "Thunderstorms")
df$EVTYPE <- mgsub(tstm, c("Thunderstorm"), df$EVTYPE)

#, "Downburst", "Downburst Winds"
ot <- sort(unique(storm_df$EVTYPE))
t <- sort(unique(df$EVTYPE))


