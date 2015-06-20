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

df <- select(storm_df, one_of(c("BGN_DATE", "EVTYPE", "FATALITIES","INJURIES",
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
# Avalance
df$EVTYPE <- gsub("([ a-zA-Z/]*)*Avalanche", "Avalanche", df$EVTYPE)
# Wind
df$EVTYPE <- gsub("W[i]*n[s|d][s]*", "Wind", df$EVTYPE)
df$EVTYPE <- gsub("Wind[ a-zA-Z/\\.]*", "Wind", df$EVTYPE)
# blizzard
df$EVTYPE <- gsub("Blizzard[-/a-zA-Z ]*", "Blizzard", df$EVTYPE)
df$EVTYPE <- gsub("[/a-zA-Z ]*Blizzard", "Blizzard", df$EVTYPE)
# Coastal Flooding
df$EVTYPE <- gsub("(Coastal|Cstl|Tidal) Flood(ing)*", "CFLD", df$EVTYPE)
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*Coastal Flood", "CFLD", df$EVTYPE)
df$EVTYPE <- gsub("Coastal Flood[/]*[a-zA-A /]+", "CFLD", df$EVTYPE)
# debris flow
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*slide", "Debris Flow", df$EVTYPE, ignore.case = T)
# dense fog 
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*Dense Fog", "Dense Fog", df$EVTYPE)
# dense smoke [NO CHANGE]

# # Drought 
# drou <- c("Excessive Heat/Drought", "Drought/Excessive Heat"
#           , "Heat Drought", "Heat Wave Drought", "Heat/Drought" 
#           , "Dry Conditions", "Excessively Dry", "Dry Pattern"
#           , "Dry Spell", "Dry Weather", "Dryness", "Abnormally Dry")
# #, "Dry")
# df$EVTYPE <- mgsub(drou, c("Drought"), df$EVTYPE)
# 
# # dust devil
# df$EVTYPE <- mgsub(c("Dust Devel", "Dust Devil Waterspout"), c("Dust Devil"), df$EVTYPE)
# 
# # dust storm 
# df$EVTYPE <- mgsub(c("Dust Storm/High Winds", "High Winds Dust Storm")
#                    ,c("Dust Storm"), df$EVTYPE)
# 

# 
# # excessive heat 
# xheat<- c("Heat Wave", "Heat Waves", "Heatburst"
#           , "Record Heat", "Record High", "Record High Temperature"
#           , "Record High Temperatures", "Record/Excessive Heat", "Record Warmth")
# #, "Heat")
# df$EVTYPE <- mgsub(xheat, c("Excessive Heat"), df$EVTYPE)

# Extreme Wind Chill 
df$EVTYPE <- gsub("(Bitter|Record|Extended|Extreme) Cold[-/a-zA-Z ]*", "ECWC", df$EVTYPE)
df$EVTYPE <- gsub("[/a-zA-Z ]*Extreme Cold", "ECWC", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z ]*(Bitter|Extreme) Wind[ ]*[C|c]?h[a-zA-Z ]+", "ECWC", df$EVTYPE)
# # Cold/Wind Chill
df$EVTYPE <- gsub("Cold/Wind Chill", "CWC", df$EVTYPE)
df$EVTYPE <- gsub("Un[a-zA-Z]+ Cold", "CWC", df$EVTYPE)
df$EVTYPE <- gsub("Cold (Temp[a-z]+[s]*|Wave|Weather)+", "CWC", df$EVTYPE)
df$EVTYPE <- gsub("Fog And Cold", "CWC", df$EVTYPE)  ## <=== doesnt work
df$EVTYPE <- gsub("Cold And[a-zA-Z /]+", "CWC", df$EVTYPE)
df$EVTYPE <- mgsub(c("High Wind/Cold","Cold/Wind"), c("CWC"), df$EVTYPE)
df$EVTYPE <- gsub("Cold","CWC", df$EVTYPE)
### Clean up the previous two
df$EVTYPE <- mgsub(c("CWC","ECWC")
    , c("Cold/Wind Chill","Extreme Cold/Wind Chill"), df$EVTYPE)

# # Winter Storm 
# wstrm <- c()
# df$EVTYPE <- mgsub(wstrm, c("Winter Storm"), df$EVTYPE)
# 
# # Winter Event 
# wevt<- c("Black Ice", "Accumulated Snowfall","Snow" 
# , "Blowing Snow" "Cold And Frost" , "Cold And Snow" 
# )
# df$EVTYPE <- mgsub(wevt, c("Winter Event"), df$EVTYPE)


# Flash flood
df$EVTYPE <- mgsub(c("Flash Flood/Flood","Flood/Flash Flood"), "Flood", df$EVTYPE)
df$EVTYPE <- gsub("Flash (Fl[o]+d)(s|ing)*", "FF", df$EVTYPE)
df$EVTYPE <- gsub("Flash Flood[ a-zA-Z/]+", "FF", df$EVTYPE)
df$EVTYPE <- gsub("[ a-zA-Z/]+Flash Flood", "FF", df$EVTYPE)
# Flood
df$EVTYPE <- gsub("Flood(s|in[g]*)*", "Flood", df$EVTYPE)
df$EVTYPE <- gsub("Flood/*[ a-zA-Z/(&]*", "Flood", df$EVTYPE)
df$EVTYPE <- gsub("Thunder[a-zA-Z/ ]+ Flood", "Flood", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z/ ]+Flood", "Flood", df$EVTYPE)
### Clean up the first two
df$EVTYPE <- mgsub(c("CFLD","FF")
                   ,c("Coastal Flood","Flash Flood"), df$EVTYPE)
# ^--- needs more clean up

# Freezing fog 
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*Freezing Fog", "Freezing Fog", df$EVTYPE)
# Funnel cloud
df$EVTYPE <- gsub("Funnel Cloud[a-zA-Z/ ]*", "Funnel Cloud", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z/ ]*Funnel Cloud", "Funnel Cloud", df$EVTYPE)
# Hail
df$EVTYPE <- gsub("Hail([ ]*[0-9]*|[ ]*/[a-zA-Z]*)", "Hail", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z /]*Hail", "Hail", df$EVTYPE)
# Heavy Snow
df$EVTYPE <- gsub("Heavy Snow[-/a-zA-Z ]*", "Heavy Snow", df$EVTYPE)
df$EVTYPE <- gsub("[/a-zA-Z ]*Heavy Snow", "Heavy Snow", df$EVTYPE)

# Hurricane/Typhoon
df$EVTYPE <- gsub("Hurricane[a-zA-Z/ ]*", "HTPH", df$EVTYPE)
df$EVTYPE <- gsub("Storm Surge[a-zA-Z/ ]*", "HTPH", df$EVTYPE)
df$EVTYPE <- gsub("HTPH", "Hurricane/Typhoon", df$EVTYPE)

# Lightning
df$EVTYPE <- gsub("Lightning[a-zA-Z/ \\.]*", "Lightning", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z/ ]*Lightning", "Lightning", df$EVTYPE)



# Thunderstorm wind
df$EVTYPE <- gsub("Tstm", "Thunderstorm", df$EVTYPE)
df$EVTYPE <- gsub("Thun[d]*e[e]*r[s]*t[or|ro]*m[s|w]*", "Thunderstorm", df$EVTYPE)
df$EVTYPE <- gsub("[(]*[a-zA-Z]*[0-9]+([ ]*Mph)*[)]*", " ", df$EVTYPE)
df$EVTYPE <- gsub("Thunderstorm Wind[\\. a-zA-Z/]*", "Thunderstorm Wind", df$EVTYPE)

# tornado
df$EVTYPE <- gsub("Tornado[es]*( F[0-9])*", "Tornado", df$EVTYPE)


df$EVTYPE <- Trim(df$EVTYPE)

t <- df %>% group_by(EVTYPE) %>% count(EVTYPE) %>% arrange(desc(n))
print(t)

otlist <- sort(unique(storm_df$EVTYPE))
tlist <- sort(unique(df$EVTYPE))

