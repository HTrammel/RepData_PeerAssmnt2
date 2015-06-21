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
        # ne landeded for my Mac
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

# select only the needed columns
df <- select(storm_df, one_of(c("BGN_DATE", "EVTYPE", "FATALITIES","INJURIES",
                                "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")))
# function to convert unit abbreviations to numeric values 
damageUnit <- function (cv) {
    for (i in cv) {
        if (i == "B") { return (1000000000) }
        else if (i == "M") { return (1000000) }
        else if (i == "K") { return (1000) }
        else return (1)
    }
}
# general conversions to make things better
df$EVTYPE <- str_to_title(as.character(df$EVTYPE))
df$BGN_DATE <- as.character.Date(df$BGN_DATE)
df$BGN_DATE <- dmy_hms(df$BGN_DATE)
# filter for only past twenty years data
df <- filter (df, year(df$BGN_DATE) >= 1994 & year(df$BGN_DATE) <= 2014 )
# filter for only rows with human and economic impact
df <- filter(df, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)
# add new column to hold calculated values
df <- mutate (df, humEffect = FATALITIES + INJURIES)
df <- mutate (df, damage = (PROPDMG * damageUnit(PROPDMGEXP)) +
                  (CROPDMG * damageUnit(CROPDMGEXP)))

# remove leading and laging spaces
df$EVTYPE <- Trim(df$EVTYPE)

# MISCELLANOUS ##########################################
# Debris Flow
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*slide", "Debris Flow", df$EVTYPE, ignore.case = T)
# Dense Fog 
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*Dense Fog", "Dense Fog", df$EVTYPE)
# Dense Smoke [NO CHANGE]

# WIND AND RAIN RELATED ######################################
# Wind
df$EVTYPE <- gsub("W[i]*n[s|d][s]*", "Wind", df$EVTYPE)
df$EVTYPE <- gsub("Wind[ a-zA-Z/0-9\\(\\)\\.]*", "Wind", df$EVTYPE)
# High Wind
df$EVTYPE <- mgsub(c("Gradient Wind","Extreme Wind"
                    ,"Storm Force Wind", "Strong Wind"
                    ,"Gusty Wind")
                  ,c("High Wind"), df$EVTYPE)
# Thunderstorm Wind
df$EVTYPE <- gsub("Tstm", "Thunderstorm", df$EVTYPE)
df$EVTYPE <- gsub("T[h]*u[n]*[d]*e[e]*r[s]*t[or|ro]*m[s|w]*", "Thunderstorm", df$EVTYPE)
df$EVTYPE <- gsub("[(]*[a-zA-Z]*[0-9]+([ ]*Mph)*[)]*", " ", df$EVTYPE)
df$EVTYPE <- gsub("Thunderstorm Wind[\\. a-zA-Z/]*", "Thunderstorm Wind", df$EVTYPE)
# Hail
df$EVTYPE <- gsub("Hail([ ]*[0-9]*|[ ]*/[a-zA-Z]*)", "Hail", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z /]*Hail", "Hail", df$EVTYPE)
# Lightning
df$EVTYPE <- gsub("Lightning[a-zA-Z/ \\.]*", "Lightning", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z/ ]*Lightning", "Lightning", df$EVTYPE)
# Heavy Rain
df$EVTYPE <- gsub("[a-zA-Z ]*Rain/Winter Weather", "WW", df$EVTYPE)
df$EVTYPE <- gsub("Heavy Rain[/a-zA-Z ]*)*", "Heavy Rain", df$EVTYPE)
df$EVTYPE <- mgsub(c("Excessive Rainfall", "Unseasonal Rain")
                 , c("Heavy Rain"), df$EVTYPE)

# HEAT RELATED ################################################
# Drought [NO CHANGE]
df$EVTYPE <- gsub("Drought[-/a-zA-Z ]*", "Drought", df$EVTYPE)
df$EVTYPE <- gsub("[/a-zA-Z ]*Drought", "Drought", df$EVTYPE)
# Dust Devil
df$EVTYPE <- gsub("Dust Devil Waterspout","Dust Devil", df$EVTYPE)
# Dust Storm [NO CHANGE]
# Excessive Heat 
df$EVTYPE <- gsub("[ a-zA-Z/]* Heat", "Excessive Heat", df$EVTYPE)
# Heat
df$EVTYPE <- gsub("Heat [a-zA-Z/ ]+", "Heat", df$EVTYPE)

# FLOOD RELATED ###############################################
# Coastal Flooding
df$EVTYPE <- gsub("(Coastal|Cstl|Tidal) Flood(ing)*", "CFLD", df$EVTYPE)
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*Coastal Flood", "CFLD", df$EVTYPE)
df$EVTYPE <- gsub("Coastal Flood[/]*[a-zA-A /]+", "CFLD", df$EVTYPE)
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
#---------------
df$EVTYPE <- mgsub(c("CFLD","FF")
                   ,c("Coastal Flood","Flash Flood"), df$EVTYPE)
###############################################################

# #### WINTER #############################################
# Avalance
df$EVTYPE <- gsub("([ a-zA-Z/]*)*Avalanche", "Avalanche", df$EVTYPE)
# Blizzard
df$EVTYPE <- gsub("Blizzard[-/a-zA-Z ]*", "Blizzard", df$EVTYPE)
df$EVTYPE <- gsub("[/a-zA-Z ]*Blizzard", "Blizzard", df$EVTYPE)
# Cold/Wind Chill
df$EVTYPE <- mgsub(c("Fog And Cold","Cold/Wind Chill"
                     ,"High Wind/Cold","Cold/Wind")
                   , c("CWC"), df$EVTYPE)
df$EVTYPE <- gsub("Un[a-zA-Z]+ Cold", "CWC", df$EVTYPE)
df$EVTYPE <- gsub("Cold (Temp[a-z]+[s]*|Wave|Weather)+", "CWC", df$EVTYPE)
df$EVTYPE <- gsub("Cold And[a-zA-Z /]+", "CWC", df$EVTYPE)
df$EVTYPE <- gsub("Cold","CWC", df$EVTYPE)
# Extreme Wind Chill <================
df$EVTYPE <- gsub("Ext[a-z]* Cold/Wind Chill", "ECWC", df$EVTYPE)
df$EVTYPE <- gsub("Record Cold/Wind Chill", "ECWC", df$EVTYPE)
df$EVTYPE <- gsub("[/a-zA-Z ]*Extreme Cold", "ECWC", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z ]*(Bitter|Extreme) Wind[ ]*[C|c]?h[a-zA-Z ]+", "ECWC", df$EVTYPE)
#---------------
df$EVTYPE <- mgsub(c("CWC","ECWC")
                   , c("Cold/Wind Chill","Extreme Cold/Wind Chill"), df$EVTYPE)
# Freezing fog 
df$EVTYPE <- gsub("[ a-zA-Z/]*[s]*[/]*Freezing Fog", "FZFG", df$EVTYPE)
# Freeze
df$EVTYPE <- gsub("Frost/Freeze","FRZ", df$EVTYPE)
df$EVTYPE <- gsub("(Agricultural|Damaging)*[ ]*Freeze","FRZ", df$EVTYPE)
# Heavy Snow
df$EVTYPE <- gsub("Heavy Snow[-/a-zA-Z ]*", "HSNO", df$EVTYPE)
df$EVTYPE <- gsub("Snow/Heavy Snow", "HSNO", df$EVTYPE)
# Winter Storm
df$EVTYPE <- gsub("Snow[/]*[ ]*Freezing Rain", "WSTRM", df$EVTYPE)
df$EVTYPE <- gsub("Freezing Rain[/]*[ ]*Snow", "WSTRM", df$EVTYPE)
df$EVTYPE <- gsub("Mixed [ a-zA-Z]+", "WSTRM", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z/ ]*Mix", "WSTRM", df$EVTYPE)
df$EVTYPE <- mgsub(c("Snow/Sleet/Freezing Rain"
                    ,"Snow And Ice","Snow/Ice Storm"
                    ,"Falling Snow/Ice","Snow/Sleet"
                    ,"Snow/ Bitter Cold/Wind Chill"
                    ,"([ a-zA-Z]*[ ]*)*Rain/Snow","Snow/ Ice")
                    , c("WSTRM"), df$EVTYPE)
# Winter Weather
df$EVTYPE <- gsub("Light Freezing Rain","WW", df$EVTYPE)
df$EVTYPE <- gsub("Freezing [a-zA-Z/ ]","WW", df$EVTYPE)
df$EVTYPE <- gsub("Snow[/a-zA-Z ]*","WW", df$EVTYPE)
#-----------------
df$EVTYPE <- gsub("FRZ", "Frost/Freeze", df$EVTYPE)
df$EVTYPE <- gsub("FZFG", "Freezing Fog", df$EVTYPE)
df$EVTYPE <- gsub("WW", "Winter Weather", df$EVTYPE)
df$EVTYPE <- gsub("HSNO", "Heavy Snow", df$EVTYPE)
df$EVTYPE <- gsub("WSTRM", "Winter Storm", df$EVTYPE)
###############################################################

# HERE WE GO ROUND IN CIRCLES
# Hurricane/Typhoon
df$EVTYPE <- gsub("Hurricane[a-zA-Z/ ]*", "HTPH", df$EVTYPE)
df$EVTYPE <- gsub("Storm Surge[a-zA-Z/ ]*", "HTPH", df$EVTYPE)
df$EVTYPE <- gsub("HTPH", "Hurricane/Typhoon", df$EVTYPE)
# Funnel cloud
df$EVTYPE <- gsub("Funnel Cloud[a-zA-Z/ ]*", "Funnel Cloud", df$EVTYPE)
df$EVTYPE <- gsub("[a-zA-Z/ ]*Funnel Cloud", "Funnel Cloud", df$EVTYPE)
# Tornado
df$EVTYPE <- gsub("Tornado[es]*( F[0-9])*", "Tornado", df$EVTYPE)
# Tropical Storm
df$EVTYPE <- gsub("Tropical Storm[ a-zA-Z]*", "Tropical Storm", df$EVTYPE)

df$EVTYPE <- Trim(df$EVTYPE)
ot <- storm_df %>% group_by(EVTYPE) %>% count(EVTYPE) %>% arrange(desc(n))
print(ot)
print("-----------------")
t <- df %>% group_by(EVTYPE) %>% count(EVTYPE) %>% arrange(desc(n))
print(t)

otlist <- sort(unique(storm_df$EVTYPE))
tlist <- sort(unique(df$EVTYPE))

