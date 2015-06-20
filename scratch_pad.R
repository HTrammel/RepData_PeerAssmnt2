require(dplyr)
require(ggplot2)
require(lubridate)
require(stringr)


if(!file.exists("./data")) {dir.create("./data")}
dataFile <- "repdata-data-StormData.csv.bz2"
dataRDS <- "./data/storm.rds"

# extract data from file and store in RDS file
if (!file.exists(dataFile)) 
{
	download.file(
		"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
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

damageUnit <- function (cv) {
    for (i in cv) {
        if (i == "B") { return (1000000000) }
        else if (i == "M") { return (1000000) }
        else if (i == "K") { return (1000) }
        else return (1)
    }
}

df <- select(storm_df, one_of(c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES",
	"INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")))

# general conversions to make things better
df$EVTYPE <- str_to_title(as.character(df$EVTYPE))
df$BGN_DATE <- as.character.Date(df$BGN_DATE)
df$BGN_DATE <- dmy_hms(df$BGN_DATE)
df <- filter (df, year(df$BGN_DATE) >= 2004 & year(df$BGN_DATE) <= 2014 )
df <- filter(df, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)
df <- mutate (df, humEffect = FATALITIES + INJURIES)
df <- mutate (df, damage = (PROPDMG * damageUnit(PROPDMGEXP)) +
                  (CROPDMG * damageUnit(CROPDMGEXP)))

# remove leading and laging spaces
df$EVTYPE <- Trim(df$EVTYPE)
df$EVTYPE <- Trim(df$EVTYPE)

df$EVTYPE <- gsub("[a-zA-Z]+nado[es]*( F[0-9])*", "Tornado", df$EVTYPE)
df$EVTYPE <- gsub("Hail([ ]*[0-9]*|[ ]*/[a-zA-Z]*)", "Hail", df$EVTYPE)
df$EVTYPE <- gsub("Tstm", "Thunderstorm", df$EVTYPE, fixed = TRUE)
df$EVTYPE <- gsub("[(]*[a-zA-Z]*[0-9]+([ ]*Mph)*[)]*", " ", df$EVTYPE)
df$EVTYPE <- mgsub(c("Thunderstormw","Thunerstorm","Thunderstorms"
                     ,"Thundertorm","Thundeerstorm")
                   , c("Thunderstorm"), df$EVTYPE)
df$EVTYPE <- mgsub(c("Wins","Wnd","Winds"), "Wind", df$EVTYPE)

# try some plots
th_df <- df %>% group_by(EVTYPE) %>%
    summarise(avg_effect = mean(humEffect)) %>%
    top_n(10, avg_effect) %>% arrange(desc(avg_effect))
names(th_df) <- c("eventType", "avg_effect")

h_df <- df %>% group_by(year(df$BGN_DATE), EVTYPE) %>%
    summarise(avg_effect = mean(humEffect)) %>%
    top_n(10, avg_effect) %>% arrange(desc(avg_effect))
names(h_df) <- c("yr", "eventType", "avg_effect")

td_df <- df %>% group_by(EVTYPE) %>%
    summarise(avg_damage = mean(damage)) %>%
    top_n(10, avg_damage) %>% arrange(desc(avg_damage))
names(td_df) <- c("eventType", "avg_damage")

d_df <- df %>% group_by(year(df$BGN_DATE), EVTYPE) %>%
    summarise(avg_damage = mean(damage)) %>%
    top_n(10, avg_damage) %>% arrange(desc(avg_damage))
names(d_df) <- c("yr", "eventType", "avg_damage")

hp1 <- ggplot(th_df, aes(eventType, avg_effect, fill = eventType)) +
       geom_bar(stat = 'identity')
print(hp1)

hp2 <- ggplot(h_df, aes(yr, avg_effect)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~ eventType)
print(hp2)