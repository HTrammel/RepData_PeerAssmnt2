if(!file.exists("./data")) {dir.create("./data")}
dataFile <- "repdata-data-StormData.csv.bz2"
dataRDS <- "./data/storm.rds"

# extract data from file and store in RDS file
if (!file.exists(dataFile)) {
	    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
		    dataFile, method = "curl", mode = "wb")
    } else if (!file.exists(dataRDS)) {
        storm_df <- read.csv(bzfile(dataFile))
        saveRDS(storm_df, dataRDS)
}

# check to see if data frame loaded already
if (exists("storm_df") == FALSE) {
    storm_df <- readRDS(dataRDS)
}

