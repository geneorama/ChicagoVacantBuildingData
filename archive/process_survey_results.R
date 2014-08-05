rm(list=ls())

library(geneorama)
library(data.table)

# dat <- read.delim("clipboard", stringsAsFactors = FALSE)
# dat <- data.table(dat)
# setnames(dat, gsub("\\.", "", colnames(dat)))
# setnames(dat, "Timestamp", "timestamp")
# setnames(dat, "Name", "name")
# setnames(dat, "Comments", "comments")
# setnames(dat, "Organization", "organization")
# setnames(dat, "Whichnightshaveyouattended", "dates")
# setnames(dat, "Howmanybreakoutgroupsessionshaveyouattended", "visits")
# setnames(dat, "Doyouplantoattendinthefuture", "plans")
# setnames(dat, "Doyouhaveanyexperienceintheseareas", "experience")
# setnames(dat, "Inwhatareaswouldyouliketocontributeorlearn", "contribute")

# dir.create("data/20140805")
# saveRDS(dat, "data/20140805/survey_results.Rds")


dat <- readRDS("data/20140805/survey_results.Rds")

dat
colnames(dat)


dat_dates <- regmatches(x = dat$dates, 
						m = gregexpr(pattern = "[0-9]+/[0-9]+/[0-9]+", 
									 text = dat$dates))
dat_dates <- list2matrix(dat_dates)
dat_dates[ , order(as.IDate(colnames(dat_dates), "%m/%d/%Y"))]

dat$dates <- NULL

dat

dat_experience <- dat$experience
dat_experience <- gsub("City, county, or census data", 
					   "City county or census data", 
					   dat_experience)
dat_experience <- gsub("[[:space:]]", "", dat_experience)
dat_experience <- list2matrix(strsplit(dat_experience, ","))
dat_experience
dat$experience <- NULL



dat_contribute <- dat$contribute
dat_contribute <- gsub("[[:space:]]", "", dat_contribute)
dat_contribute <- list2matrix(strsplit(dat_contribute, ","))
dat_contribute
dat$contribute <- NULL

dat_comments <- dat$comments
dat$comments <- NULL

dat
clipper(dat)
clipper(dat_dates)
clipper(dat_experience)
clipper(dat_contribute)
clipper(dat_comments)


