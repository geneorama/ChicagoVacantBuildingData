
##------------------------------------------------------------------------------
## INITIALIZATION
##------------------------------------------------------------------------------

rm(list=ls())
gc()
library(geneorama)
detach_nonstandard_packages()
library(geneorama)
loadinstall_libraries(c("geneorama", "data.table"))
sourceDir("functions/")

##------------------------------------------------------------------------------
## LOAD DATA FROM CSV
##------------------------------------------------------------------------------

fp <- 'data/20140529/311_Service_Requests_-_Vacant_and_Abandoned_Buildings_Reported.csv'

dat <- data.table(read.table(fp, 
							 header = TRUE, 
							 sep = ",", 
							 comment = "", 
							 quote = "\"",  
							 nrows = -1,
							 stringsAsFactors = FALSE))
str(dat)

##------------------------------------------------------------------------------
## CONVERT STRINGS TO DATE
##------------------------------------------------------------------------------

## Find columns that generally have a  "m/d/y" format:
datepattern <- "^[[:digit:]]{2}\\/[[:digit:]]{2}\\/[[:digit:]]{4}"
DatePatternCount <- sapply(X = sample(dat)[1:10000],
						   FUN = function(x) length(grep(datepattern, x)))
DatePatternCount[DatePatternCount != 0]
ColsToConvet <- names(DatePatternCount[DatePatternCount != 0])
ColsToConvet

NAsummary(dat)

## CONVERT
convert_datatable_StringDate(dat = dat, cols = ColsToConvet, fmt = "%m/%d/%Y")
saveRDS(dat, "data/20140529/311_Service_Requests_-_Vacant_and_Abandoned_Buildings_Reported.Rds")
