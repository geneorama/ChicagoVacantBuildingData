
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

fp <- 'data/20140507/Vacant_and_Abandoned_Buildings_-_Violations.csv'

dat <- data.table(read.table(fp, 
							 header = TRUE, 
							 sep = ",", 
							 comment = "", 
							 quote = "\"",  
							 nrows = -1,
							 stringsAsFactors = FALSE))

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

## CONVERT
convert_datatable_StringDate(dat = dat, cols = ColsToConvet, fmt = "%m/%d/%Y")

##------------------------------------------------------------------------------
## CONVERT CURRENCY FORMAT TO NUMERIC
##------------------------------------------------------------------------------

CurrencyFormat <- c("Total.Fines",
					"Total.Administrative.Costs",
					"Interest.Amount",
					"Collection.Costs.or.Attorney.Fees",
					"Court.Cost",
					"Original.Total.Amount.Due",
					"Total.Paid",
					"Current.Amount.Due")

for (j in CurrencyFormat){
	## Remove dollar sign
	set(x=dat, j=j, value=gsub("\\$", "", dat[[j]]))
	## Cast as numeric
	set(x=dat, j=j, value=as.numeric(dat[[j]]))
}

##------------------------------------------------------------------------------
## CHECK RESULTS AND SAVE
##------------------------------------------------------------------------------

str(dat)
saveRDS(dat, "data/20140507/Vacant_and_Abandoned_Buildings_-_Violations.Rds")

