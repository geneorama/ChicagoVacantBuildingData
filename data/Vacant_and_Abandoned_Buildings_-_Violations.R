
##------------------------------------------------------------------------------
## INITIALIZATION
##------------------------------------------------------------------------------

## Only initialize if running manually!!

if(FALSE){
    rm(list=ls())
    gc()
    library(geneorama)
    detach_nonstandard_packages()
    library(geneorama)
    loadinstall_libraries(c("geneorama", "data.table"))
    sourceDir("functions/")
}

##------------------------------------------------------------------------------
## DEFINE FILE NAMES
##------------------------------------------------------------------------------

infile <- 'https://data.cityofchicago.org/api/views/kc9i-wq85/rows.csv?accessType=DOWNLOAD'

## Note, it would be nice to have the system date in the file name,
## but then it would complicate the load process... so timestamp
## isn't implemented
outfile <- paste("data/Vacant_and_Abandoned_Buildings_-_Violations", 
                     # format(Sys.time(), "%Y%m%d"),
                     sep = "_")
outfile_csv <- paste0(outfile, ".csv")
outfile_rds <- paste0(outfile, ".Rds")

##------------------------------------------------------------------------------
## DOWNLOAD CSV
##------------------------------------------------------------------------------
if(Sys.info()['sysname']=="Linux"){
	download.file(url = infile, destfile = outfile_csv, method='wget')    
} else if  (Sys.info()['sysname']=="Windows"){
	download.file(url = infile, destfile = outfile_csv, method='internal')    
} else {
	download.file(url = infile, destfile = outfile_csv, method='curl')
}

##------------------------------------------------------------------------------
## LOAD DATA FROM CSV
##------------------------------------------------------------------------------
dat <- data.table(read.table(outfile_csv, 
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
saveRDS(dat, outfile_rds)

unlink(outfile_csv)
