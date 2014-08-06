
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
## LOAD DATA
##------------------------------------------------------------------------------
datBuild <- readRDS("data/20140507/Building_Violations.Rds")
## CONVERT NAMES
setnames(datBuild, 
		 old = colnames(datBuild), 
		 new = gsub("\\.", "_", colnames(datBuild)))
## SUMMARIZE DATA
NAsummary(datBuild)
str(datBuild)

##------------------------------------------------------------------------------
## CREATE VIOLATION_ORDINANCE
##------------------------------------------------------------------------------
# ex <- as.character(datBuild[1:10, VIOLATION_ORDINANCE])
# ex
# regmatches(ex, gregexpr("[0-9]+-[0-9]+-[0-9]+", ex))
VIOLATION_ORDINANCE <- as.character(datBuild[ , VIOLATION_ORDINANCE])
VIOLATION_ORDINANCE <- regmatches(x = VIOLATION_ORDINANCE, 
								  m = gregexpr(pattern = "[0-9]+-[0-9]+-[0-9]+", 
								  			 text = VIOLATION_ORDINANCE))
names(VIOLATION_ORDINANCE) <- datBuild[ , ID]

##------------------------------------------------------------------------------
## SAVE VIOLATION_ORDINANCE
##------------------------------------------------------------------------------
saveRDS(VIOLATION_ORDINANCE,
		'data/20140507/Building_Violations__VIOLATION_ORDINANCE.Rds')

if(FALSE){
	## OPTIONAL PLOTS
	head(VIOLATION_ORDINANCE)
	tail(VIOLATION_ORDINANCE)
	VIOLATION_ORDINANCE[1:10]
	length(unique(unlist(VIOLATION_ORDINANCE)))
	hist(table(unlist(VIOLATION_ORDINANCE)), 100)
	hist(log10(table(unlist(VIOLATION_ORDINANCE))), 20)
}



