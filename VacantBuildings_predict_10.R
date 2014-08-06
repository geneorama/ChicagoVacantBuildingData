

##------------------------------------------------------------------------------
## INITIALIZATION
##------------------------------------------------------------------------------
rm(list=ls())
gc()
library(geneorama)
detach_nonstandard_packages()
library(geneorama)
## Load and/or install 
loadinstall_libraries(c("geneorama", "data.table", "ggplot2", 
						"reshape2", "corrplot", "rpart", "party"))
## Load functions for this project
sourceDir("functions/")

##------------------------------------------------------------------------------
## LOAD DATA OBJECTS
##------------------------------------------------------------------------------

## Filepaths to data objects used in this script
## See associated R files for documentation
fp1 <- 'data/op'
fp2 <- "data/Vacant_and_Abandoned_Buildings_-_Violations"
fp3 <- "data/Building_Violations"
fp4 <- "data/Building_Violations__VIOLATION_ORDINANCE"
fp5 <- "data/Building_Violations__VIOLATION_ORDINANCE_dummy_matrix"

## Create Rds files, if they don't exist locally
if(!file.exists(paste0(fp1, ".Rds"))) source(paste0(fp1, ".R"), local=TRUE, echo=TRUE)
if(!file.exists(paste0(fp2, ".Rds"))) source(paste0(fp2, ".R"), local=TRUE, echo=TRUE)
if(!file.exists(paste0(fp3, ".Rds"))) source(paste0(fp3, ".R"), local=TRUE, echo=TRUE)
if(!file.exists(paste0(fp4, ".Rds"))) source(paste0(fp4, ".R"), local=TRUE, echo=TRUE)
if(!file.exists(paste0(fp5, ".Rds"))) source(paste0(fp5, ".R"), local=TRUE, echo=TRUE)

## LOAD RDS FILES
op <- readRDS("data/op.Rds")
datVacant <- readRDS(paste0(fp2, ".Rds"))
datBuild <- readRDS(paste0(fp3, ".Rds"))
VIOLATION_ORDINANCE <- readRDS(paste0(fp4, ".Rds"))
VIOLATION_ORDINANCE_dmat <- readRDS(paste0(fp5, ".Rds"))

## LOAD DEFAULT PAR VALUES
par(op)

## CONVERT NAMES
setnames(datBuild, 
		 old = colnames(datBuild), 
		 new = gsub("\\.", "_", colnames(datBuild)))
setnames(datVacant, 
		 old = colnames(datVacant), 
		 new = gsub("\\.", "_", colnames(datVacant)))

## CONVERT APPROPRIATE COLUMNS TO FACTOR
names_factors_vacant <- c("Issuing_Department", "Violation_Type", 
						  "Disposition_Description", "Entity_or_Person_s_")
convert_datatable_StringFactor(dat = datVacant, cols = names_factors_vacant)

## CONVERT APPROPRIATE COLUMNS TO FACTOR
names_factors_build <- c("VIOLATION_STATUS", "VIOLATION_CODE", 
						 "VIOLATION_DESCRIPTION", "VIOLATION_ORDINANCE", 
						 "INSPECTOR_ID", "INSPECTION_STATUS", 
						 "INSPECTION_WAIVED", "INSPECTION_CATEGORY", 
						 "DEPARTMENT_BUREAU")
convert_datatable_StringFactor(dat = datBuild, cols = names_factors_build)

## Convert column classes to interger based dates
convert_datatable_DateIDate(datBuild)
convert_datatable_DateIDate(datVacant)

## CONVERT LATITUDE TO NUMERIC
datBuild[ , LATITUDE:=as.numeric(LATITUDE)]

## ADD FIELD: id
datBuild[ , id := 1:nrow(datBuild)]
names(VIOLATION_ORDINANCE) = 1:length(VIOLATION_ORDINANCE)


##------------------------------------------------------------------------------
## BASIC DATA SUMMARIES
##------------------------------------------------------------------------------

## Basic data summaries
str(datVacant)
str(datBuild)

## Show NA and Unique Value (and remove columns with only one unique value)
naVacant <- NAsummary(datVacant)
naBuild <- NAsummary(datBuild)
naVacant
naBuild

## ONLY KEEP FIELDS WITH MORE THAN ONE UNIQUE VALUE
datVacant <- datVacant[ , which(naVacant$nUnique > 1), with = F]
datBuild <- datBuild[ , which(naBuild$nUnique > 1), with = F]

## Remove NA summaries
rm(naVacant, naBuild)

##------------------------------------------------------------------------------
## MATCH ADDRESSES BETWEEN VACANT AND VIOLATION DATA
##------------------------------------------------------------------------------
## Create a "Property_Address" field in the Vacant Building data
## This is accomplished by taking the suffix out of the ADDRESS field
datBuild[ , Property_Address := gsub(" [A-Z]+$", " ", ADDRESS)]

## Test for overlap between databases
inin(x = unique(datVacant$Property_Address), 
	 y = unique(datBuild$Property_Address))

## Set the key fo datBuild to match Vacant property database
setkey(datBuild, Property_Address)

##------------------------------------------------------------------------------
## ADD COLUMNS BASED ON VACANCY DATABASE
##     1) Vacant_Database_Address
##     2) Vacant_Database_EarliestDate
##------------------------------------------------------------------------------

## Merge info from vacant database into building data
datBuild <- merge(x = datBuild, 
				  y = datVacant[i = TRUE, 
				  			    j = list(Vacant_Database_Address = TRUE,
				  			  		     Vacant_Database_EarliestDate = 
				  			    		 	min(Issued_Date)), 
				  			  keyby = Property_Address], 
				  all.x = TRUE, 
				  all.y = FALSE)

## Records in the Build data that have no match to the Vacant data appear as NA
## Change NA to "FALSE", meaning that they were not found
set(x=datBuild, 
	i = which(is.na(datBuild$Vacant_Database_Address)), 
	j = which(colnames(datBuild)=="Vacant_Database_Address"), 
	value = FALSE)

##^^^^^^^^^^^^^^^^^^
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## TEST:
	## The number of rows should be the same:
	datBuild[ , .N, Property_Address]
	datBuild[ , .N, list(Property_Address, Vacant_Database_Address)]
	
	datBuild[ , any(Vacant_Database_Address), Property_Address][,dftab(V1)]
	datBuild[ , all(Vacant_Database_Address), Property_Address][,dftab(V1)]
}

##------------------------------------------------------------------------------
## ADD INDICATORS OF VACANCY TO BASED ON VIOLATION DATA
##     4) Vacant_Violation
##     5) Vacant_Violation_Address
##     6) Vacant_Violation_EarliestDate
##------------------------------------------------------------------------------
## Add field to indicate vacant by VIOLATION_DESCRIPTION or VIOLATION_ORDINANCE
datBuild[i = TRUE , 
		 Vacant_Violation := 
		 	grepl("ABDN|VACANT|VCNT", VIOLATION_DESCRIPTION, ignore.case=T) |
		 	grepl("ABDN|VACANT|VCNT", VIOLATION_ORDINANCE, ignore.case=T)
		 ]
## Indicator grouped by address
datBuild[ , Vacant_Violation_Address := any(Vacant_Violation), Property_Address]

##^^^^^^^^^^^^^^^^^^
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## TEST:
	## The second table should have more true values
	datBuild[ , table(Vacant_Violation)]
	datBuild[ , table(Vacant_Violation_Address)]
}

## Calculate "Vacant_Violation_EarliestDate", then merge it into "datBuild"
EarlyDateTable <- datBuild[i = Vacant_Violation == TRUE, 
						   j = list(Vacant_Violation_EarliestDate = min(VIOLATION_DATE)), 
						   keyby = Property_Address]
datBuild <- merge(x = datBuild, y = EarlyDateTable, all.x=TRUE)
## Remove intermediate result
rm(EarlyDateTable)

##------------------------------------------------------------------------------
## ADD COMBINED INDICATOR BASED ON VACANT DATA AND VIOLATION DATA
##     7) Vacant_Address
##------------------------------------------------------------------------------
datBuild[ , Vacant_Address := Vacant_Database_Address | Vacant_Violation_Address]

##^^^^^^^^^^^^^^^^^^
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## Summary of 
	## 1) Addresses that are identified as vacant
	## 2) Addresses that are ONLY identified as vacant, and have no other violations
	datBuild[i = TRUE,
			 j = list(mixed = any(Vacant_Violation),
			 		 all = all(Vacant_Violation)), 
			 by = list(Property_Address, Vacant_Address)][
			 	i = TRUE, 
			 	j = .N, 
			 	by = list(mixed, all, Vacant_Address)]
	
	## Addresses which were only identified by the vacant property list
	datBuild[i = TRUE , 
			 j = all(Vacant_Database_Address) & !all(Vacant_Violation_Address), 
			 by = Property_Address][,table(V1)]
}

##==============================================================================
## CREATE "datBuild_model" BY USING PREVIOUS INDICATORS
##==============================================================================

##------------------------------------------------------------------------------
## REMOVE RECORDS THAT OCCUR AFTER THE VACANCY IS KNOWN
##------------------------------------------------------------------------------
## datBuild without any future information
## datBuild_model will be the basis for all datModel objects
datBuild[ , crit1 := Vacant_Address == FALSE]
datBuild[ , crit2 := VIOLATION_DATE < Vacant_Database_EarliestDate | 
		 	                                 is.na(Vacant_Database_EarliestDate)]
datBuild[ , crit3 := VIOLATION_DATE < Vacant_Violation_EarliestDate | 
		 	                                 is.na(Vacant_Violation_EarliestDate)]
datBuild_model <- datBuild[crit1 | (crit2 & crit3)]
datBuild_model <- droplevels(datBuild_model)

##^^^^^^^^^^^^^^^^^^
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## Summary of how many properties were considered vacant, and how many 
	## were left in the model data:
	nrow(datBuild)
	nrow(datBuild_model)
	datBuild[ , all(Vacant_Address), Property_Address][,dftab(V1)]
	datBuild_model[ , all(Vacant_Address), Property_Address][,dftab(V1)]
}
## Testing addresses that were completely excluded to make sure that 
## the logic was correct:
if(FALSE){
	db = datBuild[ , all(Vacant_Address), Property_Address]
	dbm = datBuild_model[ , all(Vacant_Address), Property_Address]
	# wtf(db)
	# wtf(dbm)
	datBuild[Property_Address=="10 W 103RD ",list(
		VIOLATION_DATE, VIOLATION_DESCRIPTION, Vacant_Address,
		Vacant_Database_EarliestDate, Vacant_Violation, Vacant_Violation_Address,
		Vacant_Violation_EarliestDate, Vacant_Address, crit1, crit2, crit3)]
	datBuild[Property_Address=="100 E 35TH ",list(
		VIOLATION_DATE, VIOLATION_DESCRIPTION, Vacant_Address,
		Vacant_Database_EarliestDate, Vacant_Violation, Vacant_Violation_Address,
		Vacant_Violation_EarliestDate, Vacant_Address, crit1, crit2, crit3)]
	datBuild[Property_Address=="100 N LOTUS ",list(
		VIOLATION_DATE, VIOLATION_DESCRIPTION, Vacant_Address,
		Vacant_Database_EarliestDate, Vacant_Violation, Vacant_Violation_Address,
		Vacant_Violation_EarliestDate, Vacant_Address, crit1, crit2, crit3)]
	datBuild[Property_Address=="1000 N LATROBE ",list(
		VIOLATION_DATE, VIOLATION_DESCRIPTION, Vacant_Address,
		Vacant_Database_EarliestDate, Vacant_Violation, Vacant_Violation_Address,
		Vacant_Violation_EarliestDate, Vacant_Address, crit1, crit2, crit3)]
}
if(FALSE){
	## Test 2
	addr_1340010 <- datBuild[grep("13-40-010", VIOLATION_ORDINANCE), 
							 unique(Property_Address)]
	addr_1340010[1]
	## This should be "100 E 71ST "
	temp1 <- datBuild[Property_Address %in% addr_1340010[1]]
	temp1$idFoundInDatModel <- temp1$id %in% datBuild_model$id
	# clipper(temp1)
	
	temp2 <- datBuild_model[Property_Address %in% addr_1340010[1]]
	# clipper(temp2)
	rm(addr_1340010)
	rm(temp1, temp2)
}


datBuild_model[ , crit1 := NULL]
datBuild_model[ , crit2 := NULL]
datBuild_model[ , crit3 := NULL]
datBuild[ , crit1 := NULL]
datBuild[ , crit2 := NULL]
datBuild[ , crit3 := NULL]

##------------------------------------------------------------------------------
## USE datBuild_model TO CREATE A SUMMARY OF THE INSPECTION_CATEGORY FIELD
##------------------------------------------------------------------------------
datBuild_model
dim(datBuild_model)
colnames(datBuild_model)
datModel_insp <- dcast.data.table(data = datBuild_model[i = TRUE , 
														j = .N, 
														by = list(Property_Address, 
																  INSPECTION_CATEGORY)], 
								  formula = Property_Address ~ INSPECTION_CATEGORY,
								  value.var = "N",
								  fill = 0L)
setkey(datModel_insp, Property_Address)
datModel_insp

##------------------------------------------------------------------------------
## USE datBuild_model AND VIOLATION_ORDINANCE (IMPORTED) TO CREATE A 
## SUMMARY OF THE VIOLATION_ORDINANCE FIELD
##------------------------------------------------------------------------------

as.matrix(VIOLATION_ORDINANCE_dmat[1:10, 1:10])
datBuild[1:10, ID]



## Since the elements in VIOLATION_ORDINANCE match datBuild, we can pull out the
## ordinances that are in datBuild_model by using the id
## Check:
length(VIOLATION_ORDINANCE) == nrow(datBuild)

## Aggregate text in each list element of VIOLATION_ORDINANCE
VIOLATION_ORDINANCE_split <- split(VIOLATION_ORDINANCE[datBuild_model$id], 
								   datBuild_model$Property_Address)
VIOLATION_ORDINANCE_split <- lapply(VIOLATION_ORDINANCE_split, unlist)
VIOLATION_ORDINANCE_split <- lapply(VIOLATION_ORDINANCE_split, unname)


## Unlist the list of Violations to make into a data.table
## the addresses will be in the names of the unlisted data
VIOLATION_ORDINANCE_dt <- unlist(VIOLATION_ORDINANCE_split)
VIOLATION_ORDINANCE_dt <- data.table(
	Property_Address = gsub(" [0-9]+$", " ", names(VIOLATION_ORDINANCE_dt)),
	violation = unname(VIOLATION_ORDINANCE_dt))
setkey(VIOLATION_ORDINANCE_dt, Property_Address)

##xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# ## Similar method to make VIOLATION_ORDINANCE_dt
# 
# VIOLATION_ORDINANCE_dt <- merge(x = VIOLATION_ORDINANCE_dt,
# 								y = datBuild_model[i = TRUE , 
# 												   j = list(Vacant_Address=any(Vacant_Address)), 
# 												   keyby=Property_Address])
# VIOLATION_ORDINANCE_dt
##xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

## Note: there are fewer addresses than in the datBuild_model because
## some addresses have no citations
VIOLATION_ORDINANCE_dt[ , .N, Property_Address]



##^^^^^^^^^^^^^^^^^^
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## checking address matches
	datBuild_model[ , .N, Property_Address]
	length(VIOLATION_ORDINANCE_split)
	table(datBuild_model[,.N,Property_Address]$Property_Address == 
		  	names(VIOLATION_ORDINANCE_split))

	## Checking an example that SHOULD NOT come up
	VIOLATION_ORDINANCE_split[['11318 S STATE ']]
	datBuild[Property_Address=='11318 S STATE ', VIOLATION_ORDINANCE]
	datBuild_model[Property_Address=='11318 S STATE ', VIOLATION_ORDINANCE]
	
	## Checking an example that SHOULD come up
	VIOLATION_ORDINANCE_split[['100 E CHESTNUT ']]
	datBuild[Property_Address=='100 E CHESTNUT ', VIOLATION_ORDINANCE]
}

## ANALYSIS MANUALLY DONE IN EXCEL TO COME UP WITH KeyViolations
# clipper(VIOLATION_ORDINANCE_dt[Vacant_Address == TRUE, .N, violation])
# clipper(VIOLATION_ORDINANCE_dt[Vacant_Address == FALSE, .N, violation])

KeyViolations <- c("13-196-641", "13-196-530", "13-196-550", "13-196-630",
				   "13-196-570", "13-12-100", "18-27-410", "13-32-010",
				   "13-12-050", "13-196-590", "13-12-030", "13-12-125", 
				   "13-12-130", "13-196-540", "13-8-100", "13-12-140", 
				   "13-40-020", "13-12-135", "13-12-120", "11-31-1", 
				   "13-40-010", "15-4-970", "13-12-126")

datModel_violations <- VIOLATION_ORDINANCE_dt[
	i = TRUE, 
	j = list(TotalViolations = .N), 
	keyby = Property_Address]

VIOLATION_ORDINANCE_dt[i = violation %in% KeyViolations]

violation_mat <- dcast.data.table(
	data = VIOLATION_ORDINANCE_dt[i = violation %in% KeyViolations, 
								  j = .N, 
								  by = list(Property_Address, 
								  		  violation)], 
	formula = Property_Address ~ violation,
	value.var = "N",
	fill = 0L)
setkey(violation_mat, Property_Address)
datModel_violations <- violation_mat[datModel_violations]

## Addin the count of all the rows in the Building database
## this is both a good data point, but also it merges back in any addresses
## that don't have any actual citations in their details.
datModel_violations <- datModel_violations[datBuild_model[i = TRUE,
														  j = list(RowEntries=.N),
														  keyby=Property_Address]]

for(j in 1:ncol(datModel_violations)){
	set(x = datModel_violations,
		i = which(is.na(datModel_violations[[j]])),
		j = colnames(datModel_violations)[j],
		value = 0)
}

## How many of the Violations occur in the popular categories
jj <- grep(pattern = "[0-9]+-[0-9]+-[0-9]+", 
		   x = colnames(datModel_violations))

datModel_violations$pctTotalViolationsInDetail <-
	apply(datModel_violations[,jj,with=F], 1, sum) / 
	datModel_violations$TotalViolations
j <- which(colnames(datModel_violations) == "pctTotalViolationsInDetail")
set(x = datModel_violations,
	i = which(is.nan(datModel_violations[[j]])),
	j = colnames(datModel_violations)[j],
	value = 0)


datModel_violations

##------------------------------------------------------------------------------
## USE datBuild_model TO CREATE A SUMMARY OF THE INSPECTOR_ID FIELD
## INSPECTOR_ID IS NOT USED... SEE ANALYSIS
##------------------------------------------------------------------------------
head(datBuild_model)
temp = sort(datBuild_model[,.N,INSPECTOR_ID]$N, decreasing=T)
plot(cumsum(temp)/sum(temp),
	 ylab = "Cumulative distribution",
	 xlab = "Nth Unique Building Inspector",
	 main="Percent of Covererage Contributed by the Nth Inspector", 
	 panel.first=bgfun())
length(unique(datBuild_model$INSPECTOR_ID))
rm(temp)

##------------------------------------------------------------------------------
## USE datBuild_model TO CREATE A SUMMARY OF THE VIOLATION_STATUS FIELD
##------------------------------------------------------------------------------
datModel_status <- dcast.data.table(
	data = datBuild_model[i = TRUE, 
						  j = .N, 
						  by = list(Property_Address, 
						  		  VIOLATION_STATUS)], 
	formula = Property_Address ~ VIOLATION_STATUS,
	value.var = "N",
	fill = 0L)
setkey(datModel_status, Property_Address)


##------------------------------------------------------------------------------
## USE datBuild_model TO CREATE A SUMMARY OF THE NUMBER OF VISITS
##------------------------------------------------------------------------------
datModel_visits <- datBuild_model[ , .N, list(Property_Address, VIOLATION_DATE)]
datModel_visits <- datModel_visits[ , list(TotalVisits = .N), Property_Address]
datModel_visits
datModel_visits[TotalVisits>25]
datModel_visits$TotalVisits <- pmin(datModel_visits$TotalVisits, 25)
setkey(datModel_visits, Property_Address)

datModel_visits[,hist(TotalVisits)]
datModel_visits[,table(TotalVisits>1)]

##------------------------------------------------------------------------------
## CALCULATE VACANT INDICATOR PER Property_Address
##------------------------------------------------------------------------------

datModel_vacant <- datBuild_model[i = TRUE ,
								  j = list(Vacant_Address = any(Vacant_Address)),
								  keyby = Property_Address]
datModel_vacant[ , dftab(Vacant_Address)]
datModel_vacant

##==============================================================================
## COMBINE ALL VERIONS OF datModel
##==============================================================================
lll()

datModel <- datModel_violations[datModel_insp][datModel_status][datModel_visits]
datModel <- datModel[datModel_vacant]


datModel[ , dftab(Vacant_Address)]
datBuild_model[ , list(Vacant_Address=any(Vacant_Address)), Property_Address][ , dftab(Vacant_Address)]
datBuild[ , list(Vacant_Address=any(Vacant_Address)), Property_Address][ , dftab(Vacant_Address)]


##==============================================================================
## SAVE datModel
##==============================================================================

outfile <- "data/20140507/VacantBuildingData.Rds"

saveRDS(datModel, outfile)

# lll()
# rm(j, jj, names_factors_build, names_factors_vacant)
# save.image("data/20140507/VacantBuildingData_workspace.RData")
# load("data/20140507/VacantBuildingData_workspace.RData")




##==============================================================================
##==============================================================================
## EXCEL SUMMARIES AND GRAPHS
##==============================================================================
##==============================================================================


##^^^^^^^^^^^^^^^^^^
## Summary for Sunil
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## Work to answer Hugh's original questions:
	## from "BuildingViolations_analysis.R"
	
	lll()
	
	nrow(VIOLATION_ORDINANCE_dmat)
	nrow(datBuild)
	
	str(VIOLATION_ORDINANCE_dmat)
	head(sort(VIOLATION_ORDINANCE_dmat@i))
	
	## Create smaller Dummy Matrix
	setkey(datBuild, id)
	DM <- VIOLATION_ORDINANCE_dmat[!duplicated(datBuild$VIOLATION_CODE), ]
	DM <- data.table(as.matrix(DM))
	setnames(DM, unique(unlist(VIOLATION_ORDINANCE)))
	
	DMTotals <- vector("numeric", ncol(VIOLATION_ORDINANCE_dmat))
	for(j in 1:ncol(VIOLATION_ORDINANCE_dmat)) {
		DMTotals[j] = sum(VIOLATION_ORDINANCE_dmat[,j])
	}
	jj = order(DMTotals, decreasing=TRUE)
	DM <- DM[ , jj, with=F]
	DB_VC <- datBuild[i = !duplicated(VIOLATION_CODE), VIOLATION_CODE]
	DB_VC <- as.character(DB_VC)
	DM$VIOLATION_CODE <- DB_VC
	
	## Summary of unique violation codes 
	VCombos <- datBuild[i = TRUE, 
						j =  .N, 
						by = list(VIOLATION_CODE, 
								  VIOLATION_DESCRIPTION, 
								  VIOLATION_ORDINANCE)]
	DM_detailed <- merge(VCombos, DM, by="VIOLATION_CODE")
	# wtf(DM_detailed)
}
	


##^^^^^^^^^^^^^^^^^^
## COUNT SUMMARY
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	
	COUNT_SUMMARY <- data.table(
		Total_Records = c(nrow(datBuild), 
						  nrow(datVacant),
						  nrow(datBuild_model)),
		Count_of_ADDRESS = c(length(unique(datBuild$ADDRESS)),
							 NA,
							 length(unique(datBuild_model$ADDRESS))),
		Count_of_Property_Address = c(length(unique(datBuild$Property_Address)),
									  length(unique(datVacant$Property_Address)),
									  length(unique(datBuild_model$Property_Address)))
	)
	
	COUNT_SUMMARY
	# clipper(COUNT_SUMMARY)
}

##^^^^^^^^^^^^^^^^^^
## TRUTH TABLES
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## Analysis tables for excel report:
	## Summary tables for excel
	datBuild[ , Vacant_Desc_ABDN := grepl("ABDN", VIOLATION_DESCRIPTION)]
	datBuild[ , Vacant_Desc_VACANT := grepl("VACANT", VIOLATION_DESCRIPTION)]
	datBuild[ , Vacant_Desc_VCNT := grepl("VCNT", VIOLATION_DESCRIPTION)]
	
	datBuild[ , Vacant_VIOLATION_ORDINANCE_ABDN := grepl("ABDN", VIOLATION_DESCRIPTION)]
	datBuild[ , Vacant_VIOLATION_ORDINANCE_VACANT := grepl("VACANT", VIOLATION_DESCRIPTION)]
	datBuild[ , Vacant_VIOLATION_ORDINANCE_VCNT := grepl("VCNT", VIOLATION_DESCRIPTION)]
	
	tab1 <- datBuild[i = TRUE, 
					 j = .N, 
					 by = list(Vacant_VIOLATION_ORDINANCE_ABDN,
					 		  Vacant_VIOLATION_ORDINANCE_VACANT,
					 		  Vacant_VIOLATION_ORDINANCE_VCNT,
					 		  Vacant_Desc_ABDN,
					 		  Vacant_Desc_VACANT,
					 		  Vacant_Desc_VCNT)]
	tab1
	
	tab2 <- datBuild[i = TRUE, 
					 j = .N, 
					 by = list(Vacant_VIOLATION_ORDINANCE_ABDN,
					 		  Vacant_VIOLATION_ORDINANCE_VACANT,
					 		  Vacant_VIOLATION_ORDINANCE_VCNT,
					 		  Vacant_Desc_ABDN,
					 		  Vacant_Desc_VACANT,
					 		  Vacant_Desc_VCNT,
					 		  Vacant_Database_Address)]
	tab2
	
	tab3 <- datBuild[i = TRUE, 
					 j = .N, 
					 by = list(Property_Address,
					 		  Vacant_VIOLATION_ORDINANCE_ABDN,
					 		  Vacant_VIOLATION_ORDINANCE_VACANT,
					 		  Vacant_VIOLATION_ORDINANCE_VCNT,
					 		  Vacant_Desc_ABDN,
					 		  Vacant_Desc_VACANT,
					 		  Vacant_Desc_VCNT)]
	tab3a <- tab3[i = TRUE, 
				  j = .N, 
				  by = list(Vacant_VIOLATION_ORDINANCE_ABDN,
				  		  Vacant_VIOLATION_ORDINANCE_VACANT,
				  		  Vacant_VIOLATION_ORDINANCE_VCNT,
				  		  Vacant_Desc_ABDN,
				  		  Vacant_Desc_VACANT,
				  		  Vacant_Desc_VCNT)]
	tab3
	tab3a
	
	tab4 <- datBuild[i = TRUE, 
					 j = .N, 
					 by = list(Property_Address,
					 		  Vacant_VIOLATION_ORDINANCE_ABDN,
					 		  Vacant_VIOLATION_ORDINANCE_VACANT,
					 		  Vacant_VIOLATION_ORDINANCE_VCNT,
					 		  Vacant_Desc_ABDN,
					 		  Vacant_Desc_VACANT,
					 		  Vacant_Desc_VCNT,
					 		  Vacant_Database_Address)]
	tab4a <- tab4[i = TRUE, 
				  j = .N, 
				  by = list(Vacant_VIOLATION_ORDINANCE_ABDN,
				  		  Vacant_VIOLATION_ORDINANCE_VACANT,
				  		  Vacant_VIOLATION_ORDINANCE_VCNT,
				  		  Vacant_Desc_ABDN,
				  		  Vacant_Desc_VACANT,
				  		  Vacant_Desc_VCNT,
				  		  Vacant_Database_Address)]
	tab4
	tab4a
	
	# clipper(tab1)
	# clipper(tab2)
	# clipper(tab3a)
	# clipper(tab4a)
	
	datBuild[ , Vacant_Desc_ABDN := NULL]
	datBuild[ , Vacant_Desc_VACANT := NULL]
	datBuild[ , Vacant_Desc_VCNT := NULL]
	
	datBuild[ , Vacant_VIOLATION_ORDINANCE_ABDN := NULL]
	datBuild[ , Vacant_VIOLATION_ORDINANCE_VACANT := NULL]
	datBuild[ , Vacant_VIOLATION_ORDINANCE_VCNT := NULL]
	
	rm(tab1, tab2, tab3, tab4, tab3a, tab4a)
}


##^^^^^^^^^^^^^^^^^^
## VACANT SUMMARY
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	
	## Note: there is a second join here
	VACANT_COUNTS <- datBuild[i = TRUE,
							  j = list(mixed = any(Vacant_Violation),
							  		 all = all(Vacant_Violation)), 
							  by = list(Property_Address, Vacant_Address)][
							  	i = TRUE, 
							  	j = .N, 
							  	by = list(mixed, all, Vacant_Address)]
	VACANT_COUNTS
	clipper(VACANT_COUNTS$N)

	## Note: there is a second join here
	VACANT_COUNTS_MODEL <- datBuild_model[i = TRUE,
										  j = list(mixed = any(Vacant_Violation),
										  		 all = all(Vacant_Violation)), 
										  by = list(Property_Address, Vacant_Address)][
										  	i = TRUE, 
										  	j = .N, 
										  	by = list(mixed, all, Vacant_Address)]
	VACANT_COUNTS_MODEL
	clipper(VACANT_COUNTS_MODEL$N)
	
	
	
}


##^^^^^^^^^^^^^^^^^^
## VISIT COUNT
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## Add "vacant" indicator
	datModel_visits_merged <- merge(datModel_visits, 
									datBuild[i = TRUE, 
											 j = list(Vacant=any(Vacant_Address)), 
											 keyby = Property_Address])
	datModel_visits_merged[ , Vacant_Status := ifelse(Vacant, "Vacant", "Not Vacant")]
	datModel_visits_merged
	
	ggplot(data = datModel_visits_merged,
		   mapping = aes(x = Vacant_Status, 
		   			     y = TotalVisits, 
		   			     fill = Vacant_Status)) +
		geom_boxplot() +
		scale_y_log10() +
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Number of Unique Visits Per Address\n') +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	ggplot(data = datModel_visits_merged,
		   mapping = aes(x=TotalVisits, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 1) +
		facet_grid(  ~ Vacant_Status, scales="free_y", as.table=F) +
		xlab("Distribution of Count") + 
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Number of Unique Visits Per Address\n') +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	visit_summary_data <- datModel_visits_merged[
		i = TRUE,
		j = list(quant = seq(0, 1, .1),
				 value=quantile(TotalVisits, seq(0, 1, .1))),
		by = Vacant_Status]
	visit_summary_data
	
	visit_summary <- dcast(data = visit_summary_data, 
						   formula = quant ~ Vacant_Status,
						   value.var = "value",
						   fill = 0L)
	visit_summary$All <- quantile(datModel_visits_merged$TotalVisits, 
								  seq(0, 1, .1))
	visit_summary <- data.table(visit_summary)
	setcolorder(visit_summary, c("quant", "All", "Not Vacant", "Vacant"))
	visit_summary
	# clipper(visit_summary)
	
	visit_mean_summary <- rbind(
		datModel_visits_merged[
			i = TRUE,
			j = list(mean=mean(TotalVisits)),
			by = Vacant_Status],
		
		datModel_visits_merged[
			i = TRUE,
			j = list(Vacant_Status = "ALL",
					 mean=mean(TotalVisits))]
	)
	
	t(visit_mean_summary)
	# clipper(t(visit_mean_summary))
}


##^^^^^^^^^^^^^^^^^^
## INSPECTION TYPE
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	## Summaries for excel report
	## Add "vacant" indicator
	datModel_insp_merged <- merge(datModel_insp, 
								  datBuild[i = TRUE, 
								  		 j = list(Vacant=any(Vacant_Address)), 
								  		 keyby = Property_Address])
	datModel_insp_melt <- melt(data = datModel_insp_merged, 
							   id.vars = c("Property_Address", "Vacant"),
							   measure.vars = c("COMPLAINT", "PERIODIC", "PERMIT"),
							   variable.name = "Inspection_Type",
							   value.name = "Count")
	datModel_insp_melt[ , Vacant_Status := ifelse(Vacant, "Vacant", "Not Vacant")]
	datModel_insp_melt
	
	MySummary1 <- rbind(
		datModel_insp_merged[ , list(COMPLAINT, PERIODIC, PERMIT)][
			i = TRUE,
			j = lapply(.SD, quantile, seq(0, 1, .1))],
		datModel_insp_merged[ , list(COMPLAINT, PERIODIC, PERMIT)][
			i = TRUE,
			j = lapply(.SD, mean)])
	MySummary1
	
	MySummary2 <- rbind(
		datModel_insp_merged[Vacant==FALSE, list(COMPLAINT, PERIODIC, PERMIT)][
			i = TRUE,
			j = lapply(.SD, quantile, seq(0, 1, .1))],
		datModel_insp_merged[Vacant==FALSE, list(COMPLAINT, PERIODIC, PERMIT)][
			i = TRUE,
			j = lapply(.SD, mean)])
	MySummary2
	
	MySummary3 <- rbind(
		datModel_insp_merged[Vacant==TRUE, list(COMPLAINT, PERIODIC, PERMIT)][
			i = TRUE,
			j = lapply(.SD, quantile, seq(0, 1, .1))],
		datModel_insp_merged[Vacant==TRUE, list(COMPLAINT, PERIODIC, PERMIT)][
			i = TRUE,
			j = lapply(.SD, mean)])
	MySummary3
	
	MySummaryA <- cbind(MySummary1[,1,with=F],
						MySummary2[,1,with=F],
						MySummary3[,1,with=F])
	MySummaryB <- cbind(MySummary1[,2,with=F],
						MySummary2[,2,with=F],
						MySummary3[,2,with=F])
	MySummaryC <- cbind(MySummary1[,3,with=F],
						MySummary2[,3,with=F],
						MySummary3[,3,with=F])
	clipper(MySummaryA)
	clipper(MySummaryB)
	clipper(MySummaryC)
	
	
	ggplot(data = datModel_insp_melt,
		   mapping = aes(x=Count, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .1) +
		facet_grid(Inspection_Type ~ Vacant_Status, scales="free_y", shrink=TRUE) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		ylab("Density") +
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Inspection Types by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	ggplot(data = datModel_insp_melt,
		   mapping = aes(x=Vacant_Status, y = Count + 1, fill = Vacant_Status)) +
		geom_boxplot() +
		facet_grid(Inspection_Type ~ ., scales="free_y", shrink=TRUE) +
		scale_y_log10() +
		ylab("Distribution of Count (log 10 scale)") + 
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Inspection Types by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	ggplot(data = datModel_insp_melt,
		   mapping = aes(x=Count, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 2) +
		facet_grid(Inspection_Type ~ Vacant_Status, scales="free_y", shrink=TRUE) +
		xlab("Distribution of Count") + 
		ylab("Density") +
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Inspection Types by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	ggplot(data = datModel_insp_melt,
		   mapping = aes(x=Vacant_Status, y = Count + 1, fill = Vacant_Status)) +
		geom_boxplot() +
		facet_grid(Inspection_Type ~ ., scales="free_y", shrink=TRUE) +
		ylab("Distribution of Count") + 
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Inspection Types by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
}

##^^^^^^^^^^^^^^^^^^
## STATUS TYPE
##vvvvvvvvvvvvvvvvvv
## NEW KEEP:
if(FALSE){
	## Summaries for excel report
	## Add "vacant" indicator
	datModel_status_merged <- merge(datModel_status, 
									datBuild[i = TRUE, 
											 j = list(Vacant=any(Vacant_Address)), 
											 keyby = Property_Address])
	datModel_status_melt <- melt(data = datModel_status_merged, 
								 id.vars = c("Property_Address", "Vacant"),
								 measure.vars = c("COMPLIED", "NO ENTRY", "OPEN"),
								 variable.name = "Status",
								 value.name = "Count")
	datModel_status_melt[ , Vacant_Status := ifelse(Vacant, "Vacant", "Not Vacant")]
	datModel_status_melt
	
	MySummary1 <- rbind(
		datModel_status_merged[ , list(COMPLIED, `NO ENTRY`, OPEN)][
			i = TRUE,
			j = lapply(.SD, quantile, seq(0, 1, .1))],
		datModel_status_merged[ , list(COMPLIED, `NO ENTRY`, OPEN)][
			i = TRUE,
			j = lapply(.SD, mean)])
	MySummary1
	
	MySummary2 <- rbind(
		datModel_status_merged[Vacant==FALSE, list(COMPLIED, `NO ENTRY`, OPEN)][
			i = TRUE,
			j = lapply(.SD, quantile, seq(0, 1, .1))],
		datModel_status_merged[Vacant==FALSE, list(COMPLIED, `NO ENTRY`, OPEN)][
			i = TRUE,
			j = lapply(.SD, mean)])
	MySummary2
	
	MySummary3 <- rbind(
		datModel_status_merged[Vacant==TRUE, list(COMPLIED, `NO ENTRY`, OPEN)][
			i = TRUE,
			j = lapply(.SD, quantile, seq(0, 1, .1))],
		datModel_status_merged[Vacant==TRUE, list(COMPLIED, `NO ENTRY`, OPEN)][
			i = TRUE,
			j = lapply(.SD, mean)])
	MySummary3
	
	MySummaryA <- cbind(MySummary1[,1,with=F],
						MySummary2[,1,with=F],
						MySummary3[,1,with=F])
	MySummaryB <- cbind(MySummary1[,2,with=F],
						MySummary2[,2,with=F],
						MySummary3[,2,with=F])
	MySummaryC <- cbind(MySummary1[,3,with=F],
						MySummary2[,3,with=F],
						MySummary3[,3,with=F])
	MySummaryA
	MySummaryB
	MySummaryC
	clipper(MySummaryA)
	clipper(MySummaryB)
	clipper(MySummaryC)
	
	
	ggplot(data = datModel_status_melt,
		   mapping = aes(x=Count, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .1) +
		facet_grid(Status ~ Vacant_Status, scales="free_y", shrink=TRUE) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		ylab("Density") +
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Status Categories by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	ggplot(data = datModel_status_melt,
		   mapping = aes(x=Vacant_Status, y = Count + 1, fill = Vacant_Status)) +
		geom_boxplot() +
		facet_grid(Status ~ ., scales="free_y", shrink=TRUE) +
		scale_y_log10() +
		ylab("Distribution of Count (log 10 scale)") + 
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Status Categories by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	ggplot(data = datModel_status_melt,
		   mapping = aes(x=Count, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 2) +
		facet_grid(Status ~ Vacant_Status, scales="free_y", shrink=TRUE) +
		xlab("Distribution of Count") + 
		ylab("Density") +
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Status Categories by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	ggplot(data = datModel_status_melt,
		   mapping = aes(x=Vacant_Status, y = Count + 1, fill = Vacant_Status)) +
		geom_boxplot() +
		facet_grid(Status ~ ., scales="free_y", shrink=TRUE) +
		ylab("Distribution of Count") + 
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Status Categories by Vacant Property Status\n') +
		theme(panel.grid.major = element_line(colour = "gray70")) +
		theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted")) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
}

##^^^^^^^^^^^^^^^^^^
## ORDINANCE CODE VS
## VACANCY DB
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	colnames(datBuild)
	OrdSummary <- datBuild[i = TRUE , 
						   j = .N, 
						   by = list(VIOLATION_CODE, 
						   		  VIOLATION_DESCRIPTION, 
						   		  VIOLATION_ORDINANCE,
						   		  Vacant_Database_Address)]
	OrdSummary
	OrdSummary_cast <- dcast(data = OrdSummary, 
							 formula = VIOLATION_CODE + VIOLATION_DESCRIPTION + 
							 	VIOLATION_ORDINANCE ~ Vacant_Database_Address, 
							 value.var = "N")
	OrdSummary_cast
	wtf(OrdSummary_cast)
}


##^^^^^^^^^^^^^^^^^^
## VIOLATION TYPE
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	# Summaries for excel report
	
	## Add "vacant" indicator
	datModel_violations_merged <- merge(datModel_violations, 
										datBuild[i = TRUE, 
												 j = list(Vacant=any(Vacant_Address)), 
												 keyby = Property_Address])
	convert_datatable_int_to_num(datModel_violations_merged)
	datModel_violations_melt <- melt(data = datModel_violations_merged, 
									 id.vars = c("Property_Address", "Vacant"),
									 variable.name = "Violation_Type")
	datModel_violations_melt[ , Vacant_Status := ifelse(Vacant, "Vacant", "Not Vacant")]
	
	MySummary <- datModel_violations_melt[i = TRUE , 
										  j = list(Mean = mean(value)),
										  by = list(Violation_Type, Vacant_Status)]
	MySummary <- dcast(data = MySummary, 
					   formula = Violation_Type ~ Vacant_Status, 
					   value.var = "Mean")
	MySummary
	# clipper(MySummary)
	rm(MySummary)
	
	
	lvls <- unique(datModel_violations_melt$Violation_Type)
	grp1 <- as.character(lvls)[1:5]
	grp2 <- as.character(lvls)[6:10]
	grp3 <- as.character(lvls)[11:15]
	grp4 <- as.character(lvls)[16:17]
	grp5 <- as.character(lvls)[18:19]
	grp6 <- as.character(lvls)[20]
	
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp1],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 1) +
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp2],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 1) +
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp3],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 1) +
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp4],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 1) +
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp5],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = 5) +
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp6],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .03) +
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	
	
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp1],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .2) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp2],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .2) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp3],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .2) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp4],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .2) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp5],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .1) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	ggplot(data = datModel_violations_melt[Violation_Type %in% grp6],
		   mapping = aes(x=value, fill = Vacant_Status)) +
		geom_histogram(aes(y=..density..), binwidth = .1) +
		scale_x_log10() +
		xlab("Distribution of Count (log 10 scale)") + 
		facet_grid(Vacant_Status ~ Violation_Type, scales="free_y", as.table=F) +
		scale_fill_manual(name = "Property's Vacant Status", 
						  values = c("cornflowerblue", "brown1"))
	
	rm(lvls, grp1, grp2, grp3, grp4, grp5, grp6)
}

##^^^^^^^^^^^^^^^^^^
## STR + HEAD + TAIL
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	clipper(head(datModel))
	clipper(tail(datModel))
	clipper(cbind(colnames(datBuild),
				  NAsummary(datBuild)))
	clipper(cbind(colnames(datBuild_model),
				  NAsummary(datBuild_model)))
	
}


##^^^^^^^^^^^^^^^^^^
## VIOLATION CODES
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	# clipper(head(datModel))
	# clipper(tail(datModel))
	
	hist(table(VIOLATION_ORDINANCE_dt$violation), 200)
	diff(range(data.table(unclass(table(VIOLATION_ORDINANCE_dt$violation)))))/30
	diff(range(data.table(log(unclass(table(VIOLATION_ORDINANCE_dt$violation))))))/30
	
	ggplot(data = data.table(unclass(table(VIOLATION_ORDINANCE_dt$violation))),
		   mapping = aes(x=V1)) +
		geom_histogram(binwidth = 1500, fill="cornflowerblue") +
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Violation Code Frequency\n')
	ggplot(data = data.table(unclass(table(VIOLATION_ORDINANCE_dt$violation))),
		   mapping = aes(x=V1)) +
		geom_histogram(binwidth = .15, fill="cornflowerblue") +
		scale_x_log10() +
		theme(plot.title = element_text(size = 20)) +
		labs(title='Distribution of Violation Code Frequency (x-axis log scaled)\n')
	
}

##^^^^^^^^^^^^^^^^^^
## OLD WORK
##vvvvvvvvvvvvvvvvvv
if(FALSE){
	##==============================================================================
	##==============================================================================
	##==============================================================================
	##==============================================================================
	## OLD WORK:
	##==============================================================================
	## PLOT TO MAKE TWO HISTOGRAMS OF FREQUENCIES FOR VACANT / NON VACANT RECORDS
	
	# datBuild
	# 
	# datBuild[OverlapWithVacant==TRUE, range(VIOLATION_DATE), Property_Address]
	# 
	# datBuild[Vacant==TRUE, max(VIOLATION_DATE)-min(VIOLATION_DATE), Property_Address]
	# 
	# hist(datBuild[OverlapWithVacant==TRUE, pmin(length(unique(VIOLATION_DATE)), 15), Property_Address]$V1,
	# 	 main = "Number of violation dates for vacant properties")
	# hist(datBuild[OverlapWithVacant==FALSE, pmin(length(unique(VIOLATION_DATE)), 15), Property_Address]$V1,
	# 	 main = "Number of violation dates for non-vacant properties")
	# 
	# ViolationCount = datBuild[
	# 	i = TRUE, 
	# 	j = list(CountOfViolationDates = length(unique(VIOLATION_DATE))),
	# 	by = list(isVacant=OverlapWithVacant, Property_Address)]
	# 
	# ViolationCount$CountOfViolationDates <- 
	# 	pmin(ViolationCount$CountOfViolationDates, 15)
	# 
	# ggplot(data = ViolationCount,
	# 	   mapping = aes(x=CountOfViolationDates, fill=isVacant)) +
	# 	geom_histogram(binwidth = 1, colour="black") +
	# 	guides(fill=FALSE) +
	# 	facet_wrap( ~ isVacant, scales="free") +
	# 	xlab("Distribution of Violation Frequency") + ylab("Count") +
	# 	xlim(c(0, 15)) +
	# 	theme(plot.title = element_text(size = 20)) +
	# 	labs(title='Count of Building Code Violations by Vacant Property Status\n') +
	# 	theme(panel.background = element_rect(fill = "gray60", colour = "black")) +
	# 	theme(panel.grid.major = element_line(colour = "gray40")) +
	# 	theme(panel.grid.minor = element_line(colour = "gray70", linetype = "dotted"))
	
	
	
	## Initial exploration to find if the final Disposition_Description was 
	## important
	# setkey(datVacant, Property_Address)
	# datVacant[grep("Not liable", Disposition_Description)]
	# 
	# datVacant[grep("Not liable", Disposition_Description)][,.N,Property_Address]
	# datVacant[grep("Not liable", Disposition_Description)][
	# 	,diff(range(Last_Hearing_Date)),Property_Address]
	# 
	# datVacant[grep("Not liable", Disposition_Description), Property_Address]
	# print(datVacant[datVacant[grep("Not liable", Disposition_Description), Property_Address]],10)
	# datVacant["5714 S MARSHFIELD "]
	# datVacant["4943 W KAMERLING "]
	# datVacant["850 N SPRINGFIELD "]
	# datVacant["5121 S UNION "]
	# 
	# 
	# "City non-suit"
	# "Liable - By Plea - Motion to set-aside default granted"
	# "Default - Liable by prove-up"
	# "City's motion to continue - Granted"
	# "Liable - By plea"
	# "Not liable - Respondent came into compliance with building code prior to hearing"
	# "Not liable - City failed to meet burden of proof"
	# "City Non suit - Motion to set-aside default - Granted"
	# "Not liable - City failed to establish Prima Facie case - Motion to set-aside default granted"
	# "Not liable - City failed to establish prima facie case"
	# "Dismissed for want of prosecution - No service"
	# "Denied - Motion to set-aside default - lack of good cause"
	# "Respondent's motion to continue granted"
	# "Denied - Motion to set aside denied - Prior default order still stands"
	# "Liable - By contested finding"
	# "Respondent's motion to continue - granted / Motion to Set Aside default granted"
	# 
	# ## Why are some addresses in here twice?
	# datVacant[ , .N, Property_Address]
	# datVacant[ , .N, list(Property_Address, Issued_Date)][,.N,Property_Address]
	# datVacant[Property_Address=="1701 N LINDER "]
	# datVacant[Property_Address=="4746 W MAYPOLE "]
	# datVacant[Property_Address=="4746 W MAYPOLE ", .N, list(Property_Address, Issued_Date)]
	# 
	# 
	# datVacant[ , .N, Property_Address]
	# 
	# 
	# ## Need to get earliest date
	
	# ## Summary of dispositions
	# datVacant[, .N, Disposition_Description]
	
	# clipper(apply(datVacant[, .N, Disposition_Description], 1, 
	# 			  paste, collapse=" --- "))
}




