##------------------------------------------------------------------------------
## INITIALIZATION
##------------------------------------------------------------------------------
rm(list=ls())
gc()
library(geneorama)
detach_nonstandard_packages()
library(geneorama)
loadinstall_libraries(c("geneorama", "data.table", "ggplot2", "reshape2"))

sourceDir("functions/")
op <- readRDS("data/op.Rds")  ## Default par
par(op)

##------------------------------------------------------------------------------
## Load data for vacant buildings and violations
##------------------------------------------------------------------------------
datVacant <- readRDS("data/20140507/Vacant_and_Abandoned_Buildings_-_Violations.Rds")
datBuild <- readRDS("data/20140507/Building_Violations.Rds")
datBuild_10 <- readRDS('data/20140507/VacantBuildingData.Rds')

## Convert Names
setnames(datBuild, 
		 old = colnames(datBuild), 
		 new = gsub("\\.", "_", colnames(datBuild)))
setnames(datVacant, 
		 old = colnames(datVacant), 
		 new = gsub("\\.", "_", colnames(datVacant)))
## CONVERT LATITUDE TO NUMERIC
datBuild[ , LATITUDE:=as.numeric(LATITUDE)]


##------------------------------------------------------------------------------
## Merge lat / long data to the processed data (from the 10 file)
##------------------------------------------------------------------------------
## Create a "Property_Address" field 
## This is accomplished by taking the suffix out of the ADDRESS field
datBuild[ , Property_Address := gsub(" [A-Z]+$", " ", ADDRESS)]

## Set the key fo datBuild to match Vacant property database
setkey(datBuild, Property_Address)
setkey(datBuild_10, Property_Address)

dat <- merge(x = datBuild_10, 
			 y = datBuild[i = TRUE , 
			 			 j = list(LATITUDE = unique(LATITUDE), 
			 			 		 LONGITUDE = unique(LONGITUDE)), 
			 			 keyby = Property_Address])
## Dang, we have some multiple properties (it's from dropping the street suffix)
nrow(dat)
nrow(datBuild_10)

## Get rid of multiple properties
properties <- unique(dat[ , Property_Address])
dat <- dat[properties, .SD, mult="first"]

## that's better
nrow(dat)
nrow(datBuild_10)


##------------------------------------------------------------------------------
## Vacant property geography
##------------------------------------------------------------------------------
dat$LATITUDE[1]
dat$LATITUDE[1] == 41.68885
print(dat$LATITUDE[1], digits=20)
dat[ , LATITUDE:=round(LATITUDE, 6)]
dat[ , LONGITUDE:=round(LONGITUDE, 6)]

dat$LATITUDE[1]
print(dat$LATITUDE[1], digits=20)
dat$LATITUDE[1] == 41.688848


dat$LATITUDE[1]
print(dat$LONGITUDE[1], digits=20)

dat[LATITUDE == 41.688848 & LONGITUDE == -87.622894]

hist(datBuild$LATITUDE)
hist(datBuild$LONGITUDE)

LT <- 41.86686398
LO <- -87.69850327
rng =.001
dat[i = LATITUDE > (LT-rng) & LATITUDE < (LT+rng) & 
		LONGITUDE > (LO-rng) & LONGITUDE < (LO+rng), 
	j = .N, 
	by = Property_Address]
datBuild[i = LATITUDE > (LT-rng) & LATITUDE < (LT+rng) & 
		 	LONGITUDE > (LO-rng) & LONGITUDE < (LO+rng), 
		 j = .N, 
		 by = Property_Address]

##------------------------------------------------------------------------------
## Vacant property map
##------------------------------------------------------------------------------
dat[Vacant_Address==TRUE, .N, list(LONGITUDE, LATITUDE)]
ggplot(dat[Vacant_Address==TRUE, .N, list(LONGITUDE, LATITUDE)]) + 
			aes(x = LONGITUDE, y = LATITUDE) +
	geom_point() + geom_density2d() 



# xlim(0.5, 6) + ylim(40, 110)


