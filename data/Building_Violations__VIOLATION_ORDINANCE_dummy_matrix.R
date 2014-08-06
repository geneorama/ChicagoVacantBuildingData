

## Make an indicator matrix
## Analyzing frequency and sequence of citations


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
## Load data for vacant buildings and violations
##------------------------------------------------------------------------------
VIOLATION_ORDINANCE <- readRDS(
	"data/20140507/Building_Violations__VIOLATION_ORDINANCE.Rds")

##------------------------------------------------------------------------------
## CREATE AND CONVERT UNIVERSE
## CREATION: This is a vector of every ordinance ordinance code 
## (e.g. "13-196-530" or "13-196-641") found in our ordinance data, which
## was extracted from the field "violation ordinance" in the building 
## violation data
## CONVERSION: Convert into a giant indiactor matrix, organized by each row of 
## the building violation data, which corresponds to a single violation.
##------------------------------------------------------------------------------

## VIOLATION_ORDINANCE is currently a list
str(VIOLATION_ORDINANCE[1:10])

## Get all unique violation codes
Universe <- unique(unlist(VIOLATION_ORDINANCE))
length(VIOLATION_ORDINANCE)
length(Universe)

## Collapse VIOLATION_ORDINANCE using paste
VIOLATION_ORDINANCE_collapse <- vector(mode = "character", 
									   length = length(VIOLATION_ORDINANCE))
for(i in 1:length(VIOLATION_ORDINANCE_collapse)) {
	VIOLATION_ORDINANCE_collapse[i] <- paste(VIOLATION_ORDINANCE[[i]], 
											 collapse = ", ")
}

## Look at each column at once, to take advantage of vectorization
mat <- Matrix(data = 0, 
			  nrow = length(VIOLATION_ORDINANCE_collapse), 
			  ncol = length(Universe), 
			  sparse = TRUE)
for(j in 1:length(Universe)){
	# if(j %% 10 ==0) print(j)
	print(j)
	ii <- grep(Universe[j], VIOLATION_ORDINANCE_collapse)
	if(length(ii) > 0){
		mat[ii , j] <- 1
	}
}
sum(mat)
print(object.size(mat), units="Mb")

##------------------------------------------------------------------------------
## SAVE THE SPARSE MATRIX OF ORDINANCE VIOLATION
##------------------------------------------------------------------------------
saveRDS(mat, "data/20140507/Building_Violations__VIOLATION_ORDINANCE_dummy_matrix.Rds")



##------------------------------------------------------------------------------
## CONVERT SPARSE MATRIX TO DATA TABLE AND SAVE
## This object is very large (although it doesn't take up much disk space), so
## it may not be feasible to work with.
##------------------------------------------------------------------------------

## Read from disk if needed
# mat <- readRDS("data/20140507/Building_Violations__VIOLATION_ORDINANCE_dummy_matrix.Rds")

mat_dt <- matrix(NA, nrow=nrow(mat), ncol=ncol(mat))
for(i in 1:ncol(mat_dt)){
	if(i %% 10 == 0) print(i)
	mat_dt[ , i] <- as.matrix(mat[, i])
}
lll()
mat_dt <- as.data.table(mat_dt)
lll()

rownames(mat_dt) <- rownames(mat)
setnames(mat_dt, colnames(mat_dt), colnames(mat))

saveRDS(mat_dt, "data/20140507/Building_Violations__VIOLATION_ORDINANCE_dummy_matrix_dt.Rds")


