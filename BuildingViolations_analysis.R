

## Trying to make an indicator matrix (see example)
## Analyzing frequency and sequence of citations


# > ex
# [[1]] character(0)
# [[2]]  "13-196-530" "13-196-641"
# [[3]]  "13-196-100" "13-196-160"
# [[4]]  "13-196-580" "13-196-630"
# [[5]]  "13-196-630" "13-196-641"
# [[6]]  "13-192-760" "13-12-100" 
# [[7]]  "13-196-530" "13-196-540" "13-196-630"
# [[8]]  "7-28-060"
# [[9]]  "13-196-540"
# [[10]] "13-196-550" "13-196-641"
# 
# > Universe
# [1] "13-196-530" "13-196-641" "13-196-100" "13-196-160" "13-196-580" 
# [6] "13-196-630" "13-192-760" "13-12-100" "13-196-540" "7-28-060" "13-196-550"
# 
# > mat
# [,1]  [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11]
# [1,]     0    0    0    0    0    0    0    0    0     0     0
# [2,]     1    1    0    0    0    0    0    0    0     0     0
# [3,]     0    0    1    1    0    0    0    0    0     0     0
# [4,]     0    0    0    0    1    1    0    0    0     0     0
# [5,]     0    1    0    0    0    1    0    0    0     0     0
# [6,]     0    0    0    0    0    0    1    1    0     0     0
# [7,]     1    0    0    0    0    1    0    0    1     0     0
# [8,]     0    0    0    0    0    0    0    0    0     1     0
# [9,]     0    0    0    0    0    0    0    0    1     0     0
# [10,]    0    1    0    0    0    0    0    0    0     0     1


##------------------------------------------------------------------------------
## INITIALIZATION
##------------------------------------------------------------------------------
rm(list=ls())
gc()
library(geneorama)
detach_nonstandard_packages()
library(geneorama)
loadinstall_libraries(c("geneorama", "data.table", "reshape2", "Matrix"))

sourceDir("functions/")
op <- readRDS("data/op.Rds")  ## Default par
par(op)

##------------------------------------------------------------------------------
## Load data for vacant buildings and violations
##------------------------------------------------------------------------------
datBuild <- readRDS("data/20140507/Building_Violations.Rds")
VIOLATION_ORDINANCE <- readRDS(
	"data/20140507/Building_Violations__VIOLATION_ORDINANCE.Rds")
VIOLATION_ORDINANCE_DM <- readRDS(
	"data/20140507/Building_Violations__VIOLATION_ORDINANCE_dummy_matrix.Rds")
## Convert Names
setnames(datBuild, 
		 old = colnames(datBuild), 
		 new = gsub("\\.", "_", colnames(datBuild)))

##------------------------------------------------------------------------------
## CREATE UNIVERSE OF VIOLATIONS
## CREATE UNIVERSE OF VIOLATION ENTRIES
##------------------------------------------------------------------------------
Universe <- unique(unlist(VIOLATION_ORDINANCE))
# Universe

Universe_ViolationCode <- unique(datBuild$VIOLATION_CODE)
Universe_ViolationDesc <- unique(datBuild$VIOLATION_DESCRIPTION)
Universe_ViolationOrd <- unique(datBuild$VIOLATION_ORDINANCE)

# Universe_ViolationCode
# Universe_ViolationDesc
# Universe_ViolationOrd


## Work to answer Hugh's original questions:
if(FALSE){
	## Summary of unique violation codes 
	# wtf(datBuild[ , .N, list(VIOLATION_CODE, VIOLATION_DESCRIPTION, VIOLATION_ORDINANCE)])
	
	temp3 = vector("numeric", ncol(VIOLATION_ORDINANCE_DM))
	for(j in 1:ncol(VIOLATION_ORDINANCE_DM)) {
		temp3[j]=sum(VIOLATION_ORDINANCE_DM[,j])
	}
	jj = order(temp3, decreasing=TRUE)
	
	# clipper(Universe[jj])
	# clipper(temp3[jj])
	
	temp = VIOLATION_ORDINANCE_DM[1:1e4, jj]
	temp = as.matrix(temp)
	colnames(temp) <- Universe[jj]
	# wtf(temp)
	temp2 = datBuild$VIOLATION_ORDINANCE[1:1e4]
	# wtf(temp2)
	
	ViolationDescriptions
}



length(Universe_ViolationCode)
length(Universe_ViolationDesc)
length(Universe_ViolationOrd)

# VIOLATION_ORDINANCE_LEN <- sapply(VIOLATION_ORDINANCE, length)

##------------------------------------------------------------------------------
## CONVERTING UNIVERSE TO DUMMY MATRIX
##------------------------------------------------------------------------------
ex <- VIOLATION_ORDINANCE
length(ex)
nrow(datBuild)

Universe <- unique(unlist(ex))
length(Universe)

library("Matrix")

ex_collapse <- vector("character", length(ex))
for(i in 1:length(ex)) {
	ex_collapse[i] <- paste(ex[[i]], collapse = ", ")
}

## Similar example to sparse example above, but 
## now I'm looking at each column at once, to take
## advantage of vectorization
i = 1
mat <- Matrix(0, nrow=length(ex), ncol=length(Universe), sparse=TRUE)
for(j in 1:length(Universe)){
	if(j %% 10 ==0) print(j)
	ii <- grep(Universe[j], ex_collapse)
	if(length(ii) > 0){
		mat[ii , j] <- 1
	}
}
sum(mat)
object.size(mat)/1e6


sums = vector("numeric", ncol(mat))
for(j in 1:ncol(mat)) sums[j] = sum(mat[,j])
plot(sort(sums, decreasing=T), log="y")
mat_small = mat[1:100,sums>1000]
dim(mat_small)
prcomp(mat_small)

##------------------------------------------------------------------------------
## ANALYSIS OF DISTRIBUTION OF UNIVERSE
##------------------------------------------------------------------------------

## Analysis of frequency of VIOLATION_ORDINANCE
tab <- table(unlist(VIOLATION_ORDINANCE))
hist(log10(tab), 20)
str(tab[tab<10])
str(tab[tab>10])

## The fact that these two tables are approximately the same size 
## suggests that the same citations are usually given in the same order
dim(table(ex_collapse))
dim(table(Universe))

## The fact that the same sequences found in 2006 are also found in 2011
## suggests that not only are the same citations given in the same order, 
## but also that the order doesn't change much through time.
table(year(datBuild$VIOLATION_DATE))
ex_collapse_2006 = ex_collapse[year(datBuild$VIOLATION_DATE)==2006]
ex_collapse_2010 = ex_collapse[year(datBuild$VIOLATION_DATE)==2012]
inin(ex_collapse_2006, ex_collapse_2010)







