convert_datatable_int_to_num <- function(dat, col_skip = c('id')){
	#browser()}
	require(data.table)
	## Convert intergers to numeric
	names_integer <- names(dat[ , which(sapply(dat, class) == 'integer') ])
	
	for (col in names_integer){
		if(! col %in% col_skip){
			set(dat, j = col, value = as.numeric(dat[[col]]))
		}
	}
	
	## We don't need to return the data table because it is modified in place
	## But we reutrn it invisibly for use in Rmd files (where it makes sense
	## to do the assignment so that it doesn't get printed)
	invisible(dat)
	
}


