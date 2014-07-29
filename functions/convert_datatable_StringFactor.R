convert_datatable_StringFactor <- function(dat, cols){
	for(col in cols){
		set(dat, j=col, value=as.factor(dat[[col]]))
	}
	invisible(dat)
}

