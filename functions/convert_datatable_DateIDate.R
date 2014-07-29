convert_datatable_DateIDate <- function(dat, cols=NULL){
	# browser()}
	
	if(is.null(cols)){
		col_classes <- sapply(dat, class)
		col_classes_match <- lapply(col_classes, grep, pattern="POSIX")
		cols <- which(sapply(col_classes_match, length) != 0)
	}
	
	if(length(cols)==0){
		warning("No POSIX dates to convert")
		invisible(dat)
	} else {
		for(col in cols){
			set(dat, j=col, value=as.IDate(dat[[col]]))
		}
		invisible(dat)
	}
}

