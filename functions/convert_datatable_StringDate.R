convert_datatable_StringDate <- function(dat, cols, fmt=NULL, tz="GMT"){
	# browser()
	# as.POSIXct(strptime(dat[[cols]][1], "%m/%d/%Y"), tz="GMT")
	for(col in cols){
		set(x = dat, j = col, value = ifelse(dat[[col]]=="", NA, dat[[col]]))
		if(is.null(fmt)){
			set(x = dat, 
				j = col, 
				value = as.POSIXct(dat[[col]], tz=tz))
		} else {
			set(x = dat, 
				j = col, 
				value = as.POSIXct(strptime(x = dat[[col]],
											format = fmt),
								   tz = tz))
		}
	}
	invisible(dat)
}

