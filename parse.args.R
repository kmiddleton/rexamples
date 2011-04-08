hi <- function(...){
	args <- list(...)
	if (!is.null( args$person )){
		paste( "hi", args$person )
	} else {
		paste( "nobody here" )
	}
}

hi("hans-peter")
hi(person="hans-peter")
