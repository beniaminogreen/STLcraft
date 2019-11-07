check_stl <- function(object){
	errors <- character()
	dims <- length(object@dimensions)
	if (dims != 3){
		msg <- paste0('Dimensions are length: ', dims, ' Should be 3')
		errors <- c(errors, msg)
	}
	if (length(errors) == 0) TRUE else errors
}

STL <- setClass('STL', representation(top_surface = 'matrix', dimensions = 'list', colors = 'matrix'),validity = check_stl)

setMethod('show',
	  'STL',
	  function(object){
		  cat(sprintf('An STL with %s triangles:\n',2*nrow(object@top_surface)+2*ncol(object@top_surface)+2*ncol(object@top_surface)*nrow(object@top_surface)))
		  cat(sprintf('size       : %s by %s by %s (x,y,z) \n', object@dimensions[[1]], object@dimensions[[2]], object@dimensions[[3]]))
		  cat(sprintf('dimensions : %s by %s (nrow, ncol)\n', nrow(object@top_surface), ncol(object@top_surface)))
		  cat(sprintf('values     : %s, %s (max, min)', max(object@top_surface),min(object@top_surface)))
	  }
	  )

