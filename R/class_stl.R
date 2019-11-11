setClass('STL',
	slots = c(top_surface = 'matrix',
		  bottom_surface = 'matrix',
		  dimensions = 'matrix'),
	validity = function(object){
		mode(object@dimensions) == 'numeric' || stop('dimensions must be a numeric matrix')
		ncol(object@dimensions) == 3 || stop('dimensions must be of length 3')

		mode(object@top_surface) == "numeric" || stop('top_surface should be a numeric matrix')

		mode(object@bottom_surface) == "numeric" || stop('top_surface should be a numeric matrix')

		return(TRUE)
	}
)

