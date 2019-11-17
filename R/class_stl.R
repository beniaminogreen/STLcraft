setClass('STL',
	slots = c(top = 'matrix',
		  bot = 'matrix',
		  size = 'numeric',
		  color = 'character'
		  ),
	validity = function(object){
		length(object@size) == 3 || stop('the size must be specified in 3 dimensions')

		length(object@color) == 1 || stop('color must be a string')

		dim(object@top) == dim(object@bot) || stop('Top and Bottom surface must have same size')
		mode(object@top) == "numeric" || stop('Top surface should be a numeric matrix')
		mode(object@bot) == "numeric" || stop('Bottom surface should be a numeric matrix')
		!any(object@bot > object@top) || stop('Bottom surface may never be above top surface')

		return(TRUE)
	}
)
