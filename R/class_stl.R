setClass('STL',
	slots = c(top = 'matrix',
		bot = 'matrix',
		size = 'numeric',
		color = 'character'
		),
	validity = function(object){
		if(length(object@size) != 3 ){
			stop('the size must be specified in 3 dimensions')
		} else if(length(object@color) != 1 ){
			stop('color must be a string')
		} else if(!identical(dim(object@top),dim(object@bot))){
			stop('Top and Bottom surface must have same size')
		} else if(mode(object@top) != "numeric" ){
			stop('Top surface should be a numeric matrix')
		} else if(mode(object@bot) != "numeric" ){
			stop('Bottom surface should be a numeric matrix')
		} else if(any(object@bot > object@top)){
			stop('Bottom surface may never be above top surface')
		}

		return(TRUE)
	}
)
