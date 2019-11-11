setClass('print_arrangement',
	 slots = c(STLS = 'list',dimensions='matrix'),
	 validity = function(object){
		 ncol(object@dimensions) == 3 || stop('dimensions must be of length 3')
		 mode(object@dimensions) == 'numeric' || stop('dimensions must be a numeric matrix')

		 inherits(object@STLS, "list") || stop('STLS are not a list')

		 return(TRUE)
	 }
 )
