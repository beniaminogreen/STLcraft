setClass('Arrangement',
	slots = c(STLS = 'list',
		dimensions='numeric',
		size = 'numeric',
		colors = 'character'),
	validity = function(object){
		length(object@dimensions) == 2 || stop('dimensions must be of length 3')
		mode(object@dimensions) == 'numeric' || stop('dimensions must be numeric')

		length(object@colors) <= length(object@STLS) || stop('dimensions must be of length 3')

		inherits(object@STLS, "list") || stop('STLS are not a list')

		all(object@STLS %>% map(class) == 'STL') || stop('All STLS must be of class STL')
		length(unique(object@STLS %>% map(~.@size))) == 1  || stop('All STLs must have the same size')
		length(unique(object@STLS %>% map(dim))) == 1 || stop('All STLs must have the same dimensions')

		return(TRUE)
	}
)
