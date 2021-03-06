setClass('Arrangement',
	slots = c(STLS = 'list',
		dimensions='numeric',
		size = 'numeric',
		colors = 'character'),
	validity = function(object){
		if(length(object@dimensions) != 2 ){
			 stop('dimensions must be of length 3')
		}

		if(mode(object@dimensions) != 'numeric' ){
			 stop('dimensions must be numeric')
		}

		if(length(object@colors) > length(object@STLS)){
			stop("There can't be more colors than STL's")
		}

		if(!inherits(object@STLS, "list") ){
			stop('STLS are not a list')
		}

		if(!all(object@STLS %>% map(class) == 'STL')){
			stop('All STLS must be of class STL')
		}

		if(length(unique(object@STLS %>% map(~.@size))) != 1  ){
			stop('All STLs must have the same size')
		}

		if(length(unique(object@STLS %>% map(dim))) != 1 ){
			stop('All STLs must have the same dimensions')
		}


		return(TRUE)
	}
)
