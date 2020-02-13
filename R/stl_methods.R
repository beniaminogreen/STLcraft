#' @export
STL <- function(top, bot = NULL, size = c(1,1,1), color = 'none') {
	if(is.null(bot)){
		bot <- matrix(0L, nrow=nrow(top), ncol=ncol(top))
	}
	new("STL",top = top, bot = bot,size = size, color = color)
}

setMethod('show',
	  'STL',
	  function(object){
		  nrows <- nrow(object@top)
		  ncols <- ncol(object@top)
		  cat(sprintf('size       : %s by %s by %s (x,y,z) \n',
			      object@size[1], object@size[2], object@size[3]))
		  cat(sprintf('size : %s by %s (nrow, ncol)\n',
			      nrows, ncols))
		  cat(sprintf('values     : %s, %s (max, min) \n',
			      max(object@top),min(object@top)))
	}
)

dim.STL <- function(object) dim(object@top)
nrow.STL <- function(object) nrow(object@top)
ncol.STL <- function(object) ncol(object@top)
