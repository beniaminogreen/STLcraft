#' Constructor function to arrange STLS
#' @param a named vector of colors / STLS
#' @export
Arrangement <- function(...){
	inputs <- list(...)
	colors <- inputs %>%
		map_chr(~.@color) %>%
		unique()

	new('Arrangement',
	    STLS = inputs,
	    size = inputs[[1]]@size,
	    dimensions = dim(inputs[[1]]),
	    colors = colors
	)
}
