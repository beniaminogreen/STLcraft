#' Constructor function to arrange STLS
#' @param a nemaed vecrtor of colors / STLS
#' @export
Arrangement <- function(...){
	inputs <- list(...)
	colors <- inputs %>%
		map_chr(~.@color) %>%
		unique()

	new('Arrangement',
	    STLS = inputs,
	    dimensions = inputs[[1]]@size,
	    colors = colors
	)
}

