library(purrr)
#'A function to find the cross product of an arbirtrary number of vectors
#'
#'@param ... a list of vectors
#'@return A numeric vector
#'@export
xprod <- function(...){
	vectors <- list(...)

	len <- unique(map_dbl(vectors,length))
	if (length(len) != 1)
		stop("Vectors Must be of Same Length")

	m <- invoke(rbind, vectors)
	map_dbl(seq(len),
		 function(i) det(m[,-i,drop=FALSE]) * (-1)^(i+1)
		 )
}

#' A function to find the vector normal to a triangle
#' from three given vertexes
#'
#' @param vertex_1 A triangle vertex
#' @param vertex_2 A triangle vertex
#' @param vertex_3 A triangle vertex
#' @return A numeric vector
find_normal<- function(vertex_1, vertex_2, vertex_3){
	if (length(vertex_1)!= 3 | length(vertex_2)!= 3 | length(vertex_3)!= 3){
		stop('vertexes must all have 3 points')
	}
	if (class(vertex_1) != 'numeric'|class(vertex_2) != 'numeric'|class(vertex_3) != 'numeric'){
		stop('all arguments must be numeric vectors')
	}
	vert_t1 <- vertex_1 - vertex_2
	vert_t2 <- vertex_1 - vertex_3
	return(xprod(vert_t1, vert_t2))
}
