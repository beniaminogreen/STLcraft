#' A function to find the cross product of an arbitrary number of vectors
#'
#' @param ... a list of vectors
#' @return A numeric vector
xprod <- function(...){
	vectors <- list(...)

	len <- unique(map_dbl(vectors,length))
		(length(len) == 1) || stop("Vectors Must be of Same Length")

	m <- invoke(rbind, vectors)

	sapply(seq(len), function(i)
		det(m[,-i,drop=FALSE]) * (-1)^(i+1)
	)
}

#' A function to find the unit vector for a given vector
#'
#' @param ... a numeric vector
#' @return A numeric vector
normalize_vector<- function(vect){
	is.numeric(vect) || stop('vector not numeric')
	len <- sum(vect^2)^.5
	return(vect/len)
}

#' A function to find the vector normal to a triangle
#' from three given vertexes
#'
#' @param vertex_1 A triangle vertex
#' @param vertex_2 A triangle vertex
#' @param vertex_3 A triangle vertex
#' @return A numeric vector
find_normal<- function(vertex_1, vertex_2, vertex_3){
	(length(vertex_1)== 3 & length(vertex_2)== 3 & length(vertex_3)== 3) ||
		stop('vertexes must all have 3 points')
	(class(vertex_1) == 'numeric' & class(vertex_2) == 'numeric' & class(vertex_3) == 'numeric')||
		stop('all arguments must be numeric vectors')

	vert_t1 <- vertex_1 - vertex_2
	vert_t2 <- vertex_1 - vertex_3
	return(normalize_vector(xprod(vert_t1, vert_t2)))
}
