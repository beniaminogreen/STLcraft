library(purrr)
setClass('triangle',
	 slots = c(vertex_1 = "list",
		   vertex_2 = "list",
		   vertex_3 = "list"),
	 validity = function(object){
		 (length(object@vertex_2) == 3 & length(object@vertex_2) == 3  & length(object@vertex_3) == 3) ||
			 stop('vertexes must be of length 3')
		 TRUE
	}
)
triangle <- function(vert_1, vert_2, vert_3){
	vert_1 <- as.list(substitute(vert_1))[-1]
	vert_2 <- as.list(substitute(vert_2))[-1]
	vert_3 <- as.list(substitute(vert_3))[-1]

	new('triangle',vertex_1 =  vert_1, vertex_2 = vert_2, vertex_3 = vert_3)
}

eval_tri <- function(triangle){
	map(triangle@vertex_1,eval, envir = parent.frame())
}

test <- triangle(c(a+1, b+2, d+3), c(a+1, b+2, d+3), c(a+1, b+2, d+3))
a <- 4
b <- 2
d <- 3

eval_tri(test)
