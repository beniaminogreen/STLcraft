n_neighbors <- function(mat){
	neighbor_mat <- matrix(0,nrow = nrow(mat), ncol = ncol(mat))
	for (x in seq(nrow(mat))){
		for (y in seq(ncol(mat))){
			n <- 0
			for (dx in -1:1){
				for (dy in -1:1){
					row_index <- x+dx
					col_index <- y+dy
					if (row_index > nrow(mat) | col_index > ncol(mat)) next
					if (row_index < 1 | col_index < 1) next
					if (row_index == x & col_index == y) next
					if(!is.na(mat[row_index,col_index])){
						n <- n + 1
					}
				}
			}
			neighbor_mat[x,y] <- n
		}
	}
	return(neighbor_mat)
}
