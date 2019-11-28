matrix_cleaner<- function(mat){
	output_matrix <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
	for(x in seq(nrow(mat)-1)){
		for(y in seq(ncol(mat)-1)){
			pts <- matrix(c(x,y,mat[x,y],
				x+1,y,mat[x+1,y],
				x+1,y+1,mat[x+1,y+1],
				x,y+1,mat[x,y+1]
				 ), nrow =4 , byrow=TRUE)
			n_valid <- sum(!is.na(pts[,3]))
			if (n_valid >2 ) {
				pts <- pts[!is.na(pts[,3]),]
				for(point_num in seq(nrow(pts))){
					point <- pts[point_num,]
					output_matrix[point[1], point[2]] <- point[3]
				}
			}
		}
	}
	output_matrix
}
