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
n_neighbors <- function(x,y,mat){
	count = 0
	for(dx in -1:1){
		if(x+dx>0 & x+dx<=nrow(mat)){
			for(dy in -1:1){
				if(y+dy>0 & y+dy<=ncol(mat)){
					if (!is.na(mat[x+dx,y+dy]) & !(dy == 0 & dx == 0)){
						count <- count +1
					}
				}
			}
		}
	}
	return(count)
}
neighbor_counter<- function(mat){
	count_matrix <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
	for (x in 1:nrow(mat)){
		for(y in 1:ncol(mat)){
			if(!is.na(mat[x,y])){
				count_matrix[x,y] <- n_neighbors(x,y,mat)
			}
		}
	}
	return(count_matrix)
}

matrix(c(NA,NA,NA,12,
	NA,NA,12,12,
	NA,12,NA,NA),
      nrow=3, byrow=TRUE)
