in_bounds<- function(mat,point){
	if(point[1]<1 | point[1]>nrow(mat) | point[2]<1 | point[2]>ncol(mat)) {
		return(FALSE)
	}
	return(TRUE)
}
check_valid<- function(mat,x,y){
	offsets <- list(c(-1,1),c(0,1),c(1,1),
			c(1,0),c(1,-1),c(0,-1),
			c(-1,-1),c(-1,0),c(-1,1))
	for(i in 1:8){
		point_1<-offsets[[i]] + c(x,y)
		point_2<-offsets[[i+1]] + c(x,y)
		if(!in_bounds(mat,point_1) | !in_bounds(mat,point_2)){
			next
		}

		space_1 <- mat[offsets[[i]][1] + x,offsets[[i]][2]+y]
		space_2 <- mat[offsets[[i+1]][1] + x,offsets[[i+1]][2]+y]
		if(!is.na(space_1) & !is.na(space_2)){
			return(TRUE)
		}
	}
	return(FALSE)
}
