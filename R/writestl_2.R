write_horizontal<- function(mat,x,y){
	coords <- list(c(x,y), c(x+1,y), c(x+1,y+1), c(x,y+1))
	valid_coords<-!is.na(map(coords,~mat[.[1],.[2]]))
	n_valid <- sum(valid_coords)
	if(n_valid ==4){
		#1,2,3
		print(coords[c(1,2,3)])
		#1,3,4
		print(coords[c(2,3,4)])
	} else if (n_valid == 3) {
		#write triangle
		#print(coords[valid_coords])
		#write diagonal wall
		hypotenuse <- list(c(4,2),
				   c(1,3),
				   c(2,4),
				   c(3,1))
		hyp_coords <- rev(coords[hypotenuse[[which(!valid_coords)]]])
		#top, top, bottom
		print(append(hyp_coords,hyp_coords[2]))
		#top, bottom, bottom
		print(append(hyp_coords,hyp_coords[1]))
	}
}


