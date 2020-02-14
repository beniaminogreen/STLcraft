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

write_triangle <-  function(point_1,point_2,point_3,write_file){
		normal <- find_normal(point_1, point_2, point_3)
		write(sprintf('	facet normal %e %e %e',normal[1],normal[2],normal[1]),write_file)
		write(sprintf('		outer loop'),write_file)
		write(sprintf('			vertex %e %e %e',
			point_1[1],point_1[2],point_1[3]), write_file)
		write(sprintf('			vertex %e %e %e',
			point_2[1],point_2[2],point_2[3]), write_file)
		write(sprintf('			vertex %e %e %e',
			point_3[1],point_3[2],point_3[3]), write_file)
		write(sprintf('		endloop'),write_file)
		write(sprintf('	endfacet'),write_file)
}

