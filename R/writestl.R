#' A Fucntion to write a matrix to an STL File
#'
#' @param STL a STL
#' @param ASCII whether the file should be written as ASCII or not
#' @param filename the name of the file to save out. A string.
#' @return None
#' @export
write_stl<- function(STL, filename, ASCII = TRUE){
	if (class(filename)!='character' | length(filename) != 1){
		stop('filename must be a string')
	}
	if (!ASCII){
		#writer <- binary_writer
		#con <- file(filename, open = 'wb')
		invisible()
	} else {
		writer <- ascii_writer
		con <- file(filename, open = 'w')
		write('solid', file=con)
	}

	scale_factor <- c(nrow(STL), ncol(STL), max(STL@top)) / STL@size

	top_constructor <- function(...){
		inputs <- as.list(substitute(...()))
		top_pass<- function(x,y){
			triangles <- map_dbl(inputs, eval, envir = environment()) %>%
				array(dim=c(3,3,2))
			if (!all(rev(triangles[3,,1]) == triangles[3,,2])){
				triangles %>%
					array_branch(3) %>%
					walk(writer, con, scale_factor)
			}
		}
		return(top_pass)
	}
	side_constructor <- function(...){
		inputs <- as.list(substitute(...()))
		side_pass<- function(x,y){
			triangles <- map_dbl(inputs, eval, envir = environment()) %>%
				array(dim=c(3,3,4)) %>%
				array_branch(3) %>%
				walk(function(triangle) {
					if (length(unique(triangle[,3]))>1){
  						writer(triangle, con, scale_factor)
					}
					})
			}
		return(side_pass)
		}

	pass_1 <- top_constructor(
				x, y+1 , STL@bot[x,y+1],
				x+1, y+1 , STL@bot[x+1,y+1],
				x, y , STL@bot[x,y],
				x, y , STL@top[x,y],
				x+1, y+1 , STL@top[x+1,y+1],
				x, y+1 , STL@top[x,y+1]
				)
	pass_2 <- top_constructor(
				x+1, y+1 , STL@bot[x+1,y+1],
				x+1, y ,  STL@bot[x+1,y],
				x, y , STL@bot[x,y],
				x, y , STL@top[x,y],
				x+1, y ,  STL@top[x+1,y],
				x+1, y+1 , STL@top[x+1,y+1]
				)
	pass_3 <- side_constructor(
				x,   1, STL@bot[x,1],
				x+1, 1, STL@bot[x+1,1],
				x+1, 1, STL@top[x+1,1],
				x,   1, STL@bot[x,1],
				x+1, 1, STL@top[x+1,1],
				x,   1, STL@top[x,1],
				x, ncol(STL) , STL@bot[x,ncol(STL)],
				x+1, ncol(STL) , STL@top[x+1,ncol(STL)],
				x+1, ncol(STL) , STL@bot[x+1,ncol(STL)],
				x, ncol(STL) , STL@bot[x,ncol(STL)],
				x,  ncol(STL) ,  STL@top[x,ncol(STL)],
				x+1, ncol(STL) , STL@top[x+1,ncol(STL)]
				)
	pass_4 <- side_constructor(
				1,y ,STL@bot[1,y],
				1,y+1 ,STL@top[1,y+1],
				1,y+1 ,STL@bot[1,y+1],
				1,y ,STL@bot[1,y],
				1,y ,STL@top[1,y],
				1,y+1 ,STL@top[1,y+1],
				nrow(STL), y ,STL@bot[nrow(STL),y],
				nrow(STL), y+1 ,  STL@bot[nrow(STL),y+1],
				nrow(STL), y+1 , STL@top[nrow(STL),y+1],
				nrow(STL), y ,  STL@bot[nrow(STL), y],
				nrow(STL), y+1 ,STL@top[nrow(STL),y+1],
				nrow(STL), y ,  STL@top[nrow(STL),y]
				)
	walk(seq(nrow(STL)-1), function(x) walk(seq(ncol(STL)-1),~pass_1(x,.)))
	walk(seq(nrow(STL)-1), function(x) walk(seq(ncol(STL)-1),~pass_2(x,.)))
	walk(seq(nrow(STL)-1), ~pass_3(.,1))
	walk(seq(ncol(STL)-1), ~pass_4(1,.))
	write('endsolid', file=con)
	close(con)
}

ascii_writer <-  function(triangle,write_file,sf){
		normal <- find_normal(triangle[,1],triangle[,2],triangle[,3])
		 list(
			sprintf('	facet normal %e %e %e',normal[1],normal[2],normal[1]),
			sprintf('		outer loop'),
			sprintf('			vertex %e %e %e',
				sf[1]*triangle[1,1], sf[2]*triangle[2,1],sf[3]*triangle[3,1]),
			sprintf('			vertex %e %e %e',
				sf[1]*triangle[1,2], sf[2]*triangle[2,2],sf[3]*triangle[3,2]),
			sprintf('			vertex %e %e %e',
				sf[1]*triangle[1,3], sf[2]*triangle[2,3],sf[3]*triangle[3,3]),
			sprintf('		endloop'),
			sprintf('	endfacet')
		) %>%
			walk(write, write_file)
}
