pass_constructor<- function(df,temp_file, ...){
	relationships <- as.list(substitute(...()))
	pass <- function(x,y){
		points <- lapply(relationships, eval, envir = environment())
		normal_1 <-find_normal(as.numeric(points$v_1), as.numeric(points$v_2), as.numeric(points$v_3))
		normal_2 <-find_normal(as.numeric(points$v_4), as.numeric(points$v_5), as.numeric(points$v_6))
		output_lines <- list(
			sprintf('	facet normal %e %e %e', normal_1[1],normal_1[2],normal_1[3]),
			sprintf('		outer loop'),
			sprintf('			vertex %e %e %e', points$v_1[1], points$v_1[2],points$v_1[3]),
			sprintf('			vertex %e %e %e', points$v_2[1], points$v_2[2],points$v_2[3]),
			sprintf('			vertex %e %e %e', points$v_3[1], points$v_3[2],points$v_3[3]),
			sprintf('		endloop'),
			sprintf('	endfacet'),
			sprintf('	facet normal %e %e %e', normal_2[1],normal_2[2],normal_2[3]),
			sprintf('		outer loop'),
			sprintf('			vertex %e %e %e', points$v_4[1], points$v_4[2],points$v_4[3]),
			sprintf('			vertex %e %e %e', points$v_5[1], points$v_5[2],points$v_5[3]),
			sprintf('			vertex %e %e %e', points$v_6[1], points$v_6[2],points$v_6[3]),
			sprintf('		endloop'),
			sprintf('	endfacet')
		)
		invisible(Map(write,output_lines,temp_file))
	}
	return(pass)
}

#' A Fucntion to write a matrix to an STL File
#'
#' @param df a matrix or matrix-like object
#' @param filename A the name of the file to save out. A srting.
#' @return None
#' @export
write_stl<- function(df, filename){
	if (class(filename)!='character' | length(filename) != 1){
		stop('filename must be a string')
	}

	temp_file <- file(filename, open = 'w')
	write('solid', file=temp_file)

	pass_1 <- pass_constructor(df, temp_file,
				v_1=c(x,   y, 0),
				v_2=c(x+1, y, 0),
				v_3=c(x+1, y, df[x+1,y+1]),
				v_4=c(x,   y, 0),
				v_5=c(x+1, y, df[x+1,y+1]),
				v_6=c(x,   y, df[x+1,y+1])
				)

	pass_2 <- pass_constructor(df, temp_file,
				v_1=c(x,y ,0),
				v_2=c(x,y+1 ,df[x+1,y+1]),
				v_3=c(x,y+1 ,0),
				v_4=c(x,y ,0),
				v_5=c(x,y ,df[x+1,y]),
				v_6=c(x,y+1 ,df[x+1,y+1])
				)
	pass_3 <- pass_constructor(df, temp_file,
				v_1=c(x, y , 0),
				v_2=c(x+1, y , df[x+1,y]),
				v_3=c(x+1, y , 0),
				v_4=c(x, y , 0),
				v_5=c(x,  y ,  df[x,y]),
				v_6=c(x+1, y , df[x+1,y])
				)
	pass_4 <- pass_constructor(df, temp_file,
				v_1 =c(x, y ,0),
				v_2 =c(x, y+1 , z2 = 0),
				v_3 =c(x, y+1 , z3 = df[x,y+1]),
				v_4 =c(x, y , z1 = 0),
				v_5 =c(x, y+1 , z4 = df[x,y+1]),
				v_6 =c(x, y , z5 = df[x,y])
				)
	pass_5 <- pass_constructor(df, temp_file,
				v_1 =c(x, y , df[x,y]),
				v_2 =c(x+1, y ,  df[x+1,y]),
				v_3 =c(x+1, y+1 , df[x+1,y+1]),
				v_4 =c(x, y , df[x,y]),
				v_5 =c(x+1, y+1 , df[x+1,y+1]),
				v_6 =c(x, y+1 , df[x,y+1])
				)
	pass_6 <- pass_constructor(df, temp_file,
				v_1=c(x, y , 0),
				v_2=c(x+1, y , 0),
				v_3=c(x+1, y+1 , 0),
				v_6=c(x, y+1 , 0),
				v_5=c(x+1, y+1 , 0),
				v_4=c(x, y , 0)
				)

	walk(seq(nrow(df)-1), ~pass_1(.,1))
	walk(seq(ncol(df)-1), ~pass_2(1,.))
	walk(seq(nrow(df)-1), ~pass_3(.,ncol(df)))
	walk(seq(ncol(df)-1), ~pass_4(nrow(df),.))
	walk(seq(nrow(df)-1), function(x) walk(seq(ncol(df)-1),~pass_5(x,.)))
	walk(seq(nrow(df)-1), function(x) walk(seq(ncol(df)-1),~pass_6(x,.)))
	write('endsolid', file=temp_file)
}
