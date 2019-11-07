make_bar<- function(spolydf, field, nrows = 500, ncols = 500){
	r <- raster(ncols=ncols, nrows=nrows)
	extent(r) <- extent(spolydf)
	rast <- rasterize(spolydf,r, field = field)
	matr <- as.matrix(rast)
	matr[is.na(matr)] <- 0
}

