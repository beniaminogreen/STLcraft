setMethod('show',
	  'STL',
	  function(object){
		  cat(sprintf('An STL with %s triangles:\n',2*nrow(object@top_surface)+2*ncol(object@top_surface)+2*ncol(object@top_surface)*nrow(object@top_surface)))
		  cat(sprintf('size       : %s by %s by %s (x,y,z) \n', object@dimensions[[1]], object@dimensions[[2]], object@dimensions[[3]]))
		  cat(sprintf('dimensions : %s by %s (nrow, ncol)\n', nrow(object@top_surface), ncol(object@top_surface)))
		  cat(sprintf('values     : %s, %s (max, min\n)', max(object@top_surface),min(object@top_surface)))
	  }
	  )

