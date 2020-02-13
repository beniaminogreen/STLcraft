#' Seperate an STL into several colors / bodies by logical rules
#'
#' @param STL an STL object
#'
#' @param ... A series of two-sided formulas. As with dplyr's case_when function,
#' 	the left-hand-side (LHS) determines which values match this case. The right-hand-side
#' 	determines which STL the column should be separated into.
#'
#' 	formulas can have references to 'top', 'x', 'y', 'z' and 'bot.' 'z' is shorthand for 'top',
#' 	which refers to the z value of the top surface at any point. 'x' and 'y' refer to the x and y coordinates
#' 	at any point, and 'bot' refers to the z value of the bottom surface.
#'
#' @export
case_vpartition<- function(STL,...){
	formulas <- list(...)
	selector <- matrix('NA', nrow = nrow(STL), ncol = ncol(STL))
	enclos <- parent.frame()

	for(x in seq(ncol(STL))){
		for(y in seq(nrow(STL))){
			z <- top <- STL[y,x]
			for(form in formulas){
				result <- eval(form[[2]], enclos = enclos )
				if(result){
					selector[y,x] <- eval(form[[3]], enclos=enclos)
					break
				}
			}
			}
		}
	return(selector) #currently returns selection matrix
}
