#' mangrove_adjustment_to_surge
#' 
#' compute the new surge after mangroves
#' @param surge (m) original surge depth
#' @param attenK (percent) attenuation coefficient
#' @return new surge depth (m)



mangrove_adjustment_to_surge = function(surge, attenK) {
	new.surge = (1-attenK)*surge
	new.surge = pmax(new.surge,0)
	return(new.surge)
}

