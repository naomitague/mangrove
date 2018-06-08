#' surge_to_damage
#' 
#' Function to compute the costs of damages as a function of surge 
#' height
#' @param surge (m)
#' @param surge.min (m) minimum surge that causes damage
#' @param base ($) costs associated with any surge above the minimum
#' @param K ($/m) slope of the surge/damage linear relationship
#' 
#' @return damage in $


surge_to_damage2 = function(surge, surge.min, base, K) {

	flood = ifelse(surge > surge.min, surge-surge.min, 0)
	damage = ifelse(surge > surge.min, K*flood+base, 0)
	return(damage)
	}

