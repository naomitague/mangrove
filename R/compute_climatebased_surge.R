
#' compute climate based surge
#' 
#' computes surge for future decades given climate change scenarios
#' as a function of previous surge and climate change effects on sea 
#' level rise and storm intensity
#' 
#' @param baseline.surge baseline surges (before climate change) (m)
#' @param climate.scenario an array that gives sea level rise (m) and
#' change in storm intensity (percent) per decade, multiple scenarios can be input 
#' @param ndecades number of decades

#' @return surge (m)

compute_climatebased_surge = function(baseline.surge, climate.scenario, ndecades) {
	
nscenarios = nrow(climate.scenario)

surge = array(dim=c(length(baseline.surge), ndecades, nscenarios) )

for (decadei in 1:ndecades) {

for (scenarioi in 1:nscenarios) {

		for (return_intervali in 1:length(baseline.surge)) {

				# for a given return interval storm, add intensity increase, and sea level rise	

				surge[return_intervali, decadei,  scenarioi] =
					(1 + climate.scenario$ssf[scenarioi]*decadei)*  
					baseline.surge[return_intervali] +
					 climate.scenario$slr[scenarioi]*decadei }
		}
	}

return(surge)
}


