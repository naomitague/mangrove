#' adaptation_comparison
#' 
#' compare and plot NPV of no adaptation and mangroves
#' @param baseline.surge baseline surges (before climate change) (m)
#' @param return.prob return probability for each surge in baseline.surge
#' @param climate.scenario an array that gives sea level rise (m) and
#' change in storm intensity (percent) per decade, multiple scenarios can be input 
#' @param mangrove.atten vector of mangrove attenuation coeffcients
#' @param decades vectors of decade names
#' @param discount discount rate 
#' @return list with means for noadapt, mangrove, mangrove.allK



adaptation_comparison = function(baseline.surge.by.return, return.prob, climate.scenario, mangrove.atten, decades, discount, plot.allK=T) {

ndecades =  length(decades)
surge.noadapt = compute_climatebased_surge(baseline.surge.by.return, climate.scenario, ndecades)
surge.mangrove = mangrove_adjustment_to_surge(surge.noadapt, mean(mangrove.atten))

# look at sensitivity for different values of the mangrove attenuation coefficient
surge.mangroveK = array(dim=c(dim(surge.noadapt), length(mangrove.atten)))
for (i in 1:length(mangrove.atten)) {
surge.mangroveK[,,,i]= mangrove_adjustment_to_surge(surge.noadapt, mangrove.atten[i])
}

surge.mangrove.meanK = apply(surge.mangroveK, c(1,2,3), mean)

# compute damages
damages.noadapt = surge_to_damage(surge.noadapt, surge.min=0.2, base=10000, K=7000)
damages.mangrove.meanK = surge_to_damage(surge.mangrove.meanK, surge.min=0.2, base=10000, K=7000)
damages.mangrove.K = surge_to_damage(surge.mangroveK, surge.min=0.2, base=10000, K=7000)

# estimate damages in each decade accounting for the probability of each surge height
MLE.damages.noadapt.bydecade = apply(damages.noadapt, c(2,3), weighted.mean, w=return.prob)*10
MLE.damages.mangrove.meanK.bydecade = apply(damages.mangrove.meanK, c(2,3), weighted.mean, w=return.prob)*10
MLE.damages.mangrove.K.bydecade = apply(damages.mangrove.K, c(2,3,4), weighted.mean, w=return.prob)*10
# make prettier names
rownames(MLE.damages.noadapt.bydecade) = as.character(decades)
colnames(MLE.damages.noadapt.bydecade) = rownames(climate.scenario)
rownames(MLE.damages.mangrove.meanK.bydecade) = as.character(decades)
colnames(MLE.damages.mangrove.meanK.bydecade) = rownames(climate.scenario)

# translate damages to NPV
# initialize using MLE arrays to get correct size
damages.noadapt.NPV = MLE.damages.noadapt.bydecade
damages.mangrove.K.NPV = MLE.damages.mangrove.K.bydecade
damages.mangrove.meanK.NPV = MLE.damages.mangrove.meanK.bydecade

for (i in 1:length(decades))  {
	damages.noadapt.NPV[i,] = compute_NPV(MLE.damages.noadapt.bydecade[i,],discount=discount, time=10*(i-1))
	damages.mangrove.meanK.NPV[i,] = compute_NPV(MLE.damages.mangrove.meanK.bydecade[i,],discount=discount, time=10*(i-1))
		for (j in 1:length(mangrove.atten)) {
			damages.mangrove.K.NPV[i,,j] = compute_NPV(MLE.damages.mangrove.K.bydecade[i,,j],discount=discount, time=10*(i-1))
	}
}

# sum up NPV for each decade to get total 50 year damaage estimate

total.damage.noadapt.NPV = apply(damages.noadapt.NPV, c(2), sum)	
total.damage.mangrove.meanK.NPV = apply(MLE.damages.mangrove.meanK.bydecade, c(2), sum)	
total.damage.mangrove.K.NPV = apply(MLE.damages.mangrove.K.bydecade, c(2,3), sum)	

# plots
# include climate scenario in a box plot
if (plot.allK==T)
boxplot(total.damage.noadapt.NPV, total.damage.mangrove.meanK.NPV, total.damage.mangrove.K.NPV,
names=c("No Adaption", "Mangrove","All K Mangrove"), ylab="Damages ($1000s)", col=c("blue","red","pink"))
else
boxplot(total.damage.noadapt.NPV, total.damage.mangrove.meanK.NPV,
names=c("No Adaption", "Mangrove"), ylab="Damages ($1000s)", col=c("blue","red"))

# now looks at average across all climate scenarios
mean.noadapt=mean(total.damage.noadapt.NPV)
mean.mangrove=mean(total.damage.mangrove.meanK.NPV)
mean.mangrove.allK =mean(total.damage.mangrove.K.NPV)

if(plot.allK==T)
 return(list(noadapt=mean.noadapt, mangrove=mean.mangrove, mangrove.allK=mean.mangrove.allK))
else 
return(list(noadapt=mean.noadapt, mangrove=mean.mangrove))
}

