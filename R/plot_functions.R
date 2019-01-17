#### making graph of exposure to bite

#' making multi panel plot of exposure to bite (one panel per entomological survey for one village and one age classes)
#'
#' @param Exposure a dataframe obtained by the use of function `exposure_dat`
#' @param vil one village code (string) from Exposure$Vil
#' @param age one age classes (string) from Exposure$Age
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' Exposure <- exposure_dat(Count_HB, Data_Entomo, p = 0.92)
#' plot_exposure(Exposure, vil= "DOG", age = "pop")
#'
plot_exposure <- function(Exposure, vil, age = "pop"){

	require(tidyr)
	require(dplyr)
	require(ggplot2)
	sub_Exposure_graph <- subset(Exposure, Vil == vil & Age == age)# & Enq == enq)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour

	###### Making Graphs of exposure
	data_graph <- sub_Exposure_graph %>% gather("exposure","value",c("eui", "euo", "eun", "eup"))
	data_graph$exposure <- as.factor(data_graph$exposure)

	# reorder levels for graph purpose
	data_graph$exposure <- factor(data_graph$exposure, levels=c("eup", "eun", "eui", "euo"))

	# plot exposure
	plot <- ggplot(data_graph, aes(x=t2, y=value, fill=exposure)) +
		geom_area(colour="black", size=.2, alpha=.4) +
		scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$exposure))) +
		facet_wrap(~Enq) +
		scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))
	### ADD Sunset and sunrise (To do)
	return(plot)
}

###### making graph of human and vector behaviors

plot_behaviors <- function(Exposure, vil, age){

	sub_Exposure_graph <- subset(Exposure, Vil == vil & Age == age)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour

	sub_Exposure_graph$pNet <- sub_Exposure_graph$U_nets / sub_Exposure_graph$N_Ind
	sub_Exposure_graph$pIn <- sub_Exposure_graph$N_In / sub_Exposure_graph$N_Ind - (sub_Exposure_graph$pNet)
	maImax <- max(with(sub_Exposure_graph,Ni/d), na.rm=T)
	maOmax <- max(with(sub_Exposure_graph,No/d), na.rm=T)
	mamax <- max(maImax,maOmax)
	sub_Exposure_graph$ma_in <- with(sub_Exposure_graph,Ni/d)/mamax
	sub_Exposure_graph$ma_ou <- with(sub_Exposure_graph,No/d)/mamax

	data_graph <- sub_Exposure_graph %>% gather("people","value",33:34)
	data_graph$people <- as.factor(data_graph$people)


	ggplot(data_graph, aes(x=t2, y=value, fill=people)) +
		geom_area(colour="grey", size=.3, alpha=.4) +
		scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$people))) +
		facet_wrap(~Enq) +
		scale_y_continuous(sec.axis = sec_axis(~.*mamax, name = "hourly biting rate (bites/human/hour)"), name = "% of people indoor or under net") +
		geom_line(aes(y = ma_in, linetype ="indoor"), size=0.6) +
		geom_line(aes(y = ma_ou, linetype ="outdoor"), size=0.6) +
		scale_linetype_manual(values = c(
			'indoor' = 1,
			'outdoor' = 4)) +
		labs(linetype = 'anopheles') +
		scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))

}

plot_behavior_H <- function(Exposure, vil = c("DOG"), enq = 1, nc = 6){

	sub_Exposure_graph <- subset(Exposure, Vil %in% vil & !(Age %in% c("pop","all")) & Enq == enq)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour

	sub_Exposure_graph$pNet <- sub_Exposure_graph$U_nets / sub_Exposure_graph$N_Ind
	sub_Exposure_graph$pIn <- sub_Exposure_graph$N_In / sub_Exposure_graph$N_Ind - (sub_Exposure_graph$pNet)

	data_graph <- sub_Exposure_graph %>% gather("people","value",33:34)
	data_graph$people <- as.factor(data_graph$people)


	ggplot(data_graph, aes(x=t2, y=value, fill=people)) +
		geom_area(colour="grey", size=.3, alpha=.4) +
		scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$people))) +
		facet_wrap(~Vil+Age, ncol=nc) +
		scale_y_continuous(name = "% of people indoor or under net") +
		scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))

}

