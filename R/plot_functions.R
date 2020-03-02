#### making graph of exposure to bite

#' makes multi panel plot of exposure to bite (one panel per entomological survey for one village and one age classes)
#'
#' @param Exposure a dataframe obtained by the use of function `exposure_dat`
#' @param vil one village code (string) from Exposure$Vil
#' @param age one age classes (string) from Exposure$Age
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' Count_HB <- HB_to_counts(ODK_HB_R)
#' Count_HB$Enq <- 3
#' v_hours <- c(15:23,0:11)
#' Data_Entomo <- Entomo_PHP_to_counts(Entomo_PHP, v_hours)
#' Exposure <- exposure_dat(Count_HB, Data_Entomo, p = 0.92)
#' plot_exposure(Exposure, vil= "DOG", age = "pop")
#'
plot_exposure <- function(Exposure, vil, age = "pop"){

	sub_Exposure_graph <- subset(Exposure, Vil == vil & Age == age)# & Enq == enq)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour

	###### Making Graphs of exposure
	data_graph <- sub_Exposure_graph %>% tidyr::gather("exposure","value",c("eui", "euo", "eun", "eup"))
	data_graph$exposure <- as.factor(data_graph$exposure)

	# reorder levels for graph purpose
	data_graph$exposure <- factor(data_graph$exposure, levels=c("eup", "eun", "eui", "euo"))

	# plot exposure
	plot <- ggplot2::ggplot(data_graph, ggplot2::aes(x=t2, y=value, fill=exposure)) +
		ggplot2::geom_area(colour="black", size=.2, alpha=.4) +
		ggplot2::scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$exposure))) +
		ggplot2::facet_wrap(~Enq) +
		ggplot2::scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))
	### ADD Sunset and sunrise (To do)
	return(plot)
}

###### making graph of human and vector behaviors

#' plot human and vector behavior on the same graph (multi-panel plot, one plot per survey)
#'
#' @param Exposure a dataframe obtained by the use of function `exposure_dat`
#' @param vil one village code (string) from Exposure$Vil
#' @param age one age classes (string) from Exposure$Age
#'
#' @return a ggplot object
#' @export
#'
#' @examples
plot_behaviors <- function(Exposure, vil, age = "pop"){

	sub_Exposure_graph <- subset(Exposure, Vil == vil & Age == age)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour

	sub_Exposure_graph$pNet <- sub_Exposure_graph$U_nets / sub_Exposure_graph$N_Ind
	sub_Exposure_graph$pIn <- sub_Exposure_graph$N_In / sub_Exposure_graph$N_Ind - (sub_Exposure_graph$pNet)
	maImax <- max(with(sub_Exposure_graph,Ni/di), na.rm=T)
	maOmax <- max(with(sub_Exposure_graph,No/do), na.rm=T)
	mamax <- max(maImax,maOmax)
	sub_Exposure_graph$ma_in <- with(sub_Exposure_graph,Ni/di)/mamax
	sub_Exposure_graph$ma_ou <- with(sub_Exposure_graph,No/do)/mamax

	data_graph <- sub_Exposure_graph %>% tidyr::gather("people","value",c("pNet", "pIn"))
	data_graph$people <- as.factor(data_graph$people)


	plot <- ggplot2::ggplot(data_graph, ggplot2::aes(x=t2, y=value, fill=people)) +
		ggplot2::geom_area(colour="grey", size=.3, alpha=.4) +
		ggplot2::scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$people))) +
		ggplot2::facet_wrap(~Enq) +
		ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~.*mamax, name = "hourly biting rate (bites/human/hour)"), name = "% of people indoor or under net") +
		ggplot2::geom_line(ggplot2::aes(y = ma_in, linetype ="indoor"), size=0.6) +
		ggplot2::geom_line(ggplot2::aes(y = ma_ou, linetype ="outdoor"), size=0.6) +
		ggplot2::scale_linetype_manual(values = c(
			'indoor' = 1,
			'outdoor' = 4)) +
		ggplot2::labs(linetype = 'anopheles') +
		ggplot2::scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))

	return(plot)
}

#' multi panel plot (per age classes) of human behavior
#'
#' @param Exposure a dataframe obtained by the use of function `exposure_dat`
#' @param vil a string vector of village id from Exposure$Vil
#' @param enq one survey number (integer) from Exposure$Enq
#' @param nc number of column in the multipanel plot
#'
#' @return a ggplot object
#' @export
#'
#' @examples
plot_behavior_H <- function(Exposure, vil = c("DOG"), enq = 1, nc = 6){

	sub_Exposure_graph <- subset(Exposure, Vil %in% vil & !(Age %in% c("pop","all")) & Enq == enq)		#subset for one village and one category of age
	sub_Exposure_graph$t2 <- sub_Exposure_graph$t+0.5									#add 0.5 to the time to plot value of exposure at the center of a period of one hour

	sub_Exposure_graph$pNet <- sub_Exposure_graph$U_nets / sub_Exposure_graph$N_Ind
	sub_Exposure_graph$pIn <- sub_Exposure_graph$N_In / sub_Exposure_graph$N_Ind - (sub_Exposure_graph$pNet)

	data_graph <- sub_Exposure_graph %>% tidyr::gather("people","value",c("pNet", "pIn"))
	data_graph$people <- as.factor(data_graph$people)


	plot <- ggplot2::ggplot(data_graph, ggplot2::aes(x=t2, y=value, fill=people)) +
		ggplot2::geom_area(colour="grey", size=.3, alpha=.4) +
		ggplot2::scale_fill_brewer(palette="Greens", breaks=rev(levels(data_graph$people))) +
		ggplot2::facet_wrap(~Vil+Age, ncol=nc) +
		ggplot2::scale_y_continuous(name = "% of people indoor or under net") +
		ggplot2::scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"))

	return(plot)
}

