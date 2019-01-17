### Entomo_PHP_to_counts fonction convert data from individual mosquitoes collection dataframe to counts of individuals per collection sites (giving 8 collections sites per place, 4 indoors, 4 outdoors)
# for each survey, villages and collection hour
# works with several places (villages) and surveys

# from the output of the PHP app :
# all empty cell should be filled with 'NA' previously
# accents should be removed in column names
# see file 'PANIC Captures 1_4.txt' for the correct structure

#' convert data from individual mosquitoes collection dataframe to counts of individuals per collection sites (giving 8 collections sites per place, 4 indoors, 4 outdoors)
#'
#'
#' @param data a data frame of individual mosquitoes with the following column :
#' `NumMission` survey of collection
#' `CodeVillage` village code (factor)
#' `PointDeCapture` collection site (i.e place where collection have been performed both indoors and outdoors) value : 1 to 4
#' `PosteDeCapture` collection point (indoors or oudoors), value = int or ext
#' `HeureDeCapture` beginning of the collection hours : passible values : 0 to 23
#' `Genre` genus of the individual mosquito
#' @param v_hours a vector of sarting hours of collection i.e c(15:23,0:11) for collections lasting from 15h in the afternoon to 12h in the morning
#' @param genre the value (string) of the genus for which counts have to be computed (from factors in the `Genre` column, default : "Anopheles")
#' @keywords
#' @return a dataframe with hourly counts of mosquitoes in each collection point ("O1" "O2""O3""O4" "I1""I2" "I3" "I4"), count summed indoor and outdoors ("No""Ni"),
#'  duration of collection indoors and outdoors (d = 4), time in a new referential (t). Per villages and survey.
#' @export
#' @examples
#' v_hours <- c(15:23,0:11)
#' Entomo_PHP_to_counts(Entomo_PHP, v_hours)


Entomo_PHP_to_counts <- function(data, v_hours, genre = "Anopheles"){
	require(reshape2)
	require(dplyr)
	data$site <- as.factor(paste(data$PosteDeCapture, data$PointDeCapture))
	Sub_Entomo_PHP <- subset(data, Genre == genre)						# to modify according to what is wanted (one specific species during one survey for example)



	# Reshape dataframe
	Data_Entomo <- dcast(Sub_Entomo_PHP, CodeVillage + HeureDeCapture + NumMission ~ site)   ####### to modify if a new column of count is added in the new version of the PHP app ########
	colnames(Data_Entomo) <- c("Vil", "HeureDeCapture", "Enq", "O1", "O2","O3","O4","I1","I2","I3","I4") # rename columns

	# Create rows for hours with zero vectors collected
	v_entomo <- levels(Data_Entomo$Vil)
	e_entomo <- unique(Data_Entomo$Enq)
	collect_times <- expand.grid(Vil = v_entomo , HeureDeCapture = v_hours, Enq = e_entomo) 	# Create table with all possible values of Vil, HeureDecapture and Enq
	Data_Entomo <- left_join(collect_times, Data_Entomo, by=c('Vil','Enq','HeureDeCapture'))	# join data to Data_Entomo
	Data_Entomo[is.na(Data_Entomo)] <- 0																				 							# Replace NAs by zeros

	# Calculate sums vectors collected indoors (Ni) and outdoors (No)
	Data_Entomo$No <- rowSums(subset(Data_Entomo, select = c(O1,O2,O3,O4)))
	Data_Entomo$Ni <- rowSums(subset(Data_Entomo, select = c(I1,I2,I3,I4)))
	Data_Entomo$d <- 4																	# duration of collection indoors per time interval (4 sites x 1 hours)

	# Create column t (hours in the same referential than human behavio data with 12h = 0, 00H = 12 etc..)
	# function to convert hours of collection in a new referential with 12h = 0 and 00h = 12
	hour_to_n <- function(x){
		if (x > 12) {n <- x - 12} else {n <- x + 12 }
	}
	Data_Entomo$t <- sapply(Data_Entomo$HeureDeCapture,hour_to_n)
	##### END #############
	#'Data_Entomo' is used in the code 'Exposure_calc_graph' that calculate exposure value and produce graph of exposure to bite
	return(Data_Entomo)
}
