### Entomo_PHP_to_counts fonction convert data from individual mosquitoes collection dataframe to counts of individuals per collection sites (giving 8 collections sites per place, 4 indoors, 4 outdoors)
# for each survey, villages and collection hour
# works with several places (villages) and surveys

# from the output of the PHP app :
# all empty cell should be filled with 'NA' previously
# accents should be removed in column names
# see file 'Entomo_PHP.RData' for the correct structure

#' convert data from a dataframe of individual mosquitoes to counts of individuals per collection sites
#'
#'
#' @param data a data frame of individual mosquitoes with the following column :
#' `NumMission` survey of collection
#' `CodeVillage` village code (factor)
#' `PointDeCapture` collection site identifier (integer refering to a place where collection have been performed both indoors and outdoors)
#' `PosteDeCapture` collection point (indoors or oudoors), value = int or ext
#' `HeureDeCapture` beginning of the collection hour, values : 0 to 23
#' `Genre` genus of the individual mosquito (default "Anopheles")
#' @param v_hours a vector of sarting hours of collection i.e c(15:23,0:11) for collections lasting from 15h in the afternoon to 12h in the morning
#' @param ndays duration (in days) of collection during each survey in each collection point (default = 1)
#' @param genre the value (string) of the genus for which counts have to be computed (from factors in the `Genre` column, default : "Anopheles")
#' @keywords
#' @return a dataframe with hourly counts of mosquitoes in each collection point ("O1" "O2""O3""O4" "I1""I2" "I3" "I4"), summed counts both indoor and outdoors ("No""Ni"),
#'  duration of collection indoors and outdoors ("di" "do", i.e., the product of the number of sites by the number of days), time in a new referential (t). Per villages and survey.
#' @export
#' @examples
#' v_hours <- c(15:23,0:11)
#' ndays <- 2
#' Entomo_PHP_to_counts(Entomo_PHP, v_hours, ndays)


Entomo_PHP_to_counts <- function(data, v_hours, ndays = 1, genre = "Anopheles"){

	data$site <- as.factor(paste0(data$PosteDeCapture, data$PointDeCapture))
	Sub_Entomo_PHP <- subset(data, Genre == genre)						# to modify according to what is wanted (one specific species during one survey for example)



	# Reshape dataframe
	Data_Entomo <- Sub_Entomo_PHP %>%
									dplyr::group_by(CodeVillage, HeureDeCapture, NumMission, site) %>%
									dplyr::summarise(count = dplyr::n()) %>%
									tidyr::spread(site, count, fill=0)   ####### to modify if a new column of count is added in the new version of the PHP app ########

	colnames(Data_Entomo)[1:3] <- c("Vil", "HeureDeCapture", "Enq") # rename columns

	# Create rows for hours with zero vectors collected
	v_entomo <- levels(Data_Entomo$Vil)
	e_entomo <- unique(Data_Entomo$Enq)
	collect_times <- expand.grid(Vil = v_entomo , HeureDeCapture = v_hours, Enq = e_entomo) 	# Create table with all possible values of Vil, HeureDecapture and Enq
	Data_Entomo <- dplyr::left_join(collect_times, Data_Entomo, by=c('Vil','Enq','HeureDeCapture'))	# join data to Data_Entomo
	Data_Entomo[is.na(Data_Entomo)] <- 0																				 							# Replace NAs by zeros

	# Calculate sums vectors collected indoors (Ni) and outdoors (No)
	Data_Entomo <- Data_Entomo %>% dplyr::mutate(No = dplyr::select(., dplyr::contains("ext")) %>% rowSums())
	Data_Entomo <- Data_Entomo %>% dplyr::mutate(Ni = dplyr::select(., dplyr::contains("int")) %>% rowSums())

	Data_Entomo$di <- stringr::str_detect(colnames(Data_Entomo), "int") %>% which(TRUE) %>% length() %>% prod(ndays)	# duration of collection indoors per time interval (nb of sites x 1 hours x nb of days of collection)
	Data_Entomo$do <- stringr::str_detect(colnames(Data_Entomo), "ext") %>% which(TRUE) %>% length() %>% prod(ndays)	# duration of collection outdoors per time interval (nb of sites x 1 hours x nb of days of collection)

	# Create column t (hours in the same referential than human behavior data with 12h = 0, 00H = 12 etc..)
	# function to convert hours of collection in a new referential with 12h = 0 and 00h = 12
	hour_to_n <- function(x){
		if (x > 12) {n <- x - 12} else {n <- x + 12 }
	}
	Data_Entomo$t <- sapply(Data_Entomo$HeureDeCapture,hour_to_n)
	##### END #############

	return(Data_Entomo)
}
