### HB_to_counts fonction convert data from interviews of human behavior (see MOIROUX et al. 2014) to counts of individuals in each compartiments (indoor, outdoors, under nets)
## works with several places (villages) and age classes but for only one survey
## compute stats for the overall population based on the recorded number of peoples censored in the household



#' Count individuals in different states (indoors, outdoors, sleeping) for both user and non-users of nets at each hour (from 0h to 23h)
#'
#' `summarise_HB` returns hourly counts of individual humans in 3 compartiments (indoor, outdoors, under nets) from individual data (hours) or behavior.
#' Used in `HB_to_counts` function
#'
#' @param data a data frame of individual behavior data with the following fields:
#' `dormirssmoust`, did the individual used a mosquito net ? (value = "oui" or "non"),
#' `hintmaison`, the hour the individual went indoors in the evening, in HH:MM:SS character format (for ex. "20:30:00")
#' `hsortiemaison`, the hour the individual went outdoors in the morning
#' `hcoucher`, the hour the individual went to sleep
#' `hlever`, the hour the individual wake up
#' @keywords
#' @return a dataframe with 6 columns of counts (N_Ind, N_User, N_In, N_Sleep, N_Mild, N_User_In) and 24 lines (one per hours)
#' @export
#' @examples
#' subset_hb <- subset(ODK_HB_R,rcpcodevillage == "DOG")
#' summarise_HB(subset_hb)

summarise_HB <- function(data){
	require(lubridate)

	# Convert hours in HH:MM:SS format to decimal days (0 to 1 with 0h = 0, 12h = 0.5 and 24h = 1)
	data$hintmaison <- as.numeric(lubridate::as.duration(lubridate::hms(data$hintmaison)), "days")
	data$hcoucher <- as.numeric(lubridate::as.duration(lubridate::hms(data$hcoucher)), "days")
	data$hlever <- as.numeric(lubridate::as.duration(lubridate::hms(data$hlever)), "days")
	data$hsortiemaison <- as.numeric(lubridate::as.duration(lubridate::hms(data$hsortiemaison)), "days")

	# change the referential of hours (from 0h to 12h, i.e, in the new referential: 12h = 0 and 0h = 0.5)
	data$HIn <- c()					# new column for hour of going indoor
	data$HCou <- c()				# new column for hour of going to bed
	data$HLev <- c()				# new column for hour of waking up
	data$HExt <- c()				# new column for hour of going outdoor

	for (i in 1:nrow(data)) {		# boucle pour la conversion des horaires dans le nouveau referentiel
		if (data$hintmaison[i] > 0.5) {data$HIn[i] <- data$hintmaison[i] - 0.5} else {data$HIn[i] <- data$hintmaison[i] + 0.5}
		if (data$hcoucher[i] > 0.5) {data$HCou[i] <- data$hcoucher[i] - 0.5} else {data$HCou[i] <- data$hcoucher[i] + 0.5}
		if (data$hlever[i] > 0.5) {data$HLev[i] <- data$hlever[i] - 0.5} else {data$HLev[i] <- data$hlever[i] + 0.5}
		if (data$hsortiemaison[i] > 0.5) {data$HExt[i] <- data$hsortiemaison[i] - 0.5} else {data$HExt[i] <- data$hsortiemaison[i] + 0.5}
	}

	N_Ind <- rep(nrow(data), 24)						# vector of hourly Number of individuals
	N_User <-  rep(length(which(data$dormirssmoust == "oui" )), 24)	# vector of hourly Number of individuals having used an LLIN
	N_In <- c()															# vector of hourly number of people being indoor
	N_Sleep <- c()													# vector of hourly number of people being asleep
	N_Mild <- c()														# vector of hourly number of people being asleep AND under an LLIN
	N_User_In <- c()												# vector of hourly number of LLIN user being indoor

	for (i in 0:23){					# 5 correspond à 17h dans le nouveau ref. et 20 correspond à 8h
		N_In[i+1] <- length(which(data$HIn < (i+1)/24 & data$HExt > (i+1)/24))   # pour être à l'intérieur, il faut être entré avant la fin de la tranche horaire (i+1) et sorti après
		N_Sleep[i+1] <- length(which(data$HCou < (i+1)/24 & data$HLev > (i+1)/24))
		N_Mild[i+1] <- length(which(data$HCou < (i+1)/24 & data$HLev > (i+1)/24 & data$dormirssmoust == "oui"))
		N_User_In[i+1] <- length(which(data$HIn < (i+1)/24 & data$HExt > (i+1)/24 & data$dormirssmoust == "oui" ))
	}
	ODK_HB_R_extr_Count_HB <- as.data.frame(cbind(N_Ind, N_User, N_In, N_Sleep, N_Mild, N_User_In))
	return(ODK_HB_R_extr_Count_HB)
}
