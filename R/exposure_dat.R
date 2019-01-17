###### Exposure calculations (for graphical purposes)

#' Calculate hourly exposure to bite indices based on human and vector behaviors data
#' the generated dataframe can be used with functions `plot_exposure`, ` plot_behaviors` and `plot_behavior_H`for plotting purpose
#' and with `daily_exp_stat` and `simul_IC_exp` to calculate daily statistics of exposure
#'
#' @param Count_HB a dataframe resulting from function `HB_to_counts` with an additional column specifying the survey number ("Enq", integer)
#' @param COunt_ento a dataframe resulting from function `Entomo_PHP_to_counts`
#' @param p the personal protection provided by an LLIN (default = 0.92 for Permanet 2 according to Corbel et al. 2010)
#' @keywords
#' @return a dataframe joining Count_HB and COunt_ento plus the following calculmated fields:
#' `pui`` proportion of LLIN user being indoor (rounded to deal with small neagtive value of U_outdoors)
#' `puo` proportion of LLIN user being outdoor
#' `pup` proportion of LLIN user being to bed and protected by a LLIN
#' `eui` user exposuExposure indoor
#' `euo` user exposuExposure outdoor
#' `eun` user exposuExposure under an LLIN
#' `eup` user exposure prevented by LLIN
#' @export
#' @examples
#' Count_HB <- HB_to_counts(ODK_HB_R)
#' Count_HB$Enq <- 1
#' Data_Entomo <- Entomo_PHP_to_counts(Entomo_PHP)
#' exposure_dat(Count_HB, Data_Entomo, p = 0.92)
#'
exposure_dat <- function(Count_HB, Count_ento, p = 0.92){
	require(dplyr)

	Exposure <- left_join(Count_HB, Count_ento, by=c('Vil','Enq','t'))		# join the table Count_HB and Count_ento based on Vil, Enq and t

	Exposure$pui <- round(Exposure$U_indoors/Exposure$N_User, 10)		# proportion of LLIN user being indoor (rounded to deal with small neagtive value of U_outdoors)
	Exposure$puo <- round(Exposure$U_outdoors/Exposure$N_User, 10)		# proportion of LLIN user being outdoor
	Exposure$pup <- round(Exposure$U_nets/Exposure$N_User, 10)			# proportion of LLIN user being to bed and protected by a LLIN

	Exposure$eui<-Exposure$Ni/Exposure$d*Exposure$pui			# user exposuExposure indoor (Bi,t*(It-St))
	Exposure$euo<-Exposure$No/Exposure$d*Exposure$puo			# user exposuExposure outdoor (Bo,t*(1-It))
	Exposure$eun<-Exposure$Ni/Exposure$d*Exposure$pup*(1-p)		# user exposuExposure under an LLIN (Bi,t*St*(1-P))
	Exposure$eup<-Exposure$Ni/Exposure$d*Exposure$pup*p			# user exposure prevented by LLIN (Bi,t*St*P)

	return(Exposure)
}

