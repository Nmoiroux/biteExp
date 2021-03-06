###### calculation of Exposure at the night level and other interesting indicators

#' Title
#'
#' @param DataExp a dataframe obtained by the use of function `exposure_dat`
#' @param tE upper limit hour for evening exposure calculation, in a referential with 12h as origin (i.e 10 corresponds to 22h)
#' @param tM lower limit hour for morning exposure calculation, in a referential with 12h as origin (i.e 18 corresponds to 6h)
#'
#' @return a dataframe with 25 columns :
#' `Vil` village / place id
#' `Enq` survey number
#' `Age` age classe
#' `Seui` daily exposure indoor for user
#' `Seuo` daily exposure outdoor for user
#' `Seun` daily exposure under net for user
#' `Seup` exposure prevented daily by using of nets
#' `SeuiE` daily exposure indoor in the Evening for user
#' `SeuoE` daily exposure outdoor in the Evening for user
#' `SeunE` daily exposure under net in the Evening for user
#' `SeupE` exposure prevented daily by using of nets in the Evening
#' `SeuiM` daily exposure indoor in the Morning for user
#' `SeuoM` daily exposure outdoor in the Morning for user
#' `SeunM` daily exposure under net in the Morning for user
#' `SeupM` exposure prevented daily by using of nets in the Morning
#' `N_Ind` number of individual surveyed
#' `N_User` number of individual using nets
#' `PeuE` Proportion of exposure in the Evening for users
#' `PenuE`Proportion of exposure in the Evening for non users
#' `PeuM` Proportion of exposure in the Morning for users
#' `PenuM` Proportion of exposure in the Morning for non users
#' `Peui` Proportion of exposure indoor for user
#' `Penui`Proportion of exposure indoor for non user
#' `Eff` True protective efficacy (P*)
#' `useRate` use Rate of LLINs
#' @export
#'
#' @examples
#' Count_HB <- HB_to_counts(ODK_HB_R)
#' Count_HB$Enq <- 1
#' Data_Entomo <- Entomo_PHP_to_counts(Entomo_PHP)
#' Exposure <- exposure_dat(Count_HB, Data_Entomo, p = 0.92)
#' sumExp <- daily_exp_stat(Exposure, tE = 10, tM = 18)
#'
daily_exp_stat <- function(DataExp, tE = 10, tM = 18){

	expE <- DataExp %>% dplyr::filter(t < tE)										# select row only for hour before 22h (for evening exposure)
	expM <- DataExp %>% dplyr::filter(t >= tM)									# select row only for hour after 6h (for morning exposure)

	# sum hourly exposure (total, evening and morning)
	sumExp <- DataExp %>% dplyr::group_by(Vil, Enq, Age) %>% dplyr::summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T) # daily sums
	names(sumExp)[-c(1:3)] <- paste0("S",names(sumExp)[-c(1:3)])				# rename variables
	sumExpE <- expE %>% dplyr::group_by(Vil, Enq, Age) %>% dplyr::summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T)
	names(sumExpE)[-c(1:3)] <- paste0("S",names(sumExpE)[-c(1:3)],"E")				# rename variables
	sumExpM <- expM %>% dplyr::group_by(Vil, Enq, Age) %>% dplyr::summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T)
	names(sumExpM)[-c(1:3)] <- paste0("S",names(sumExpM)[-c(1:3)],"M")				# rename variables
	useRate <- DataExp %>% dplyr::group_by(Vil, Enq, Age) %>% dplyr::summarise(N_Ind = first(N_Ind), N_User = first(N_User))


	sumExp <- dplyr::bind_cols(sumExp,sumExpE[,-c(1:3)],sumExpM[,-c(1:3)], useRate[,-c(1:3)])								# groups all data in one table

	sumExp$PeuE <- 	with(sumExp,(SeuiE+SeuoE+SeunE)/(Seui+Seuo+Seun))						# Proportion of exposure in the Evening for users
	sumExp$PenuE <- with(sumExp,(SeuiE+SeuoE+SeunE+SeupE)/(Seui+Seuo+Seun+Seup))			# Proportion of exposure in the Evening for non users
	sumExp$PeuM <- with(sumExp,(SeuiM+SeuoM+SeunM)/(Seui+Seuo+Seun))						# Proportion of exposure in the Morning for users
	sumExp$PenuM <- with(sumExp,(SeuiM+SeuoM+SeunM+SeupM)/(Seui+Seuo+Seun+Seup))			# Proportion of exposure in the Morning for non users
	sumExp$Peui <- with(sumExp,(Seui+Seun)/(Seui+Seun+Seuo))											# Proportion of exposure indoor for user
	sumExp$Penui <- with(sumExp,(Seui+Seun+Seup)/(Seui+Seun+Seuo+Seup))						# Proportion of exposure indoor for non user
	sumExp$Eff <- with(sumExp,1-((Seui+Seun+Seuo)/(Seui+Seun+Seuo+Seup)))					# True protective efficacy (P*)
	sumExp$useRate <- with(sumExp,N_User/N_Ind)																		# use Rate of LLINs




	return(sumExp)
}
