###### calculation of Exposure at the night level and other interesting indicators

daily_exp_stat <- function(Exposure, tE = 10, tM = 18){
	require(dplyr)
	expE <- Exposure %>% filter(t < 10)										# select row only for hour before 22h (for evening exposure)
	expM <- Exposure %>% filter(t >= 18)									# select row only for hour after 6h (for morning exposure)

	# sum hourly exposure (total, evening and morning)
	sumExp <- Exposure %>% group_by(Vil, Enq, Age) %>% summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T) # daily sums
	names(sumExp)[-c(1:3)] <- paste0("S",names(sumExp)[-c(1:3)])				# rename variables
	sumExpE <- expE %>% group_by(Vil, Enq, Age) %>% summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T)
	names(sumExpE)[-c(1:3)] <- paste0("S",names(sumExpE)[-c(1:3)],"E")				# rename variables
	sumExpM <- expM %>% group_by(Vil, Enq, Age) %>% summarise_at(c("eui","euo","eun","eup"),sum, na.rm=T)
	names(sumExpM)[-c(1:3)] <- paste0("S",names(sumExpM)[-c(1:3)],"M")				# rename variables
	sumExp <- bind_cols(sumExp,sumExpE[,-c(1:3)],sumExpM[,-c(1:3)])								# groups all data in one table

	sumExp$PeuE <- 	with(sumExp,(SeuiE+SeuoE+SeunE)/(Seui+Seuo+Seun))						# Proportion of exposure in the Evening for users
	sumExp$PenuE <- with(sumExp,(SeuiE+SeuoE+SeunE+SeupE)/(Seui+Seuo+Seun+Seup))			# Proportion of exposure in the Evening for non users
	sumExp$PeuM <- with(sumExp,(SeuiM+SeuoM+SeunM)/(Seui+Seuo+Seun))						# Proportion of exposure in the Morning for users
	sumExp$PenuM <- with(sumExp,(SeuiM+SeuoM+SeunM+SeupM)/(Seui+Seuo+Seun+Seup))			# Proportion of exposure in the Morning for non users
	sumExp$Peui <- with(sumExp,(Seui+Seun)/(Seui+Seun+Seuo))								# Proportion of exposure indoor for user
	sumExp$Penui <- with(sumExp,(Seui+Seun+Seup)/(Seui+Seun+Seuo+Seup))						# Proportion of exposure indoor for non user
	sumExp$Eff <- with(sumExp,1-((Seui+Seun+Seuo)/(Seui+Seun+Seuo+Seup)))					# True protective efficacy (P*)

	return(sumExp)
}
