### To use after code 1_ODK_to_Count_HB.r and 2_Exposure_calc_graph.r (i.e need Dataframe 'DataExp')
###### With dataframe 'DataExp' (see supplementary method of Moiroux et al. 2014 for details)

#' Calculates exposure indices and simulates confidence intervals based on bootstraping methods
#'
#' @param DataExp a dataframe obtained by the use of function `exposure_dat`
#' @param p the personal protection provided by an LLIN (default = 0.92 for Permanet 2 according to Corbel et al. 2010)
#' @param ndays duration (in days) of collection during each survey (default = 1)
#' @param nsim number of simulations (default = 5000)
#' @param tE upper limit hour for evening exposure calculation, in a referential with 12h as origin (i.e 10 correspond to 22h)
#' @param tM lower limit hour for morning exposure calculation, in a referential with 12h as origin (i.e 18 correspond to 6h)
#'
#' @return a dataframe with 7 columns :
#' "Vil"     village code (factor)
#' "Enq"     survey number (integer)
#' "Age"     age classes (factor)
#' "var_exp" name of the calculated exposure variable (Eff, Peui, Peuni, PeuE, PeneE, PeuM, PenuM) (factor)
#' "mean"    mean value (real)
#' "lo95"    lower bound of 95% CI of mean  (real)
#' "hi95"   higer bound of 95% CI of mean  (real)
#' @export
#'
#' @examples
#' Count_HB <- HB_to_counts(ODK_HB_R)
#' Count_HB$Enq <- 1
#' Data_Entomo <- Entomo_PHP_to_counts(Entomo_PHP)
#' Exposure <- exposure_dat(Count_HB, Data_Entomo, p = 0.92)
#' exp_CI <- simul_IC_exp(Exposure, p = 0.92, ndays = 1, nsim = 5000, tE = 10, tM = 18)
#'
simul_IC_exp <- function(DataExp, p = 0.92, ndays = 1, nsim = 5000, tE = 10, tM = 18){
	Vil <- unique(DataExp$Vil)
	Enq <- unique(DataExp$Enq)
	Age <- unique(DataExp$Age)

	i <- 1
	for (v in Vil) {
		for (e in Enq) {
			for (a in Age) {

				Subset<-subset(DataExp, Vil == v & Enq == e & Age == a) # & t %in% Hours)

				for (t in 1:nrow(Subset)){

					Ni<-Subset[t,"Ni"]
					No<-Subset[t,"No"]
					fi1<-Subset[t,"I1"]
					fi2<-Subset[t,"I2"]
					fi3<-Subset[t,"I3"]
					fi4<-Subset[t,"I4"]
					fo1<-Subset[t,"O1"]
					fo2<-Subset[t,"O2"]
					fo3<-Subset[t,"O3"]
					fo4<-Subset[t,"O4"]
					h<-Subset[t,"d"]
					ui<-round(Subset[t,"U_indoors"],0)
					uo<-round(Subset[t,"U_outdoors"],0)
					un<-round(Subset[t,"U_nets"],0)

					hi <- sum(!is.na(c(fi1,fi2,fi3,fi4))) * ndays			# duration (in hours) of effective collection indoor (max 4 collection points)
					ho <- sum(!is.na(c(fo1,fo2,fo3,fo4))) * ndays			# duration (in hours) of effective collection outdoor
					Ni <- sum(fi1,fi2,fi3,fi4)
					No <- sum(fo1,fo2,fo3,fo4)

					bi=rpois(nsim, lambda=Ni)/hi 			# simulate nsim values of densities indoors according to a poisson distribution of counts
					bo=rpois(nsim, lambda=No)/ho 			# simulate nsim values of densities outdoors according to a poisson distribution of counts

					#--------------------------------------------------------
					usize<-ui+uo+un 			# number of users

					pui<-ui/usize 			# proportion users indoors
					puo<-uo/usize 			# proportion users outdoors
					pun<-un/usize 			# proportion users under net

					sp<-rmultinom(nsim, size = usize, prob = c(pui,puo,pun)) / usize # simulate nsim values of proportions according to a multinomial distribution of proportions

					eui<-bi*t(sp)[,1] 	# hourly exposure calculations
					euo<-bo*t(sp)[,2]
					eun<-bi*t(sp)[,3]*(1-p)
					eup<-bi*t(sp)[,3]*p
					#--------------------------------------------------------------


					if (t==1) {
						res<-cbind(eui,euo,eun,eup)
						colnames(res)<-c("eui","euo","eun","eup")
					}
					if (t>1) {
						res2<-cbind(eui,euo,eun,eup)
						colnames(res2)<-c("eui","euo","eun","eup")
						res<-cbind(res,res2)
					}

				}

				Reui<-which(colnames(res)=="eui")   			#Reui: vector of row index for eui in res
				Reuo<-which(colnames(res)=="euo")
				Reun<-which(colnames(res)=="eun")
				Reup<-which(colnames(res)=="eup")

				ReuiE<-which(colnames(res)=="eui")[1:tE]   		#ReuiE: vector of row index for eui from the first hour of collection to tE (22:00) in res (Evening)
				ReuoE<-which(colnames(res)=="euo")[1:tE]
				ReunE<-which(colnames(res)=="eun")[1:tE]
				ReupE<-which(colnames(res)=="eup")[1:tE]

				ReuiM<-which(colnames(res)=="eui")[(tM+1):24]   	#ReuiM: vector of row index for eui from tM (6:00) to the last hour of collection in res (Morning)
				ReuoM<-which(colnames(res)=="euo")[(tM+1):24]
				ReunM<-which(colnames(res)=="eun")[(tM+1):24]
				ReupM<-which(colnames(res)=="eup")[(tM+1):24]


				Seui<-rowSums(res[,Reui],na.rm = TRUE)			# Sum of hourly exposure indoor = daily exposure indoor
				Seuo<-rowSums(res[,Reuo],na.rm = TRUE)
				Seun<-rowSums(res[,Reun],na.rm = TRUE)
				Seup<-rowSums(res[,Reup],na.rm = TRUE)

				SeuiE<-rowSums(res[,ReuiE],na.rm = TRUE)
				SeuoE<-rowSums(res[,ReuoE],na.rm = TRUE)
				SeunE<-rowSums(res[,ReunE],na.rm = TRUE)
				SeupE<-rowSums(res[,ReupE],na.rm = TRUE)

				SeuiM<-rowSums(res[,ReuiM],na.rm = TRUE)
				SeuoM<-rowSums(res[,ReuoM],na.rm = TRUE)
				SeunM<-rowSums(res[,ReunM],na.rm = TRUE)
				SeupM<-rowSums(res[,ReupM],na.rm = TRUE)

				PeuE<-(SeuiE+SeuoE+SeunE)/(Seui+Seuo+Seun)						# Proportion of exposure in the Evening for users
				PenuE<-(SeuiE+SeuoE+SeunE+SeupE)/(Seui+Seuo+Seun+Seup)			# Proportion of exposure in the Evening for non users
				PeuM<-(SeuiM+SeuoM+SeunM)/(Seui+Seuo+Seun)						# Proportion of exposure in the Morning for users
				PenuM<-(SeuiM+SeuoM+SeunM+SeupM)/(Seui+Seuo+Seun+Seup)			# Proportion of exposure in the Morning for non users
				Peui<-(Seui+Seun)/(Seui+Seun+Seuo)								# Proportion of exposure indoor for user
				Penui<-(Seui+Seun+Seup)/(Seui+Seun+Seuo+Seup)					# Proportion of exposure indoor for non user
				Eff<-1-((Seui+Seun+Seuo)/(Seui+Seun+Seuo+Seup))					# True protective efficacy (P*)


				ResMCI <- data.frame(Vil = v, Enq = e, Age = a,
														 var_exp = c("Eff", "Peui", "Penui", "PeuE", "PenuE", "PeuM", "PenuM"),
														 mean = NA, lo95 = NA, hi95 = NA)

				ResMCI$mean <- sapply(as.character(ResMCI$var_exp), function(x) mean(eval(parse(text = x)),na.rm = TRUE))
				ResMCI$lo95 <- sapply(as.character(ResMCI$var_exp), function(x) quantile(eval(parse(text = x)),0.025,na.rm = TRUE))
				ResMCI$hi95 <- sapply(as.character(ResMCI$var_exp), function(x) quantile(eval(parse(text = x)),0.975,na.rm = TRUE))

				if (i==1){
					Results <- ResMCI
				}else{
					Results <- rbind(Results,ResMCI)
				}
				i<-i+1
			}
		}
	}
	return(Results)
}
