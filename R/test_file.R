ODK_HB_R <- read.table("C:\\Users\\moiroux\\Dropbox\\Proj_PANIC\\Data_Comportement\\Hum_Behav_PANIC.txt", header=TRUE, sep="", na.strings="NA",
											 dec=".", strip.white=TRUE)

## dataframe management in oder to be passed to function HB_to_counts
# modify column names
colnames(ODK_HB_R)[1:8] <- paste0("rcp", colnames(ODK_HB_R)[1:8])

# uppercase codevillage (needed to merge data with entomological data subsequently)
ODK_HB_R$rcpcodevillage <- as.factor(sapply(ODK_HB_R$rcpcodevillage,toupper))
ODK_HB_R <- ODK_HB_R[,-c(2,4)]

save(ODK_HB_R, file="ODK_HB_R.RData")

Entomo_PHP <- read.delim("C:\\Users\\moiroux\\Dropbox\\Proj_PANIC\\Data_Entomo\\PANIC Captures 1_4.txt", na.strings="")
names(Entomo_PHP) <- iconv(names(Entomo_PHP),to="ASCII//TRANSLIT") # remove accents

v_hours <- c(15:23,0:11)
Entomo_PHP <- Entomo_PHP[,c(1,4,5,7,9,12)]

save(Entomo_PHP, file="Entomo_PHP.RData")

load_all()

Count_HB <- HB_to_counts(ODK_HB_R)
Count_HB$Enq <- 3
v_hours <- c(15:23,0:11)
Data_Entomo <- Entomo_PHP_to_counts(Entomo_PHP, v_hours)

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...)) # function equiv expand.grid but for df
Count_HB <- expand.grid.df(Count_HB,data.frame(Enq = 1:4))

exposure_dat(Count_HB, Data_Entomo, p = 0.92)

Exposure <- exposure_dat(Count_HB, Data_Entomo, p = 0.92)
plot_exposure(Exposure, vil= "DOG", age = "pop")

