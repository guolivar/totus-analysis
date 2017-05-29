library("data.table")
library("ggplot2")
library("gridExtra")
library("grid")

##read the file
fileName <- as.data.table(read.csv("~/data/TOTUS/Ayushi_book/all_data_merged_chch_04012017.csv"))
fileName$percentageNO2_del <- (fileName$MEANno2_de/fileName$MEANno2)*100
fileName$POP_del <- fileName$UR_pop_201 - fileName$URpop2006
fileName$DI_del <- fileName$NZDep2013 - fileName$NZDep2006

populationperAU2006 <- tapply(fileName$URpop2006,fileName$AU2013_NAM,sum)
NO2perAU2006 <- tapply(fileName$MEANno2,fileName$AU2013_NAM,mean)
populationperAU2013 <- tapply(fileName$UR_pop_201,fileName$AU2013_NAM,sum)
NO2perAU2013<- tapply(fileName$MEANno2_2,fileName$AU2013_NAM,mean)
exposuretable <- cbind(populationperAU2006,populationperAU2013,NO2perAU2006,NO2perAU2013)
AU2013_NAM <- row.names(exposuretable)
exposuretable <- as.data.table(cbind(AU2013_NAM, exposuretable))
exposuretable$populationperAU2006 <- as.numeric(exposuretable$populationperAU2006)
exposuretable$populationperAU2013 <- as.numeric(exposuretable$populationperAU2013)
exposuretable$NO2perAU2006 <- as.numeric(exposuretable$NO2perAU2006)
exposuretable$NO2perAU2013 <- as.numeric(exposuretable$NO2perAU2013)
exposuretable$Pop_Del <- exposuretable$populationperAU2013 - exposuretable$populationperAU2006
exposuretable$NO2_del <- 100*(exposuretable$NO2perAU2013 - exposuretable$NO2perAU2006)/exposuretable$NO2perAU2006

#write.csv(exposuretable, "~/data/TOTUS/Ayushi_book/AUwise_exposuretable.csv")

fileName_merged <- merge(fileName,exposuretable, by = "AU2013_NAM", all=TRUE)


TA_NAM_sum2006 <- tapply(fileName_merged$URpop2006,fileName_merged$TA2013_NAM,sum)
TA_NAM_sum2013 <- tapply(fileName_merged$UR_pop_201,fileName_merged$TA2013_NAM,sum)
TA_NAM_meanNO2_2006 <- tapply(fileName_merged$MEANno2,fileName_merged$TA2013_NAM,mean)
TA_NAM_meanNO2_2013 <- tapply(fileName_merged$MEANno2_2,fileName_merged$TA2013_NAM,mean)

TA_populationtable <- cbind(TA_NAM_sum2006,TA_NAM_sum2013,TA_NAM_meanNO2_2006,TA_NAM_meanNO2_2013)
TA2013_NAM <- row.names(TA_populationtable)
TA_populationtable <- as.data.table(cbind(TA2013_NAM,TA_populationtable))
TA_populationtable$pop_del <- ((as.numeric(TA_populationtable$TA_NAM_sum2013) - as.numeric(TA_populationtable$TA_NAM_sum2006))/as.numeric(TA_populationtable$TA_NAM_sum2006))*100
TA_populationtable$NO2_del <- 100*(as.numeric(TA_populationtable$TA_NAM_meanNO2_2013) - as.numeric(TA_populationtable$TA_NAM_meanNO2_2006))/(as.numeric(TA_populationtable$TA_NAM_meanNO2_2006))


fileName_merged <- merge(fileName_merged,TA_populationtable, by = "TA2013_NAM", all=TRUE)
fileName_merged$populationfactor2006 <- as.numeric(fileName_merged$populationperAU2006)/as.numeric(fileName_merged$TA_NAM_sum2006)
fileName_merged$populationfactor2013 <- as.numeric(fileName_merged$populationperAU2013)/as.numeric(fileName_merged$TA_NAM_sum2013)

fileName_merged$popexp2006 <- fileName_merged$MEANno2*fileName_merged$populationfactor2006
fileName_merged$popexp2013 <- fileName_merged$MEANno2_2*fileName_merged$populationfactor2013

fileName_merged$ExposureDel <- fileName_merged$popexp2013 - fileName_merged$popexp2006


write.csv(fileName_merged, "~/data/TOTUS/Ayushi_book/exposure_added_summary.csv")

ggplot(fileName_merged[which(fileName_merged$DI_del>=-5|fileName_merged$DI_del<=5)]) + geom_smooth(aes(Pop_Del, NO2_del))

