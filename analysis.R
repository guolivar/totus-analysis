library("data.table")
library("ggplot2")
library("gridExtra")
library("grid")

##read the file
fileName <- as.data.table(read.csv("~/data/TOTUS/Ayushi_book/all_data_merged_chch_04012017.csv"))
fileName$roundNO2_del <- round(fileName$no2_delta)


##Subset for 2006 data based on deprivation index
DI0 <- fileName[ which(fileName$NZDep2006 == 0),]
DI1 <- fileName[ which(fileName$NZDep2006 == 1),]
DI2 <- fileName[ which(fileName$NZDep2006 == 2),]
DI3 <- fileName[ which(fileName$NZDep2006 == 3),]
DI4 <- fileName[ which(fileName$NZDep2006 == 4),]
DI5 <- fileName[ which(fileName$NZDep2006 == 5),]
DI6 <- fileName[ which(fileName$NZDep2006 == 6),]
DI7 <- fileName[ which(fileName$NZDep2006 == 7),]
DI8 <- fileName[ which(fileName$NZDep2006 == 8),]
DI9 <- fileName[ which(fileName$NZDep2006 == 9),]
DI10 <- fileName[ which(fileName$NZDep2006 == 10),]

fileName$weightage2006 <- NULL
fileName[which(fileName$NZDep2006 == 0),"weightage2006"] <- (fileName$URpop2006*sum(DI0$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 1),"weightage2006"] <- (fileName$URpop2006*sum(DI1$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 2),"weightage2006"] <- (fileName$URpop2006*sum(DI2$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 3),"weightage2006"] <- (fileName$URpop2006*sum(DI3$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 4),"weightage2006"] <- (fileName$URpop2006*sum(DI4$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 5),"weightage2006"] <- (fileName$URpop2006*sum(DI5$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 6),"weightage2006"] <- (fileName$URpop2006*sum(DI6$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 7),"weightage2006"] <- (fileName$URpop2006*sum(DI7$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 8),"weightage2006"] <- (fileName$URpop2006*sum(DI8$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 9),"weightage2006"] <- (fileName$URpop2006*sum(DI9$URpop2006))/sum(fileName$URpop2006)
fileName[which(fileName$NZDep2006 == 10),"weightage2006"] <-(fileName$URpop2006*sum(DI10$URpop2006))/sum(fileName$URpop2006)

DI_2013_0 <- fileName[ which(fileName$NZDep2013 == 0),]
DI_2013_1 <- fileName[ which(fileName$NZDep2013 == 1),]
DI_2013_2 <- fileName[ which(fileName$NZDep2013 == 2),]
DI_2013_3 <- fileName[ which(fileName$NZDep2013 == 3),]
DI_2013_4 <- fileName[ which(fileName$NZDep2013 == 4),]
DI_2013_5 <- fileName[ which(fileName$NZDep2013 == 5),]
DI_2013_6 <- fileName[ which(fileName$NZDep2013 == 6),]
DI_2013_7 <- fileName[ which(fileName$NZDep2013 == 7),]
DI_2013_8 <- fileName[ which(fileName$NZDep2013 == 8),]
DI_2013_9 <- fileName[ which(fileName$NZDep2013 == 9),]
DI_2013_10 <- fileName[ which(fileName$NZDep2013 == 10),]



fileName$weightage2013 <- NULL
fileName[which(fileName$NZDep2013 == 0),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_0$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 1),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_1$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 2),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_2$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 3),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_3$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 4),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_4$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 5),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_5$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 6),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_6$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 7),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_7$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 8),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_8$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 9),"weightage2013"] <- (fileName$UR_pop_201*sum(DI_2013_9$UR_pop_201))/sum(fileName$UR_pop_201)
fileName[which(fileName$NZDep2013 == 10),"weightage2013"] <-(fileName$UR_pop_201*sum(DI_2013_10$UR_pop_201))/sum(fileName$UR_pop_201)

ggplot(data = fileName[which(fileName$DI_del>0),]) + geom_hex(aes(x = DI_del, y = roundNO2_del))

ggplot(data = fileName[which(fileName$pop_del >250),]) +geom_smooth(aes(pop_del,REL_RISK), se = FALSE)
ggplot(data = fileName[which(fileName$pop_del < -250),]) +geom_smooth(aes(abs(pop_del),REL_RISK), se = FALSE)
ggplot(data = fileName[which(fileName$pop_del >= -250 & fileName$pop_del <250),]) +geom_smooth(aes(pop_del,REL_RISK), se = FALSE)


ggplot(data = fileName) +geom_hex(aes(DI_del,REL_RISK)) + xlab("Change in Deprivation Index") + ylab("Relative risk")
ggplot(data = fileName) +geom_smooth(aes(DI_del,roundNO2_del), se = FALSE)
ggplot(data = fileName[which(fileName$pop_del < -250),]) +geom_smooth(aes(abs(pop_del),REL_RISK), se = FALSE)
ggplot(data = fileName[which(fileName$pop_del >= -250 & fileName$pop_del <250),]) +geom_smooth(aes(pop_del,REL_RISK), se = FALSE)


ggplot(data = fileName[which(fileName$NZDep2006 >0),]) + geom_smooth(aes(NZDep2006,REL_RISK))+
  scale_x_continuous(breaks = pretty(fileName$NZDep2006, n = 10))


plot_per_area <- ggplot(data = fileName) +geom_smooth(aes(weightage2006, MEDno2, colour = "weightage2006"), se = FALSE)+
  geom_smooth(aes(weightage2013, MEDno2_2, colour = "weightage2013"), se = FALSE)+
  xlab("weightage") +ylab("Median NO2")

plot_per_area +facet_grid(TA2013_NAM~.)

ggplot(data = fileName[which(fileName$TA2013_NAM != "Area Outside Territorial Authority"),]) +
  geom_smooth(aes(NZDep2006,MEDno2)) +facet_grid(TA2013_NAM~.) + scale_x_continuous(breaks = pretty(fileName$NZDep2006, n = 10))

chch <- ggplot(data = fileName[which(fileName$TA2013_NAM == "Christchurch City"),]) +
  geom_smooth(aes(UR_pop_201,MEDno2_2,colour = "2013"), se = FALSE) +
  geom_smooth(aes(URpop2006,MEDno2,colour = "2006"), se = FALSE)+
  ggtitle("Christchurch City")

selwyn <- ggplot(data = fileName[which(fileName$TA2013_NAM == "Selwyn District"),]) +
  geom_smooth(aes(UR_pop_201,MEDno2_2,colour = "2013"), se = FALSE) +
  geom_smooth(aes(URpop2006,MEDno2,colour = "2006"), se = FALSE)+
  ggtitle("Selwyn District")

waimakariri <- ggplot(data = fileName[which(fileName$TA2013_NAM == "Waimakariri District"),]) +
  geom_smooth(aes(UR_pop_201,MEDno2_2,colour = "2013"), se = FALSE) +
  geom_smooth(aes(URpop2006,MEDno2,colour = "2006"), se = FALSE)+
  ggtitle("Waimakariri District")

grid.arrange(chch, selwyn, waimakariri)






plot2006 <- ggplot(data = fileName) +geom_boxplot(aes(factor(NZDep2006), MEANno2)) +xlab("NZDep2006") 
plot2013 <- ggplot(data = fileName) +geom_boxplot(aes(factor(NZDep2013), MEANno2_2)) +xlab("NZDep2013") 
smoothboth <- ggplot(data = fileName) +geom_smooth(aes(NZDep2006, MEANno2, colour = "2006")) + geom_smooth(aes(NZDep2013, MEANno2_2, colour = "2013")) +xlab("Deprivation Index") +ylab("MeanNO2")
popweight <- ggplot(data = fileName) +geom_smooth(aes(NZDep2006, URpop2006, colour = "2006")) + geom_smooth(aes(NZDep2013, UR_pop_201, colour = "2013")) +xlab("Deprivation Index") +ylab("Urban population")
grid.arrange(plot2006,plot2013, smoothboth, popweight)


##summary of 2006 data
pop_fraction2006 <- rbind(sum(DI0$URpop2006)/sum(fileName$URpop2006)*100, sum(DI1$URpop2006)/sum(fileName$URpop2006)*100, sum(DI2$URpop2006)/sum(fileName$URpop2006)*100,
                          sum(DI3$URpop2006)/sum(fileName$URpop2006)*100, sum(DI4$URpop2006)/sum(fileName$URpop2006)*100, sum(DI5$URpop2006)/sum(fileName$URpop2006)*100,
                          sum(DI6$URpop2006)/sum(fileName$URpop2006)*100, sum(DI7$URpop2006)/sum(fileName$URpop2006)*100, sum(DI8$URpop2006)/sum(fileName$URpop2006)*100,
                          sum(DI9$URpop2006)/sum(fileName$URpop2006)*100,sum(DI10$URpop2006)/sum(fileName$URpop2006)*100)


means2006 <- as.data.table(tapply(fileName$MEANno2, fileName$NZDep2006, mean))
mins2006 <- as.data.table(tapply(fileName$MEANno2, fileName$NZDep2006, min))
maxs2006 <- as.data.table(tapply(fileName$MEANno2, fileName$NZDep2006, max))
quantiles2006 <- t(as.data.table(tapply(fileName$MEANno2, fileName$NZDep2006, quantile)))
quantiles2006 <- as.data.table(quantiles2006)
summary2006 <- cbind(c(0:10),pop_fraction2006, means2006, mins2006,maxs2006, quantiles2006)
names(summary2006) <- c("DI","pop_fraction","means","mins","maxs","percent0","percent25","percent50","percent75","percent100")

pop_plot2006 <- ggplot(data = summary2006) + geom_point(aes(pop_fraction, means, colour = "means")) +
  geom_line(aes(pop_fraction, percent25, colour = "percent25")) +
  geom_line(aes(pop_fraction, percent50, colour = "percent50")) + 
  geom_line(aes(pop_fraction, percent75, colour = "percent75"))

DI_plot2006 <- ggplot(data = summary2006) + geom_point(aes(DI, means, colour = "means")) +
  geom_line(aes(DI, percent25, colour = "percent25")) +
  geom_line(aes(DI, percent50, colour = "percent50")) + 
  geom_line(aes(DI, percent75, colour = "percent75"))

POP_DI2006 <- qplot(DI, pop_fraction, data = summary2006, geom = c("point","smooth"))

s2006 <- grid.arrange(pop_plot,DI_plot, POP_DI)


##summary of 2013 data
pop_fraction2013 <- rbind(sum(DI0$UR_pop_201)/sum(fileName$UR_pop_201)*100, sum(DI1$UR_pop_201)/sum(fileName$UR_pop_201)*100, sum(DI2$UR_pop_201)/sum(fileName$UR_pop_201)*100,
                          sum(DI3$UR_pop_201)/sum(fileName$UR_pop_201)*100, sum(DI4$UR_pop_201)/sum(fileName$UR_pop_201)*100, sum(DI5$UR_pop_201)/sum(fileName$UR_pop_201)*100,
                          sum(DI6$UR_pop_201)/sum(fileName$UR_pop_201)*100, sum(DI7$UR_pop_201)/sum(fileName$UR_pop_201)*100, sum(DI8$UR_pop_201)/sum(fileName$UR_pop_201)*100,
                          sum(DI9$UR_pop_201)/sum(fileName$UR_pop_201)*100,sum(DI10$UR_pop_201)/sum(fileName$UR_pop_201)*100)

means2013 <- as.data.table(tapply(fileName$MEANno2_2, fileName$NZDep2013, mean))
mins2013 <- as.data.table(tapply(fileName$MEANno2_2, fileName$NZDep2013, min))
maxs2013 <- as.data.table(tapply(fileName$MEANno2, fileName$NZDep2013, max))
quantiles2013 <- t(as.data.table(tapply(fileName$MEANno2_2, fileName$NZDep2013, quantile)))
quantiles2013 <- as.data.table(quantiles2013)
summary2013 <- cbind(c(0:10),pop_fraction2013, means2013, mins2013,maxs2013, quantiles2013)
names(summary2013) <- c("DI","pop_fraction","means","mins","maxs","percent0","percent25","percent50","percent75","percent100")


change_summary <- merge(summary2006,summary2013, by = "DI", suffixes = c(".2006", ".2013"))


ggplot(data = change_summary) +geom_smooth(aes(DI, pop_fraction.2006, colour = "pop_fraction.2006"), se = F)+
  geom_smooth(aes(DI, pop_fraction.2013, colour = "pop_fraction.2013"), se = F)+
  xlab("NZDep") +ylab("per DI population proportion")

ggplot(data = change_summary) +geom_smooth(aes(DI, means.2006, colour = "means.2006"), se = F)+
  geom_smooth(aes(DI, means.2013, colour = "means.2013"), se = F)+
  xlab("NZDep") +ylab("mean NO2 levels")

ggplot(data = change_summary) +geom_point(aes(pop_fraction.2006, means.2006, colour = "means.2006"), se = F)+
  geom_point(aes(pop_fraction2013, means.2013, colour = "means.2013"), se = F)+
  xlab("population fraction per DI") +ylab("mean NO2 levels")

pop_plot <- ggplot(data = summary2013) + geom_point(aes(pop_fraction, means, colour = "means")) +
  geom_line(aes(pop_fraction, percent25, colour = "percent25")) +
  geom_line(aes(pop_fraction, percent50, colour = "percent50")) + 
  geom_line(aes(pop_fraction, percent75, colour = "percent75"))

DI_plot<- ggplot(data = summary2013) + geom_point(aes(DI, means, colour = "means")) +
  geom_line(aes(DI, percent25, colour = "percent25")) +
  geom_line(aes(DI, percent50, colour = "percent50")) + 
  geom_line(aes(DI, percent75, colour = "percent75"))

POP_DI<- qplot(DI, pop_fraction, data = summary2013, geom = c("point","smooth"))

s2013 <- grid.arrange(pop_plot,DI_plot, POP_DI)


ggplot(data = change_summary) +geom_smooth(aes(DI, means.2006, colour = "means.2006"), se = FALSE)+
  geom_smooth(aes(DI, means.2013, colour = "means.2013"), se = FALSE)+
  xlab("Deprivation Index") + ylab("Means")



##plots of minimums in both 2006 and 2013 based on deprivation index
min <- ggplot(data = minimums) + geom_point(aes(x = NZDep2006, y = minNO2_2006, colour = "minNO2_2006")) +
  geom_smooth(aes(x = NZDep2006, y = minNO2_2006, colour = "minNO2_2006"), se = FALSE) +  
  geom_point(aes(x = NZDep2013, y = minNO2_2013, colour = "minNO2_2013")) +
  geom_smooth(aes(x = NZDep2013, y = minNO2_2013, colour = "minNO2_2013"), se = FALSE) + 
  xlab ("NZDep")

min <- ggplot(data = minimums) + geom_point(aes(x = NZDep2006, y = pop_fraction2006, colour = "pop_fraction2006")) +
  geom_smooth(aes(x = NZDep2006, y =pop_fraction2006, colour = "pop_fraction2006"), se = FALSE) +  
  geom_point(aes(x = NZDep2006, y = minNO2_2006, colour = "minNO2_2006")) +
  geom_smooth(aes(x = NZDep2006, y = minNO2_2006, colour = "minNO2_2006"), se = FALSE) + 
  geom_point(aes(x = NZDep2006, y = pop_fraction2013, colour = "pop_fraction2013")) +
  geom_smooth(aes(x = NZDep2006, y =pop_fraction2013, colour = "pop_fraction2013"), se = FALSE) +  
  geom_point(aes(x = NZDep2006, y = minNO2_2013, colour = "minNO2_2013")) +
  geom_smooth(aes(x = NZDep2013, y = minNO2_2013, colour = "minNO2_2013"), se = FALSE) + 
  xlab ("NZDep")+ ylab("%pop and minimumNO2")

delta <- ggplot(data = minimums) +geom_smooth(aes(x = NZDep2006, y = del, colour = "del")) +xlab("NZDep")

grid.arrange(min,delta)

##subset form: increase and decrease in deprivation index (DI_del >0)

negative_outcome <- fileName[ which(fileName$DI_del>0),]
positive_outcome <- fileName[which(fileName$DI_del<0), ]

c <- ggplot(data = negative_outcome) +geom_smooth(aes(x =MEANno2, y =DI_del))
ggplot(data = positive_outcome) +geom_smooth(aes(x =pop_del, y = MEANno2_de))

ggplot(data = positive_outcome) +geom_boxplot(aes(x = factor(DI_del), y = weightage*MEANno2, colour = "MEANno2")) 

ggplot(data = negative_outcome) +geom_point(aes(x = DI_del, y = MEANno2_de, colour = "MEANno2_de")) 

ggplot(data = fileName, aes(x =NZDep2006, y =weightage, group = NZDep2006)) + geom_boxplot()


a <- ggplot(data = fileName[fileName$URpop2006 <= 100,]) + geom_boxplot(aes(x =factor(NZDep2006), y =weightage*MEANno2))
b <- ggplot(data = fileName[fileName$URpop2006 > 100 & fileName$URpop2006 <=300,]) + geom_boxplot(aes(x =factor(NZDep2006), y =weightage*MEANno2))
c <- ggplot(data = fileName[fileName$URpop2006 > 300,]) + geom_boxplot(aes(x =factor(NZDep2006), y =weightage*MEANno2))
grid.arrange(a,b,c)


fit <- loess(fileName$MEANno2~fileName$weightage+fileName$DI_del)




## 12/12/2016

slices <- c(453,3849,269586,337374,22458,6252,360,180)
lbls <- c("-2","-1","0","1","2","3","4","5")
pie(slices, labels = lbls, main="Pie Chart of NO2 change observed on Urban Population 2006")

library(dplyr)
fileName <- fileName[which(fileName$pop_del<=-100 | fileName$pop_del>=100)]
summary <- as.data.table(fileName %>% group_by(NZDep2006, CB2013_NAM) %>% summarise(Sum_pop = sum(URpop2006)))
no2_summary <- as.data.table(fileName %>% group_by(NZDep2006, CB2013_NAM) %>% summarise(no2_2006 = mean(no2)))
no2_change <- as.data.table(fileName %>% group_by(NZDep2006, CB2013_NAM) %>% summarise(no2_delta = mean(no2_delta)))
summary <- merge(summary, no2_summary, by = c("NZDep2006", "CB2013_NAM"), all = TRUE)
summary <- merge(summary, no2_change, by = c("NZDep2006", "CB2013_NAM"), all = TRUE)
summary$CB2013_NAM <- unlist(strsplit(as.character(summary$CB2013_NAM)," Community"))
summary <- summary[which(summary$NZDep2006>0),] 


print(unique(summary$CB2013_NAM[which(summary$no2_delta < 0)]))
print(unique(summary$CB2013_NAM[which(summary$no2_delta > 0)]))
print(unique(summary$CB2013_NAM[which(summary$no2_delta == 0)]))
