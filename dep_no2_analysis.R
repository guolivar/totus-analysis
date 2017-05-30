## Load libraries
library(data.table)
library(ggplot2)
library(gridExtra)
library(grid)
library(readr)

## Read the data file
data.raw <- read.csv("~/data/TOTUS/Ayushi_book/all_data_merged_chch_04012017.csv")
## Only using Christchurch City data
data.raw <- subset(data.work,subset = TA2013_NAM == 'Christchurch City')
## Select only useful variables (NO2 and areas)
data.2006.mb <- data.raw[,c('AU06',
                         'URpop2006',
                         'MEANno2')]

data.2013.mb <- data.raw[,c('AU2013',
                         'UR_pop_201',
                         'MEANno2_2')]

## Rename to something more meaningful
# MB = Meshblock
# AU = Area Unit
# TA = Territorial Authority
# NZDep = NZ Deprivation Index
# URpop = Urban population
# no2 = Long term NO2 concentration estimates

names(data.2006.mb) <- c('AU.2006',
                         'URpop.2006',
                         'no2.2006.mean')

names(data.2013.mb) <- c('AU.2013',
                         'URpop.2013',
                         'no2.2013.mean')

## Aggregate data to AU level
dt <- data.table(data.2006.mb)
dt[,list(URpop.2006=sum(URpop.2006),no2.2006.mean=mean(no2.2006.mean)),by=AU.2006]
data.2006.au <- as.data.frame(dt)
dt <- data.table(data.2013.mb)
dt[,list(URpop.2013=sum(URpop.2013),no2.2013.mean=mean(no2.2013.mean)),by=AU.2013]
data.2013.au <- as.data.frame(dt)

## Load Deprivation Index data at CU level
nzdep.2006 <- read.csv('~/data/TOTUS/Ayushi_book/nzdep2006_CAU.txt',sep = '\t')
nzdep.2013 <- read.csv('~/data/TOTUS/Ayushi_book/nzdep2013_CAU.txt',sep = '\t')

## Join both years separately
data.2006.au <- merge(data.2006.au,nzdep.2006, by.x = 'AU.2006', by.y = 'CAU_num_2006')
data.2013.au <- merge(data.2013.au,nzdep.2013, by.x = 'AU.2013', by.y = 'CAU_2013')

## Wide dataset
data.wide.au <- merge(data.2006.au,data.2013.au, by.x = 'AU.2006',by.y = 'AU.2013')

# Change in NZDep index between 2006 and 2013
data.wide.au$NZDep.delta <- data.wide.au$CAU_average_NZDep2013 - data.wide.au$CAU_average_NZDep2006
# Change in long term NO2 concentrations between 2006 and 2013
data.wide.au$no2.delta <- data.wide.au$no2.2013.mean - data.wide.au$no2.2006.mean

## long dataset
data.2006.long <- data.2006.au
names(data.2006.long) <- c('AU','URpop','no2','CAU','NZDep.rank','NZDep.score')
data.2013.long <- data.2013.au
names(data.2013.long) <- c('AU','URpop','no2','CAU','NZDep.rank','NZDep.score')
data.2006.long$year <- 2006
data.2013.long$year <- 2013
data.long.au <- rbind(data.2006.long,data.2013.long)
# Create NZDep factors
data.long.au$NZDep.rank.factor <- as.factor(data.long.au$NZDep.rank)
data.long.au$year.factor <- as.factor(data.long.au$year)
data.long.au$NZDep.Year.factor <- interaction(data.long.au$NZDep.rank.factor,data.long.au$year.factor)
data.long.au <- data.long.au[!is.na(data.long.au$NZDep.rank.factor),]


## Plots ####

ggplot(data = data.long.au) +
  geom_boxplot(aes(x = NZDep.rank.factor,
                   y = no2,
                   fill = year.factor),
               position = 'dodge') +
  ggtitle('Long term NO2 concentrations for\nDeprivation index 2006 and 2013') +
  xlab('NZ Deprivation Index') +
  ylab('Long term NO2 concentration [ug/m3]') +
  scale_fill_discrete(name = 'Year')
