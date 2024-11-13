library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/merging')

### Loading in the data ###

bulls <- read.csv('bulls.csv')
celtics <- read.csv('celtics.csv')
jazz <- read.csv('jazz.csv')
rockets <- read.csv('rockets.csv')
warriors <- read.csv('warriors.csv')
caveliers <- read.csv('caveliers.csv')
heat <- read.csv('heat.csv')
lakers <- read.csv('lakers.csv')
pistons <- read.csv('pistons.csv')
spurs <- read.csv('spurs.csv')


###########################
### Editing Data Frames ###
###########################

### Function for calculating the difference in VORP ###

difference <- function(df){
  for(i in 1:nrow(df)){
    df[i,7] <- diff(c(df[i,4],df[i,5]))
  }
  temp <- df[,-1]
  names(temp) <- c('player','year','RVORP','PVORP','win','DVORP')
  return(temp)
}

bulls <- difference(bulls)
celtics <- difference(celtics)
jazz <- difference(jazz)
rockets <- difference(rockets)
warriors <- difference(warriors)
caveliers <- difference(caveliers)
heat <- difference(heat)
lakers <- difference(lakers)
pistons <- difference(pistons)
spurs <- difference(spurs)


####################################
### Creating a vector of players ###
####################################

### Playoff performers ###

playoffers <- data.frame()

clutchers <- function(df,teamer,final){
  for(i in unique(df$player)){
    temp <- filter(df,player==i)
    test <- sum(temp$DVORP > 0) == length(temp$DVORP)
    if(test == TRUE){
      adder <- data.frame(player = i, team = teamer)
      final <- rbind(final,adder)
    }
  }
  return(final)
}

playoffers <- clutchers(bulls,'bulls',playoffers)
playoffers <- clutchers(celtics,'celtics',playoffers)
playoffers <- clutchers(jazz,'jazz',playoffers)
playoffers <- clutchers(rockets,'rockets',playoffers)
playoffers <- clutchers(warriors,'warriors',playoffers)
playoffers <- clutchers(caveliers,'caveliers',playoffers)
playoffers <- clutchers(heat,'heat',playoffers)
playoffers <- clutchers(lakers,'lakers',playoffers)
playoffers <- clutchers(pistons,'pistons',playoffers)
playoffers <- clutchers(spurs,'spurs',playoffers)

### Regular season performers ###

regulars <- data.frame()

chokers <- function(df,teamer,final){
  for(i in unique(df$player)){
    temp <- filter(df,player==i)
    test <- sum(temp$DVORP < 0) == length(temp$DVORP)
    if(test == TRUE){
      adder <- data.frame(player = i, team = teamer)
      final <- rbind(final,adder)
    }
  }
  return(final)
}


regulars <- chokers(bulls,'bulls',regulars)
regulars <- chokers(celtics,'celtics',regulars)
regulars <- chokers(jazz,'jazz',regulars)
regulars <- chokers(rockets,'rockets',regulars)
regulars <- chokers(warriors,'warriors',regulars)
regulars <- chokers(caveliers,'caveliers',regulars)
regulars <- chokers(heat,'heat',regulars)
regulars <- chokers(lakers,'lakers',regulars)
regulars <- chokers(pistons,'pistons',regulars)
regulars <- chokers(spurs,'spurs',regulars)


##########################
### Saving the Changes ###
##########################

write.csv(bulls,'bulls.csv')
write.csv(caveliers,'caveliers.csv')
write.csv(celtics,'celtics.csv')
write.csv(heat,'heat.csv')
write.csv(jazz,'jazz.csv')
write.csv(lakers,'lakers.csv')
write.csv(pistons,'pistons.csv')
write.csv(rockets,'rockets.csv')
write.csv(spurs,'spurs.csv')
write.csv(warriors,'warriors.csv')
write.csv(playoffers,'playoffers.csv')
write.csv(regulars,'regulars.csv')























