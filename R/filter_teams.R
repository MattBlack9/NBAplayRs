library(lookup)
library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)
library(dplyr)
library(readxl)
library(aod)
library(ggplot2)
library(tidyr)
library(stringr)
library(lookup)
library(reshape)
library(coda)


#####################################
### Setting the right directories ###
#####################################

setwd('/home/mtb77/filter')

playoff_games <- read.csv('playoff_games.csv')
regular_season <- read.csv('regular_season.csv')

names <- c('Visitor/Neutral','Visitor Points','Home/Neutral','Home Points','id','year','win','team','season_id','margin','playoff')

names(regular_season) <- names
names(playoff_games) <- names




####################
### My Functions ###
####################

filter_teams1 <- function(first,last,part,team1,team2,team3){
  temp <- part$year >= first & part$year < last
  test <- part
  test$correct <- temp
  temp2 <- part$`Visitor/Neutral` == team1 | part$`Home/Neutral` == team1 | part$`Visitor/Neutral` == team2 | part$`Home/Neutral` == team2 | part$`Visitor/Neutral` == team3 | part$`Home/Neutral` == team3 
  test$cteams <- temp2
  df <- filter(test,correct == T & cteams == T)
  return(df)
}

filter_teams2 <- function(first,last,part,team1,team2,team3,team4){
  temp <- part$year >= first & part$year < last
  test <- part
  test$correct <- temp
  temp2 <- part$`Visitor/Neutral` == team1 | part$`Home/Neutral` == team1 | part$`Visitor/Neutral` == team2 | part$`Home/Neutral` == team2 | part$`Visitor/Neutral` == team3 | part$`Home/Neutral` == team3 | part$`Visitor/Neutral` == team4 | part$`Home/Neutral` == team4
  test$cteams <- temp2
  df <- filter(test,correct == T & cteams == T)
  return(df)
}


cleaning <- function(decade,team,list){
  temp <- decade
  temp$teamer <- temp$`Visitor/Neutral` == team | temp$`Home/Neutral` == team
  temp <- filter(temp,teamer == T)
  holder <- 1
  for(i in unique(temp$year)){
    curr <- temp[temp$year == i, ]
    curr <- curr[,1:10]
    list[[holder]] <- curr
    holder <- holder + 1
  }
  return(list)
}


#80s
#Celtics
#Lakers
#Pistons

my_list <- list()

reg80 <- filter_teams1(1980,1990,regular_season,'BOS','DET','LAL')
post80 <- filter_teams1(1980,1990,playoff_games,'BOS','DET','LAL')

reg.lal.80 <- cleaning(reg80,'LAL',my_list)
post.lal.80 <- cleaning(post80,'LAL',my_list)

reg.bos.80 <- cleaning(reg80,'BOS',my_list)
post.bos.80 <- cleaning(post80,'BOS',my_list)

reg.det.80 <- cleaning(reg80,'DET',my_list)
post.det.80 <- cleaning(post80,'DET',my_list)

#90s
#Rockets
#Bulls
#Jazz

reg90 <- filter_teams1(1990,2000,regular_season,'HOU','CHI','UTA')
post90 <- filter_teams1(1990,2000,playoff_games,'HOU','CHI','UTA')

reg.hou.90 <- cleaning(reg90,'HOU',my_list)
post.hou.90 <- cleaning(post90,'HOU',my_list)

reg.chi.90 <- cleaning(reg90,'CHI',my_list)
post.chi.90 <- cleaning(post90,'CHI',my_list)

reg.uta.90 <- cleaning(reg90,'UTA',my_list)
post.uta.90 <- cleaning(post90,'UTA',my_list)

#00s
#Lakers
#Celtics
#Pistons
#Spurs

reg00 <- filter_teams2(2000,2010,regular_season,'LAL','BOS','DET','SAS')
post00 <- filter_teams2(2000,2010,playoff_games,'LAL','BOS','DET','SAS')

reg.bos.00 <- cleaning(reg00,'BOS',my_list)
post.bos.00 <- cleaning(post00,'BOS',my_list)

reg.det.00 <- cleaning(reg00,'DET',my_list)
post.det.00 <- cleaning(post00,'DET',my_list)

reg.lal.00 <- cleaning(reg00,'LAL',my_list)
post.lal.00 <- cleaning(post00,'LAL',my_list)

reg.sas.00 <- cleaning(reg00,'SAS',my_list)
post.sas.00 <- cleaning(post00,'SAS',my_list)


#10s
#Heat
#Cavs
#Warriors
#Spurs

reg10 <- filter_teams2(2010,2020,regular_season,'MIA','CLE','GSW','SAS')
post10 <- filter_teams2(2010,2020,playoff_games,'MIA','CLE','GWS','SAS')

reg.mia.10 <- cleaning(reg10,'MIA',my_list)
post.mia.10 <- cleaning(post10,'MIA',my_list)

reg.cle.10 <- cleaning(reg10,'CLE',my_list)
post.cle.10 <- cleaning(post10,'CLE',my_list)

reg.gsw.10 <- cleaning(reg10,'GSW',my_list)
post.gsw.10 <- cleaning(post10,'GSW',my_list)

reg.sas.10 <- cleaning(reg10,'SAS',my_list)
post.sas.10 <- cleaning(post10,'SAS',my_list)



#################################
### Calculating VORP Function ###
#################################

vorp <- function(df){
  stats <- data.frame(NULL)
  for(g in 1:nrow(df)){
    id <- df$id[g]
    team <- df$team[g]
    node <- paste0("table#box-", team, "-game-advanced > tbody > tr > td")
    node2 <- paste0("table#box-", team, "-game-advanced > tbody > tr > th")
    url <- paste0("https://www.basketball-reference.com/boxscores/", id, ".html")
    webpage <- read_html(url)
    vorp <- webpage %>% 
      html_nodes(node) %>% 
      html_attr("data-tip") %>%
      data.frame()
    vorp <- vorp[-which(is.na(vorp[,1])),]
    vorp <- data.frame(vorp)
    colnames(vorp) <- "VORP"
    for(i in 1:nrow(vorp)){
      vorp$VORP[i] <- as.numeric(substr(vorp$VORP[i],
                                        unlist(gregexpr("VORP", vorp$VORP[i]))[1] + 6,
                                        unlist(gregexpr("VORP", vorp$VORP[i]))[2] - 17))
    }
    
    min <- webpage %>% 
      html_nodes(node) %>% 
      html_text() %>%
      data.frame()
    colnames(min) <- "min"
    elim <- which(min$min == "Did Not Play")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Did Not Dress")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Inactive")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Not With Team")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Player Suspended")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    min <- min[which(unlist(gregexpr(":", min$min)) > 0),]
    min <- data.frame(min)
    for(i in 1:nrow(min)){
      min$sec[i] <- as.numeric(substr(min$min[i],
                                      nchar(min$min[i]) - 1,
                                      nchar(min$min[i])))
      min$min[i] <- as.numeric(substr(min$min[i],
                                      1 ,
                                      nchar(min$min[i]) - 3))
      if(is.na(min$sec[i])){
        min$sec[i] <- 0
      }
    }
    min$min <- as.numeric(min$min)
    min$min <- min$min + (min$sec / 60)
    min <- min %>%
      select(1)
    
    player <- webpage %>% 
      html_nodes(node2) %>% 
      html_attr("data-append-csv") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    player <- player[-which(is.na(player[,1])),]
    player <- player[c(1:nrow(min))]
    
    
    adder <- cbind(player, min, vorp)
    adder <- adder[order(min, decreasing = TRUE),]
    adder$id <- paste0(id, "-", team)
    if(team == df$`Home/Neutral`[g]){
      adder$opp_id <- paste0(id, "-", df$`Visitor/Neutral`[g])
    } else {
      adder$opp_id <- paste0(id, "-", df$`Home/Neutral`[g])
    }
    adder$season_id <- df$season_id[g]
    adder$win <- df$win[g]
    adder$year <- df$year[g]
    adder$adj_VORP <- as.numeric(adder$VORP) * as.numeric(adder$min)
    adder$avg_VORP <- mean(adder$adj_VORP)
    
    stats <- rbind(stats, adder)
    
    print(paste0(round((g/nrow(df)) * 100, 3),"%"))
    Sys.sleep(5)
  }
  stats$link <- paste0(stats$player,"-",stats$year)
  
  return(stats)
}




##############################################################
### Separating the decades into years and calculating VORP ###
##############################################################

### Regular Season ###

bos.reg.80years <- c('celtics.reg85.csv','celtics.reg88.csv','celtics.reg89.csv','celtics.reg86.csv','celtics.reg87.csv')

det.reg.80years <- c('pistons.reg85.csv','pistons.reg88.csv','pistons.reg89.csv','pistons.reg86.csv','pistons.reg87.csv')

lal.reg.80years <- c('lakers.reg85.csv','lakers.reg88.csv','lakers.reg89.csv','lakers.reg86.csv','lakers.reg87.csv')

chi.reg.90years <- c('bulls.reg98.csv','bulls.reg90.csv','bulls.reg91.csv','bulls.reg92.csv','bulls.reg93.csv',
                      'bulls.reg94.csv','bulls.reg95.csv','bulls.reg96.csv','bulls.reg97.csv','bulls.reg99.csv')

hou.reg.90years <- c('rockets.reg98.csv','rockets.reg90.csv','rockets.reg91.csv','rockets.reg92.csv','rockets.reg93.csv',
                      'rockets.reg94.csv','rockets.reg95.csv','rockets.reg96.csv','rockets.reg97.csv','rockets.reg99.csv')

uta.reg.90years <- c('jazz.reg98.csv','jazz.reg90.csv','jazz.reg91.csv','jazz.reg92.csv','jazz.reg93.csv',
                      'jazz.reg94.csv','jazz.reg95.csv','jazz.reg96.csv','jazz.reg97.csv','jazz.reg99.csv')

bos.reg.00years <- c('celtics.reg00.csv','celtics.reg01.csv','celtics.reg02.csv','celtics.reg03.csv','celtics.reg04.csv',
                      'celtics.reg07.csv','celtics.reg08.csv','celtics.reg09.csv','celtics.reg05.csv','celtics.reg06.csv')

det.reg.00years <- c('pistons.reg00.csv','pistons.reg01.csv','pistons.reg02.csv','pistons.reg03.csv','pistons.reg04.csv',
                      'pistons.reg07.csv','pistons.reg08.csv','pistons.reg09.csv','pistons.reg05.csv','pistons.reg06.csv')

lal.reg.00years <- c('lakers.reg00.csv','lakers.reg01.csv','lakers.reg02.csv','lakers.reg03.csv','lakers.reg04.csv',
                      'lakers.reg07.csv','lakers.reg08.csv','lakers.reg09.csv','lakers.reg05.csv','lakers.reg06.csv')

sas.reg.00years <- c('spurs.reg00.csv','spurs.reg01.csv','spurs.reg02.csv','spurs.reg03.csv','spurs.reg04.csv',
                      'spurs.reg07.csv','spurs.reg08.csv','spurs.reg09.csv','spurs.reg05.csv','spurs.reg06.csv')

cle.reg.10years <- c('caveliers.reg10.csv','caveliers.reg11.csv','caveliers.reg13.csv','caveliers.reg14.csv','caveliers.reg15.csv',
                      'caveliers.reg16.csv','caveliers.reg17.csv','caveliers.reg18.csv','caveliers.reg19.csv','caveliers.reg12.csv')

gsw.reg.10years <- c('warriors.reg10.csv','warriors.reg11.csv','warriors.reg13.csv','warriors.reg14.csv','warriors.reg15.csv',
                      'warriors.reg16.csv','warriors.reg17.csv','warriors.reg18.csv','warriors.reg19.csv','warriors.reg12.csv')

mia.reg.10years <- c('heat.reg10.csv','heat.reg11.csv','heat.reg13.csv','heat.reg14.csv','heat.reg15.csv',
                      'heat.reg16.csv','heat.reg17.csv','heat.reg18.csv','heat.reg19.csv','heat.reg12.csv')

sas.reg.10years <- c('spurs.reg10.csv','spurs.reg11.csv','spurs.reg13.csv','spurs.reg14.csv','spurs.reg15.csv',
                      'spurs.reg16.csv','spurs.reg17.csv','spurs.reg18.csv','spurs.reg19.csv','spurs.reg12.csv')



### Post Season ###

bos.post.80years <- c('celtics.post85.csv','celtics.post88.csv','celtics.post89.csv','celtics.post86.csv','celtics.post87.csv')

det.post.80years <- c('pistons.post85.csv','pistons.post88.csv','pistons.post89.csv','pistons.post86.csv','pistons.post87.csv')

lal.post.80years <- c('lakers.post85.csv','lakers.post88.csv','lakers.post89.csv','lakers.post86.csv','lakers.post87.csv')

chi.post.90years <- c('bulls.post98.csv','bulls.post90.csv','bulls.post91.csv','bulls.post92.csv','bulls.post93.csv',
                      'bulls.post94.csv','bulls.post95.csv','bulls.post96.csv','bulls.post97.csv')

hou.post.90years <- c('rockets.post98.csv','rockets.post90.csv','rockets.post91.csv','rockets.post93.csv','rockets.post94.csv',
                      'rockets.post95.csv','rockets.post96.csv','rockets.post97.csv','rockets.post99.csv')

uta.post.90years <- c('jazz.post98.csv','jazz.post90.csv','jazz.post91.csv','jazz.post92.csv','jazz.post93.csv','jazz.post94.csv',
                      'jazz.post95.csv','jazz.post96.csv','jazz.post97.csv','jazz.post99.csv')

bos.post.00years <- c('celtics.post02.csv','celtics.post03.csv','celtics.post04.csv','celtics.post08.csv','celtics.post09.csv',
                      'celtics.post05.csv')

det.post.00years <- c('pistons.post00.csv','pistons.post02.csv','pistons.post03.csv','pistons.post04.csv','pistons.post07.csv',
                      'pistons.post08.csv','pistons.post09.csv','pistons.post05.csv','pistons.post06.csv')

lal.post.00years <- c('lakers.post00.csv','lakers.post01.csv','lakers.post02.csv','lakers.post03.csv','lakers.post04.csv',
                      'lakers.post07.csv','lakers.post08.csv','lakers.post09.csv','lakers.post06.csv')

sas.post.00years <- c('spurs.post00.csv','spurs.post01.csv','spurs.post02.csv','spurs.post03.csv','spurs.post04.csv',
                      'spurs.post07.csv','spurs.post08.csv','spurs.post09.csv','spurs.post05.csv','spurs.post06.csv')

cle.post.10years <- c('caveliers.post10.csv','caveliers.post15.csv','caveliers.post16.csv','caveliers.post17.csv',
                      'caveliers.post18.csv')

gsw.post.10years <- c('warriors.post13.csv','warriors.post15.csv','warriors.post16.csv','warriors.post17.csv',
                      'warriors.post18.csv')

mia.post.10years <- c('heat.post10.csv','heat.post11.csv','heat.post13.csv','heat.post14.csv','heat.post16.csv',
                      'heat.post18.csv','heat.post12.csv')

sas.post.10years <- c('spurs.post10.csv','spurs.post11.csv','spurs.post13.csv','spurs.post14.csv','spurs.post15.csv',
                      'spurs.post16.csv','spurs.post17.csv','spurs.post18.csv','spurs.post19.csv','spurs.post12.csv')



### Function to Separate ###

ind.years <- function(list,years){
  holder <- 1
  for(i in list){
    temp <- vorp(i)
    write_csv(temp,years[holder])
    holder <- holder + 1
  }
}



### Separating the decades into individual csv's ###

ind.years(reg.lal.80,lal.reg.80years)
Sys.sleep(300)
ind.years(reg.det.80,det.reg.80years)
Sys.sleep(300)
ind.years(reg.chi.90,chi.reg.90years)
Sys.sleep(300)
ind.years(reg.hou.90,hou.reg.90years)
Sys.sleep(300)
ind.years(reg.uta.90,uta.reg.90years)
Sys.sleep(300)
ind.years(reg.bos.00,bos.reg.00years)
Sys.sleep(300)
ind.years(reg.det.00,det.reg.00years)
Sys.sleep(300)
ind.years(reg.lal.00,lal.reg.00years)
Sys.sleep(300)
ind.years(reg.sas.00,sas.reg.00years)
Sys.sleep(300)
ind.years(reg.cle.10,cle.reg.10years)
Sys.sleep(300)
ind.years(reg.gsw.10,gsw.reg.10years)
Sys.sleep(300)
ind.years(reg.mia.10,mia.reg.10years)
Sys.sleep(300)
ind.years(reg.sas.10,sas.reg.10years)
Sys.sleep(300)


ind.years(post.bos.80,bos.post.80years)
Sys.sleep(300)
ind.years(post.lal.80,lal.post.80years)
Sys.sleep(300)
ind.years(post.det.80,det.post.80years)
Sys.sleep(300)
ind.years(post.chi.90,chi.post.90years)
Sys.sleep(300)
ind.years(post.hou.90,hou.post.90years)
Sys.sleep(300)
ind.years(post.uta.90,uta.post.90years)
Sys.sleep(300)
ind.years(post.bos.00,bos.post.00years)
Sys.sleep(300)
ind.years(post.det.00,det.post.00years)
Sys.sleep(300)
ind.years(post.lal.00,lal.post.00years)
Sys.sleep(300)
ind.years(post.sas.00,sas.post.00years)
Sys.sleep(300)
ind.years(post.cle.10,cle.post.10years)
Sys.sleep(300)
ind.years(post.gsw.10,gsw.post.10years)
Sys.sleep(300)
ind.years(post.mia.10,mia.post.10years)
Sys.sleep(300)
ind.years(post.sas.10,sas.post.10years)
Sys.sleep(300)


