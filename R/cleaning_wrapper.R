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

#' Title
#'
#' @param decade the years wanted
#' @param team the team wanted
#' @param list an empty list to fill
#'
#' @return the list of players from specific team wanted
#' @export
#'
#' @examples
#' load("data/playoff_games.rda")
#' load("data/regular_season.rda")
#' load("data/names.rda")
#' source("R/filter_teams1_wrapper.R")
#' 
#' names(playoff_games) <- names
#' names(regular_season) <- names
#' 
#' three_teams_80s <- filter_teams1(1980,1990,regular_season,'BOS','DET','LAL')
#' 
#' output <- list()
#' boston <- cleaning(three_teams_80s, 'BOS', output)


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


