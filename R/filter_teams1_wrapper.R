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
#' @param first the first year desired
#' @param last the last year desired
#' @param part which part of the season desired
#' @param team1 first team wanted
#' @param team2 second team wanted
#' @param team3 third team wanted
#'
#' @return returns all of the players stats from the desired teams in the decade
#' @export
#'
#' @examples
#' load("data/playoff_games.rda")
#' load("data/regular_season.rda")
#' load("data/names.rda")
#' 
#' names(playoff_games) <- names
#' names(regular_season) <- names
#' 
#' three_teams_80s <- filter_teams1(1980,1990,regular_season,'BOS','DET','LAL')

filter_teams1 <- function(first,last,part,team1,team2,team3){
  temp <- part$year >= first & part$year < last
  test <- part
  test$correct <- temp
  temp2 <- part$`Visitor/Neutral` == team1 | part$`Home/Neutral` == team1 | part$`Visitor/Neutral` == team2 | part$`Home/Neutral` == team2 | part$`Visitor/Neutral` == team3 | part$`Home/Neutral` == team3 
  test$cteams <- temp2
  df <- filter(test,correct == T & cteams == T)
  return(df)
}

