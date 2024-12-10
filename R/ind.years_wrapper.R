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
#' @param list the team that is wanted to get the adjusted vorp
#' @param years vector of names for the csv's, one per year
#'
#' @return the adjusted vorp of players
#' @export
#'
#' @examples
#' load("data/playoff_games.rda")
#' load("data/regular_season.rda")
#' load("data/names.rda")
#' source("R/filter_teams1_wrapper.R")
#' source("R/cleaning_wrapper.R")
#' source("R/vorp_wrapper.R")
#' 
#' names(playoff_games) <- names
#' names(regular_season) <- names
#' 
#' three_teams_80s <- filter_teams1(1980,1990,regular_season,'BOS','DET','LAL')
#' 
#' output <- list()
#' boston <- cleaning(three_teams_80s, 'BOS', output)
#' bos.reg.80years <- c('celtics.reg85.csv','celtics.reg88.csv','celtics.reg89.csv','celtics.reg86.csv','celtics.reg87.csv')
#' 
#' boston <- ind.years(boston, bos.reg.80years)

ind.years <- function(list,years){
  holder <- 1
  for(i in list){
    temp <- vorp(i)
    write_csv(temp,years[holder])
    holder <- holder + 1
  }
}
