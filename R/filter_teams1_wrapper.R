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
#' @param first 
#' @param last
#' @param part
#' @param team1
#' @param team2
#' @param team3
#'
#' @return the adjusted vorp of players
#' @export
#'
#' @examples

filter_teams1 <- function(first,last,part,team1,team2,team3){
  temp <- part$year >= first & part$year < last
  test <- part
  test$correct <- temp
  temp2 <- part$`Visitor/Neutral` == team1 | part$`Home/Neutral` == team1 | part$`Visitor/Neutral` == team2 | part$`Home/Neutral` == team2 | part$`Visitor/Neutral` == team3 | part$`Home/Neutral` == team3 
  test$cteams <- temp2
  df <- filter(test,correct == T & cteams == T)
  return(df)
}