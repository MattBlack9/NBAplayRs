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
#' @param year dataframe of team
#' @param time year
#' @param team team and year
#' @param reg_post regular or post season
#'
#' @return returns VORP of every player for a single team
#' @export
#'
#' @examples
#' data("reg_bos_80")
#' 
#' test <- reg.bos.80[[1]][1,]
#' vor <- vorp(test)
#' 
#' bind(vor, 1985, 'BOS-1985', 'regular')


bind <- function(year, time, team, reg_post){
  df <- filter(year, season_id==team)
  new <- data.frame()
  for(i in unique(df$player)){
    per <- filter(df,player==i)
    score <- mean(as.numeric(per$VORP))
    adder <- data.frame(player = i, TVORP = score, year = time, time = reg_post)
    new <- rbind(new,adder)
  }
  return(new)
}