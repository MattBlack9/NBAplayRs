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
#' @param list 
#' @param years
#'
#' @return the adjusted vorp of players
#' @export
#'
#' @examples
#' 

ind.years <- function(list,years){
  holder <- 1
  for(i in list){
    temp <- vorp(i)
    write_csv(temp,years[holder])
    holder <- holder + 1
  }
}