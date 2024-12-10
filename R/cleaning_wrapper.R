library(dplyr)
library(tidyr)
library(ggplot2)

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
#' data("playoff_games")
#' data("regular_season")
#' data("names")
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


