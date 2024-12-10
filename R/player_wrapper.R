library(tidyverse)
library(ggplot2)
#' Title
#'
#' @param play the desired player
#' @param team the team wanted
#' @param titles the title of the graph 
#'
#' @return a plot comparing a player's regular season to post season
#' @export
#'
#' @examples
#' data("lakers")
#' 
#' player('abdulka01',lakers)

player <- function(play, team, titles = "Player Performance"){
  temp <- filter(team, player == play)
  minyear <- min(temp$year)
  ggplot(temp) +
    geom_point(aes(x = year, y= RVORP,color = 'Regular Season')) +
    geom_point(aes(x = year, y= PVORP,color = 'Post Season')) +
    geom_line(aes(x = year,y=RVORP,color = 'Regular Season')) +
    geom_line(aes(x = year,y=PVORP,color = 'Post Season')) +
    labs(title=titles,x = 'Year',y='Time Adjusted VORP',subtitle = 'Regular Season Adjusted VORP vs Post Season Adjusted VORP')+
    scale_color_manual(name='Time',values = c('#0072B2','#D55E00'))+
    scale_x_continuous(breaks = seq(min(temp$year), max(temp$year), by = 1))+
    ylim(-5,15)
}
