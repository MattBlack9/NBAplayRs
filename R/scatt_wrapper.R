library(tidyverse)
library(ggplot2)
#' Title
#'
#' @param data the dataframe of the team interested
#' @param years the years that is wanted to see
#' @param mine the plot's title
#'
#' @return scatterplot of a teams perfomance comparing the adjusted vorp of the regular season to the post season
#' @export
#'
#' @examples
#' load("data/heat.rda")
#' 
#' scatt(heat)

scatt <- function(data,years = NULL, mine = NULL){
  if(is.null(mine) == TRUE){
    mine <- 'Team Performance'
  }
  if(is.null(years) == TRUE){
    ggplot(data)+
      geom_point(aes(x=RVORP,y=PVORP,color=win)) +
      scale_color_manual(name='Champions',values = c('black','firebrick3'),labels=c('No','Yes')) +
      geom_abline(intercept = 0,slope=1, color='blue')+ 
      labs(title=mine,x = 'Regular Season Adjusted VORP',y='Post Season Adjusted VORP',subtitle = 'Performance with y=x line')+
      xlim(-5,15)+
      ylim(-5,15)
  }
  else{
    temp <- filter(data, year == years)
    if(sum(temp$win) > 0){
      colors <- 'firebrick3'
      champ <- 'Yes'
    }
    else{
      colors <- 'black'
      champ <- 'No'
    }
    ggplot(temp)+
      geom_point(aes(x=RVORP,y=PVORP,color=win)) +
      scale_color_manual(name='Champions',values =colors,labels=champ) +
      geom_abline(intercept = 0,slope=1, color='blue') + 
      labs(title='Team Performance',x = 'Regular Season VORP',y='Post Season VORP',subtitle = 'Performance with y=x line')+
      xlim(-5,15)+
      ylim(-5,15)
  }
}
