library(tidyverse)
library(ggplot2)
#' Title
#'
#' @param player1 the first player wanting to be compared
#' @param player2 the second player wanting to be compared
#' @param team1 the first player's team
#' @param team2 the second player's team
#' @param player3 the third player wanting to be compared
#' @param team3 the third player's team
#' @param player4 the fourth player wanting to be compared
#' @param team4 the fourth player's team
#'
#' @return a line graph comparing two player's regular seasons
#' @export
#'
#' @examples
#' load("data/jazz.rda")
#' load("data/warriors.rda")
#' 
#' reg('curryst01','malonka01',warriors,jazz)

reg <- function(player1,player2,team1,team2,player3=NULL,team3=NULL,player4=NULL,team4=NULL){
  if(is.null(player3) == TRUE){
    temp1 <- filter(team1,player==player1)
    temp2 <- filter(team2,player==player2)
    temp1$xax <- seq_along(temp1[, 1])
    temp2$xax <- seq_along(temp2[, 1])
    
    df <- data.frame()
    df <- rbind(df,temp1,temp2)
    ggplot(df)+
      geom_point(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_point(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_line(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_line(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      labs(x='Years made the Playoffs',y='Regular Season VORP')+
      scale_color_manual(name='Players',values = c('#0072B2','#D55E00'),labels=c(player1,player2))+
      scale_x_continuous(breaks = seq(1, 10, by = 1))+
      ylim(-5,15)
  }
  else if(is.null(player3) == FALSE & is.null(player4) == TRUE){
    temp1 <- filter(team1,player==player1)
    temp2 <- filter(team2,player==player2)
    temp3 <- filter(team3,player==player3)
    temp1$xax <- seq_along(temp1[, 1])
    temp2$xax <- seq_along(temp2[, 1])
    temp3$xax <- seq_along(temp3[, 1])
    
    df <- data.frame()
    df <- rbind(df,temp1,temp2,temp3)
    ggplot(df)+
      geom_point(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_point(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_line(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_line(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_point(aes(x = xax, y=RVORP, color = player3), data = filter(df, player==player3)) +
      geom_line(aes(x = xax, y = RVORP, color = player3), data = filter(df, player==player3)) + 
      labs(x='Years made the Playoffs',y='Regular Season VORP')+
      scale_color_manual(name='Players',values = c('#0072B2','#D55E00','#CC79A7'),labels=c(player1,player2, player3))+
      scale_x_continuous(breaks = seq(1, 10, by = 1))+
      ylim(-5,15)
  }
  else{
    temp1 <- filter(team1,player==player1)
    temp2 <- filter(team2,player==player2)
    temp3 <- filter(team3,player==player3)
    temp4 <- filter(team4,player==player4)
    temp1$xax <- seq_along(temp1[, 1])
    temp2$xax <- seq_along(temp2[, 1])
    temp3$xax <- seq_along(temp3[,1])
    temp4$xax <- seq_along(temp4[,1])
    
    df <- data.frame()
    df <- rbind(df,temp1,temp2,temp3,temp4)
    ggplot(df)+
      geom_point(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_point(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_line(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_line(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_point(aes(x = xax, y= RVORP,color = player3), data = filter(df,player==player3)) +
      geom_line(aes(x = xax, y= RVORP, color = player3),data = filter(df,player==player3)) +
      geom_point(aes(x = xax, y= RVORP,color = player4), data = filter(df,player==player4)) +
      geom_line(aes(x = xax, y= RVORP, color = player4),data = filter(df,player==player4)) +
      labs(x='Years made the Playoffs',y='Regular Season VORP')+
      scale_color_manual(name='Players',values = c('#0072B2','#D55E00','#CC79A7','#009E73'),labels=c(player4,player1,player2,player3))+
      scale_x_continuous(breaks = seq(1, 10, by = 1))+
      ylim(-5,15)
  }
}
