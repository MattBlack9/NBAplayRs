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
#' @param df dataframe of the team and players
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
#' 
#' names(playoff_games) <- names
#' names(regular_season) <- names
#' 
#' three_teams_80s <- filter_teams1(1980,1990,regular_season,'BOS','DET','LAL')
#' 
#' output <- list()
#' boston <- cleaning(three_teams_80s, 'BOS', output)
#' 
#' bos_vorp <- vorp(boston[[1]])


vorp <- function(df){
  stats <- data.frame(NULL)
  for(g in 1:nrow(df)){
    id <- df$id[g]
    team <- df$team[g]
    node <- paste0("table#box-", team, "-game-advanced > tbody > tr > td")
    node2 <- paste0("table#box-", team, "-game-advanced > tbody > tr > th")
    url <- paste0("https://www.basketball-reference.com/boxscores/", id, ".html")
    webpage <- read_html(url)
    vorp <- webpage %>% 
      html_nodes(node) %>% 
      html_attr("data-tip") %>%
      data.frame()
    vorp <- vorp[-which(is.na(vorp[,1])),]
    vorp <- data.frame(vorp)
    colnames(vorp) <- "VORP"
    for(i in 1:nrow(vorp)){
      vorp$VORP[i] <- as.numeric(substr(vorp$VORP[i],
                                        unlist(gregexpr("VORP", vorp$VORP[i]))[1] + 6,
                                        unlist(gregexpr("VORP", vorp$VORP[i]))[2] - 17))
    }
    
    min <- webpage %>% 
      html_nodes(node) %>% 
      html_text() %>%
      data.frame()
    colnames(min) <- "min"
    elim <- which(min$min == "Did Not Play")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Did Not Dress")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Inactive")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Not With Team")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    elim <- which(min$min == "Player Suspended")
    if(length(elim) > 0){  
      min <- min[-elim,]
      min <- data.frame(min)
      colnames(min) <- "min"
    }
    min <- min[which(unlist(gregexpr(":", min$min)) > 0),]
    min <- data.frame(min)
    for(i in 1:nrow(min)){
      min$sec[i] <- as.numeric(substr(min$min[i],
                                      nchar(min$min[i]) - 1,
                                      nchar(min$min[i])))
      min$min[i] <- as.numeric(substr(min$min[i],
                                      1 ,
                                      nchar(min$min[i]) - 3))
      if(is.na(min$sec[i])){
        min$sec[i] <- 0
      }
    }
    min$min <- as.numeric(min$min)
    min$min <- min$min + (min$sec / 60)
    min <- min %>%
      select(1)
    
    player <- webpage %>% 
      html_nodes(node2) %>% 
      html_attr("data-append-csv") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    player <- player[-which(is.na(player[,1])),]
    player <- player[c(1:nrow(min))]
    
    
    adder <- cbind(player, min, vorp)
    adder <- adder[order(min, decreasing = TRUE),]
    adder$id <- paste0(id, "-", team)
    if(team == df$`Home/Neutral`[g]){
      adder$opp_id <- paste0(id, "-", df$`Visitor/Neutral`[g])
    } else {
      adder$opp_id <- paste0(id, "-", df$`Home/Neutral`[g])
    }
    adder$season_id <- df$season_id[g]
    adder$win <- df$win[g]
    adder$year <- df$year[g]
    adder$adj_VORP <- as.numeric(adder$VORP) * as.numeric(adder$min)
    adder$avg_VORP <- mean(adder$adj_VORP)
    
    stats <- rbind(stats, adder)
    
    print(paste0(round((g/nrow(df)) * 100, 3),"%"))
    Sys.sleep(5)
  }
  stats$link <- paste0(stats$player,"-",stats$year)
  
  return(stats)
}




