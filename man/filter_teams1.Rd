% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_teams1_wrapper.R
\name{filter_teams1}
\alias{filter_teams1}
\title{Title}
\usage{
filter_teams1(first, last, part, team1, team2, team3)
}
\arguments{
\item{first}{the first year desired}

\item{last}{the last year desired}

\item{part}{which part of the season desired}

\item{team1}{first team wanted}

\item{team2}{second team wanted}

\item{team3}{third team wanted}
}
\value{
returns all of the players stats from the desired teams in the decade
}
\description{
Title
}
\examples{
data("playoff_games")
data("regular_season")
data("names")

names(playoff_games) <- names
names(regular_season) <- names

three_teams_80s <- filter_teams1(1980,1990,regular_season,'BOS','DET','LAL')
}
