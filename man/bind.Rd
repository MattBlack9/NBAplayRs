% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_wrapper.R
\name{bind}
\alias{bind}
\title{Title}
\usage{
bind(year, time, team, reg_post)
}
\arguments{
\item{year}{dataframe of team}

\item{time}{year}

\item{team}{team and year}

\item{reg_post}{regular or post season}
}
\value{
returns VORP of every player for a single team
}
\description{
Title
}
\examples{
data("reg_bos_80")

test <- reg.bos.80[[1]][1,]
vor <- vorp(test)

bind(vor, 1985, 'BOS-1985', 'regular')
}
