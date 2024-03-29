% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Irrigation.R
\name{Irrigation}
\alias{Irrigation}
\title{Get irrigation depth for current day}
\usage{
Irrigation(InitCond, IrrMngt, Crop, Soil, ClockStruct, GrowingSeason, P,
  Runoff)
}
\arguments{
\item{InitCond}{Crop setting initial structure}

\item{IrrMngt}{Irrigation management}

\item{Crop}{parameters for a given crop}

\item{Soil}{properties of soil}

\item{ClockStruct}{crop calendar}

\item{GrowingSeason}{crop developmental stage}

\item{P}{precipitation}

\item{Runoff}{water Runoff}
}
\value{
list with \code{NewCond} and \code{Irr} for a n time-step.
}
\description{
Get irrigation depth for current day
}
\examples{
Irrigation(InitCond, IrrMngt, Crop, Soil, ClockStruct, GrowingSeason, P, Runoff)
}
