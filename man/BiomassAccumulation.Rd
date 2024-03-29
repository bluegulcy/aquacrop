% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BiomassAccumulation.R
\name{BiomassAccumulation}
\alias{BiomassAccumulation}
\title{Calculate biomass accumulation (g m-2)}
\usage{
BiomassAccumulation(Crop, InitCond, Tr, TrPot, Et0, Tmax, Tmin, GDD,
  GrowingSeason)
}
\arguments{
\item{Crop}{Parameters for a given crop}

\item{InitCond}{Crop setting initial structure}

\item{Tr}{actual transpiration}

\item{TrPot}{potential transpiration}

\item{Et0}{Evapotranspiration}

\item{Tmax}{max temp for n time-step}

\item{Tmin}{min temp for n time-step}

\item{GDD}{Growing degree days}

\item{GrowingSeason}{crop developmental stage}
}
\value{
\code{NewCond} for a n time-step.
}
\description{
Calculate biomass accumulation (g m-2)
}
\examples{
BiomassAccumulation(Crop, InitCond, Tr, TrPot, Et0, Tmax, Tmin, GDD, GrowingSeason)
}
