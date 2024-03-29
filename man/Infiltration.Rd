% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Infiltration.R
\name{Infiltration}
\alias{Infiltration}
\title{Infiltrate incoming water (rainfall and irrigation)}
\usage{
Infiltration(Soil, InitCond, Infl, Irr, IrrMngt, FieldMngt, FluxOut,
  DeepPerc0, Runoff0)
}
\arguments{
\item{Soil}{properties of soil}

\item{InitCond}{Crop setting initial structure}

\item{Infl}{Infiltration}

\item{Irr}{Irrigation}

\item{IrrMngt}{Irrigation management}

\item{FieldMngt}{Field management}

\item{FluxOut}{Flux}

\item{DeepPerc0}{Deep percolation}

\item{Runoff}{water Runoff}
}
\value{
list with \code{NewCond}, \code{DeepPerc}, \code{RunoffTot}, \code{Infl} and \code{FluxOut} for a n time-step.
}
\description{
Infiltrate incoming water (rainfall and irrigation)
}
\examples{
Infiltration(Soil, InitCond, Infl, Irr, IrrMngt, FieldMngt, FluxOut, DeepPerc0, Runoff0)
}
