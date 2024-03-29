% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CheckGroundwaterTable.R
\name{CheckGroundwaterTable}
\alias{CheckGroundwaterTable}
\title{Check for presence of a groundwater table, and, if present,
to adjust compartment water contents and field capacities where
necessary}
\usage{
CheckGroundwaterTable(Soil, Groundwater, InitCond, ClockStruct)
}
\arguments{
\item{Soil}{structure of Soil}

\item{Groundwater}{ground water table}

\item{InitCond}{Crop setting initial structure}

\item{ClockStruct}{Model time settings}
}
\value{
\code{NewCond} model values for n time-step
}
\description{
Check for presence of a groundwater table, and, if present,
to adjust compartment water contents and field capacities where
necessary
}
\examples{
CheckGroundwaterTable(Soil, Groundwater, InitCond)
}
