% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UpdateCCxCDC.R
\name{UpdateCCxCDC}
\alias{UpdateCCxCDC}
\title{Update CCx and CDC parameter valyes for rewatering in late
season of an early declining canopy}
\usage{
UpdateCCxCDC(CCprev, CDC, CCx, dt)
}
\arguments{
\item{CCprev}{Prev Canopy Cover}

\item{CDC}{Canopy decline coefficient}

\item{CCx}{Maximum canopy cover}

\item{dt}{dt parameter}
}
\value{
list with \code{CCXadj} and \code{CDCadj} for a n time-step.
}
\description{
Update CCx and CDC parameter valyes for rewatering in late
season of an early declining canopy
}
\examples{
UpdateCCxCDC(CCprev,CDC,CCx,dt)
}
