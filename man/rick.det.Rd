% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ricker.R
\name{rick.det}
\alias{rick.det}
\title{deterministic Ricker model for simulating population growth in a community.}
\usage{
rick.det(nSpp, tMax, r, N, K)
}
\arguments{
\item{nSpp}{The number of species to be simulated}

\item{tMax}{The number of time steps to include in the simulation}

\item{r}{Vector (length nSpp) of the average intrinsic growth rate for each species}

\item{N}{Matrix (nrow=tMax, ncol=nSpp) to store abundances; N[1,] must be initialized}

\item{K}{Vector (length nSpp) of the carrying capacity for each species}
}
\value{
Updated matrix N with simulated abundances
}
\description{
This function generates deterministic population abundances for a nSpp species.
}
\examples{
nSpp <- 3; tMax <- 20
r <- runif(3, 0.75, 1.25)
K <- runif(3, 100, 200)
N <- matrix(nrow=tMax, ncol=nSpp)
N[1,] <- K
N <- rick.det(nSpp, tMax, r, N, K)

}
\keyword{Ricker,}
\keyword{deterministic}
\keyword{simulation,}
