#' deterministic Ricker model for simulating population growth in a community.
#'
#' This function generates deterministic population abundances for a nSpp species.
#' @param nSpp The number of species to be simulated
#' @param tMax The number of time steps to include in the simulation
#' @param r Vector (length nSpp) of the average intrinsic growth rate for each species
#' @param N Matrix (nrow=tMax, ncol=nSpp) to store abundances; N[1,] must be initialized
#' @param K Vector (length nSpp) of the carrying capacity for each species
#' @return Updated matrix N with simulated abundances
#' @keywords Ricker, simulation, deterministic
#' @export
#' @examples
#' nSpp <- 3; tMax <- 20
#' r <- runif(3, 0.75, 1.25)
#' K <- runif(3, 100, 200)
#' N <- matrix(nrow=tMax, ncol=nSpp)
#' N[1,] <- K
#' N <- rick.det(nSpp, tMax, r, N, K)
#' 
rick.det <- function(nSpp, tMax, r, N, K) {
  # values: nSpp, tMax
  # vectors: r, K (length nSpp)
  # matrix: N (nrow=tMax, ncol=nSpp)
  
  for(t in 2:tMax) {
    r.t <- r*(1 - N[t-1,]/K)
    N[t,] <- N[t-1,]*exp(r.t)
  }
  return(N)
}





#' stochastic Ricker model for simulating population growth in a community.
#'
#' This function generates stochastic population abundances for a nSpp species.
#' @param nSpp The number of species to be simulated
#' @param tMax The number of time steps to include in the simulation
#' @param r Vector (length nSpp) of the average intrinsic growth rate for each species
#' @param N Matrix (nrow=tMax, ncol=nSpp) to store abundances; N[1,] must be initialized
#' @param K Vector (length nSpp) of the carrying capacity for each species
#' @param sd Vector (length nSpp) of the standard deviation in annual growth rate for each species
#' @return Updated matrix N with simulated abundances
#' @keywords Ricker, simulation, deterministic
#' @export
#' @examples
#' nSpp <- 3; tMax <- 20
#' r <- runif(3, 0.75, 1.25)
#' sd <- runif(3, 0.001, 2)
#' K <- runif(3, 100, 200)
#' N <- matrix(nrow=tMax, ncol=nSpp)
#' N[1,] <- K
#' N <- rick.stoch(nSpp, tMax, r, N, K, sd)
#' 
rick.stoch <- function(nSpp, tMax, r, N, K, sd) {
  # values: nSpp, tMax
  # vectors: r, K, sd (length nSpp)
  # matrix: N (nrow=tMax, ncol=nSpp)
  
  for(t in 2:tMax) {
    r.t <- rnorm(nSpp, r*(1 - N[t-1,]/K), sd)
    N[t,] <- N[t-1,]*exp(r.t)
  }
  return(N)
}