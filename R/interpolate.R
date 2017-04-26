#' interpolate species richness for each elevational band
#'
#' This function interpolates species richness for a sequence of elevational bands based on the range limits of a set of species.
#' @param sp.df Dataframe with columns 'Binomial', 'LowEl', 'HighEl'
#' @param els Vector of elevations 
#' @keywords richness, elevation, interpolate
#' @export
#' @examples
#' sp.df <- data.frame(Binomial=LETTERS[1:10],
#'                     LowEl=runif(10, 0, 1000))
#' sp.df$HighEl <- sp.df$LowEl + runif(10, 0, 1000)
#' els <- seq(0, 2000, by=100)
#' sp.band(sp.df, els)
#' 
sp.band <- function(sp.df, els) {
  
  sp.count <- rep(NA, length(els))
  for(i in 1:length(els)) {
    sp.pres <- droplevels(subset(sp.df, ( (sp.df$LowEl < (els[i] + 100)) & 
                                            (sp.df$HighEl >= els[i]) )))
    sp.count[i] <- nlevels(sp.pres$Binomial)
  }
  return(sp.count)
}





#' interpolate genus richness for each elevational band
#'
#' This function interpolates genus richness for a sequence of elevational bands based on the range limits of a set of species.
#' @param sp.df Dataframe with columns 'Genus', 'LowEl', 'HighEl'
#' @param els Vector of elevations 
#' @keywords richness, elevation, interpolate
#' @export
#' @examples
#' sp.df <- data.frame(Binomial=LETTERS[1:10],
#'                     Genus=rep(letters[1:3], times=c(2,7,1)),
#'                     LowEl=runif(10, 0, 1000))
#' sp.df$HighEl <- sp.df$LowEl + runif(10, 0, 1000)
#' els <- seq(0, 2000, by=100)
#' g.band(sp.df, els)
#' 
g.band <- function(sp.df, els) {
  
  g.count <- rep(NA, length(els))
  for(i in 1:length(els)) {
    sp.pres <- droplevels(subset(sp.df, ( (sp.df$LowEl < (els[i] + 100)) & 
                                            (sp.df$HighEl >= els[i]) )))
    g.count[i] <- nlevels(sp.pres$Genus)
  }
  return(g.count)
}






#' interpolate subfamily richness for each elevational band
#'
#' This function interpolates subfamily richness for a sequence of elevational bands based on the range limits of a set of species.
#' @param sp.df Dataframe with columns 'Binomial', 'LowEl', 'HighEl'
#' @param els Vector of elevations 
#' @keywords richness, elevation, interpolate
#' @export
#' @examples
#' sp.df <- data.frame(Binomial=LETTERS[1:10],
#'                     Genus=rep(letters[1:3], times=c(2,7,1)),
#'                     Subfamily=rep(c("SF1", "SF2", times=c(2,8))),
#'                     LowEl=runif(10, 0, 1000))
#' sp.df$HighEl <- sp.df$LowEl + runif(10, 0, 1000)
#' els <- seq(0, 2000, by=100)
#' sp.band(sp.df, els)
#' 
sf.band <- function(sp.df, els) {
  
  sf.count <- rep(NA, length(els))
  for(i in 1:length(els)) {
    sp.pres <- droplevels(subset(sp.df, ( (sp.df$LowEl < (els[i] + 100)) & 
                                            (sp.df$HighEl >= els[i]) )))
    sf.count[i] <- nlevels(sp.pres$Subfamily)
  }
  return(sf.count)
}