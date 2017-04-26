#' interpolate species richness for each elevational band
#'
#' This function interpolates species, genus, and subfamily richness for a 
#' sequence of elevational bands based on the range limits of a set of species.
#' @param sp.df Dataframe with columns 'Binomial', 'LowEl', 'HighEl'
#' @param els Vector of elevations 
#' @return Returns a list with items "sp", "g", and "sf" which each contain a 
#' vector of interpolated richness values
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
sp.band <- function(sp.df, els) {
  
  sp.S <- rep(NA, length(els)); names(sp.S) <- els
  g.S <- sp.S
  sf.S <- sp.S
  for(i in 1:length(els)) {
    sp.pres <- droplevels(subset(sp.df, ( (sp.df$LowEl < (els[i] + 100)) & 
                                            (sp.df$HighEl >= els[i]) )))
    sp.S[i] <- nlevels(sp.pres$Binomial)
    g.S[i] <- nlevels(sp.pres$Genus)
    sf.S[i] <- nlevels(sp.pres$Subfamily)
  }
  return(list("sp"=sp.S, "g"=g.S, "sf"=sf.S))
}



