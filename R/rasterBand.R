#' zonal statistics: internal function for summarizing within zones
#'
#' This function generates a matrix summing the number of cells for each zone-value
#' combination based on a zonal raster and a value raster.
#' @param zone.r zonal raster
#' @param value.r value raster to be summarized within zones
#' @return matrix (rows = unique value, columns = zones, cells = sums)
#' @keywords raster, zonal, zone
#' @export

zone.freqmx <- function(zone.r, value.r) {
  zone.l <- layerize(zone.r) # layerize zones
  value.r <- projectRaster(value.r, zone.r) # ensure projections are the same
  value.l <- value.r * zone.l # separate values by zone
  freq.l <- freq(value.l, merge=TRUE, useNA="no") # calculate frequencies
  m.l <- as.matrix(freq.l[,-1])  # convert to matrix
  rownames(m.l) <- freq.l$value
  colnames(m.l) <- substr(names(zone.l), 2, max(nchar(names(zone.l))))
  return(m.l)
}








#' zonal statistics: Shannon H for raster values within zones
#'
#' This function calculates 
#' @param zone.r zonal raster
#' @param value.r value raster to be summarized within zones
#' @param exclude.anthro Logical; should NLCD anthropogenic habitats be excluded? Defaults to FALSE
#' @param na.val NA value in value raster; defaults to "na"
#' @return named vector (names = zones, values = Shannon H)
#' @keywords raster, diversity, Shannon H, zonal, zone
#' @export

zone.H <- function(zone.r, value.r, exclude.anthro=FALSE, na.val="na") {
  m.l <- zone.freqmx(zone.r, value.r)
  if(exclude.anthro) {
    m.l <- m.l[!rownames(m.l) %in% c(11,12,23,24),]
  }
  m.l <- m.l[rownames(m.l) != na.val,]
  m.l[is.na(m.l)] <- 0
  H <- diversity(m.l, MARGIN=2)
  return(H)
}





#' zonal statistics: mean of raster values within zones
#'
#' This function sums raster values within zones
#' @param zone.r zonal raster
#' @param value.r value raster to be summarized within zones
#' @param exclude.anthro Logical; should NLCD anthropogenic habitats be excluded? Defaults to FALSE
#' @param na.val NA value in value raster; defaults to "na"
#' @return named vector (names = zones, values = mean of cell values)
#' @keywords raster, sum, total, zonal, zone
#' @export

zone.mean <- function(zone.r, value.r, exclude.anthro=FALSE, na.val="na") {
  m.l <- zone.freqmx(zone.r, value.r)
  if(exclude.anthro) {
    m.l <- m.l[!rownames(m.l) %in% c(11,12,23,24),]
  }
  m.l <- m.l[rownames(m.l) != na.val,]
  z.mn <- colSums(m.l*as.numeric(rownames(m.l)), na.rm=TRUE)/colSums(m.l, na.rm=TRUE)
  return(z.mn)
}








#' zonal statistics: sum of raster values within zones
#'
#' This function sums raster values within zones
#' @param zone.r zonal raster
#' @param value.r value raster to be summarized within zones
#' @param exclude.anthro Logical; should NLCD anthropogenic habitats be excluded? Defaults to FALSE
#' @param na.val NA value in value raster; defaults to "na"
#' @return named vector (names = zones, values = sum of cell values)
#' @keywords raster, sum, total, zonal, zone
#' @export

zone.sum <- function(zone.r, value.r, exclude.anthro=TRUE, na.val="na") {
  m.l <- zone.freqmx(zone.r, value.r)
  if(exclude.anthro) {
    m.l <- m.l[!rownames(m.l) %in% c(11,12,23,24),]
  }
  m.l <- m.l[rownames(m.l) != na.val,]
  z.sum <- colSums(m.l*as.numeric(rownames(m.l)), na.rm=TRUE)
  return(z.sum)
}

