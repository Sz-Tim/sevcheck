#' Add columns lon and lat from an sf object
#' 
#' I don't think there's a native option for this
#'
#' @param x an sf object
#' @param drop_geom {FALSE} run st_drop_geometry() to return only a tibble?
#'
#' @return sf object or tibble with added columns 'lon' and 'lat'
#' @export
add_lonlat <- function(x, drop_geom=FALSE) {
  library(sf); library(magrittr)
  x_ <- x %>%
    mutate(lon=st_coordinates(.)[,1],
           lat=st_coordinates(.)[,2])
  if(drop_geom) {
    x_ <- x_ |> st_drop_geometry()
  }
  return(x_)
}
