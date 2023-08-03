#' Lag multiple variables at once
#'
#' From https://stackoverflow.com/questions/55814028/multiple-lags-with-dplyr
#'
#' @param data Dataframe
#' @param ... Unquoted variable names to lag
#' @param n Number of lags
#'
#' @return Dataframe with new lagged columns added
#' @export
get_lags <- function(data, ..., n=2){
  library(tidyverse); library(rlang)
  variable <- enquos(...)
  
  indices <- seq_len(n)
  combos <- crossing(indices, var=as.list(variable))
  
  quosures <- map2(combos$indices, combos$var,
                   ~quo(lag(!!.y, !!.x)) ) |>
    set_names(paste0(map_chr(combos$var, quo_text), combos$indices))
  mutate(data, !!!quosures )
  
}