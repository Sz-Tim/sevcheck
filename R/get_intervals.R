#' Summarise a dataframe to mean, medians, and CIs
#'
#' @param df Dataframe; if grouped, output is by group
#' @param y Variable to summarise
#' @param type Type of CI; either "hdci" or "qi", using the corresponding function from tidybayes
#'
#' @return Summarised dataframe with columns mn, med, L025, L05, L10, L25, L75, L90, L95, and L975, and either a single row or one row per group
#' @export
get_intervals <- function(df, y, type="hdci") {
  library(tidybayes)
  
  ci_fun <- switch(type,
                   "hdci"=hdci,
                   "qi"=qi)
  df |>
    summarise(mn=mean({{y}}),
              med=median({{y}}),
              L025=ci_fun({{y}}, 0.95)[,1],
              L05=ci_fun({{y}}, 0.9)[,1],
              L10=ci_fun({{y}}, 0.8)[,1],
              L25=ci_fun({{y}}, 0.5)[,1],
              L75=ci_fun({{y}}, 0.5)[,2],
              L90=ci_fun({{y}}, 0.8)[,2],
              L95=ci_fun({{y}}, 0.9)[,2],
              L975=ci_fun({{y}}, 0.95)[,2])
}
