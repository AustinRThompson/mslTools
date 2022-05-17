#' Euclidean Distance
#' @description This function returns a data frame containing a column of the calculated Euclidean distance.
#'
#' @param x1 x of the first coordinate pair
#' @param y1 y of the first coordinate pair
#' @param x2 x of the second coordinate pair
#' @param y2 y of the second coordinate pair
#'
#' @return This function returns a data frame containing a column of the calculated Euclidean distance
#' @export eucDist
#'
#' @examples
library(tidyverse)
eucDist <- function(x1,y1,x2,y2) {
  df <- data.frame(x1,y1,x2,y2) %>%
    dplyr::mutate(eucDist = base::sqrt((x2 - x1)^2 + (y2 - y1)^2)) %>%
    dplyr::select(eucDist)

  return(df)
}
