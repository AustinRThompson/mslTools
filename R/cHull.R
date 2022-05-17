#' Convex Hull
#' @description This function returns the convex hull area for the set of points provided.
#'
#' @param x x of the coordinate pair
#' @param y y of the coordinate pair
#'
#' @return This function returns the convex hull area for the set of points provided.
#' @export cHull
#'
#' @examples
library(tidyverse)
cHull <- function(x,y) {
  xyCoords <- base::cbind(x,y) %>%
    base::as.data.frame() %>%
    dplyr::rename(x = 1,
                  y = 2) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::filter(!is.na(y))

  chullCoords <- grDevices::chull(x = xyCoords %>% dplyr::pull(x),
                           y = xyCoords %>% dplyr::pull(y)) %>%
    c(., .[1]) %>%
    xyCoords[.,]

  if (base::NROW(chullCoords) < 4) {
    cHull <- NA
  } else {
    cHull <- sp::Polygon(chullCoords, hole=F) %>%
      .@area
  }
  return(cHull)
}
