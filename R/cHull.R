#' Convex Hull
#' @description This function returns the convex hull area for the set of points provided.
#'
#' @param x x of the coordinate pair
#' @param y y of the coordinate pair
#'
#' @return This function returns the convex hull area for the set of points provided.
#' @export eucDist
#'
#' @examples
cHull <- function(x,y) {
  xyCoords <- cbind(x,y) %>%
    as.data.frame() %>%
    dplyr::rename(x = 1,
                  y = 2) %>%
    dplyr::filter(!is.na(x)) %>%
    dplyr::filter(!is.na(y))

  chullCoords <- grDevices::chull(x = xyCoords %>% pull(x),
                           y = xyCoords %>% pull(y)) %>%
    c(., .[1]) %>%
    xyCoords[.,]

  if (NROW(chullCoords) < 4) {
    cHull <- NA
  } else {
    cHull <- sp::Polygon(chullCoords, hole=F) %>%
      .@area
  }
  return(cHull)
}
