#' Formant Analysis
#' @description This function analyzes formant data.
#' @param time
#' @param formant
#'
#' @return
#' @export
#'
#' @examples
formantAnalysis <- function(time,formant) {
  df <- data.frame(time,formant)
  df <- df %>%
    dplyr::mutate(time_incre = NA,
                  f_change = NA,
                  formant = na_if(formant,0))
  if (all(is.na(df$formant))){
    onset <- NA
    offset <- NA
    extent <- NA
    slope <- NA
    df <- df %>%
      dplyr::mutate(f_active = NA,
                    (f_steady = NA))
    percentActive_F <- NA
    f_change <- NA
    slope_2020 <- NA
    steady_2020 <- NA
    steadyDuration <- NA
    active_F <- NA
  } else {
    firstLast <- df %>%
      dplyr::select(formant) %>%
      na.omit(formant)
    onset <- as.numeric(data.table::first(firstLast))
    offset <- as.numeric(data.table::last(firstLast))
    extent <- base::abs(offset-onset)#*1000
    slope <- extent/((data.table::last(df$time) - data.table::first(df$time)))
    df <- df %>%
      dplyr::mutate(f_change = c(0,base::abs(diff(df$formant))),
                    time_change = c(0,base::abs(diff(df$time))),
                    time_incre = cumsum(time_change)
      )

    # This is the loop to determine active transitions
    c <- 1
    c_1 <- c + 1
    fl <- as.numeric(length(df$formant))
    d <- fl
    while(c <= fl){
      if (!is.na(df$f_change[c])){
        if (df$f_change[c] >= .02){
          df$f_active[c] <- TRUE
        } else {
          df$f_active[c] <- FALSE
        }
      } else {
        df$f_active[c] <- NA
      } # If there is an NA in the formants, this will make the active status NA
      c <- c + 1
    }
    df$f_steady <- !df$f_active

    # Active: This is the loop to correct for times where slopes are not active for only 1 time sequence ----
    c <- 2
    b <- c - 1
    a <- c + 1
    df_na <- df %>%
      dplyr::filter(!is.na(f_active))
    if (NROW(df_na) != 0) {
      while (c <= (NROW(df_na)-1)) {
        if (df_na$f_active[c] == FALSE && df_na$f_active[b] == TRUE && df_na$f_active[a] == TRUE){
          df_na$f_active[c] <- TRUE
        }
        c <- c + 1
        b <- c - 1
        a <- c + 1
      }
      df_na <- df_na %>%
        dplyr::select(time,f_active)
      df <- df %>%
        dplyr::select(!f_active) %>%
        base::merge(df_na,by = "time",all = TRUE)
    }

    # Active: Calculate slopes ----
    slopes <- rle(df$f_active)$lengths
    df_na <- df %>%
      dplyr::filter(!is.na(f_active))
    if (!all(df_na$f_active == FALSE)){
      TRUE_slopes <- slopes
      TRUE_slopes[!rle(df$f_active)$values] <- NA
      longestSlope <- base::max(as.numeric(TRUE_slopes), na.rm = TRUE)
    } else {
      longestSlope <- 0
    }
    if (longestSlope <= 1){
      slope_2020 <- NA
      active_F <- NA
    } else {
      inds <- as.numeric(first(which(TRUE_slopes == longestSlope)))
      beginning <- ifelse(inds == 1,1,sum(slopes[1:(first(inds)-1)]) + 1)
      if ((beginning-1) + slopes[inds] == as.numeric(NROW(df$f_active))){
        end <- as.numeric(NROW(df$f_active))
      } else {
        end <- as.numeric(NROW(df$f_active)) - sum(slopes[((inds[1])+1):as.numeric(NROW(slopes))])
      }
      active_F <- df[beginning:end,]
      timeMaxFormant <- active_F %>%
        dplyr::filter(formant == base::max(as.numeric(active_F$formant), na.rm = TRUE)) %>%
        dplyr::select(time) %>%
        data.table::first() %>%
        as.numeric

      active_F <- active_F %>%
        dplyr::filter(time <= timeMaxFormant) %>%
        dplyr::select(time, formant)

      timeMinFormant <- active_F %>%
        dplyr::filter(formant == base::min(as.numeric(active_F$formant), na.rm = TRUE)) %>%
        dplyr::select(time) %>%
        data.table::first() %>%
        as.numeric

      active_F <- active_F %>%
        dplyr::filter(time >= timeMinFormant)

      slope_2020 <- base::abs((dplyr::first(active_F$formant)) - (dplyr::last(active_F$formant))) / ((dplyr::last(active_F$time) - dplyr::first(active_F$time)))
    }
    percentActive_F <- (sum(df$f_active, na.rm = TRUE)/as.numeric(length(df$f_active)))*100

    # Steady: This is the loop to correct for times where slopes are not steady for only 1 time sequence ----
    c <- 2
    b <- c - 1
    a <- c + 1
    df_na <- df %>%
      dplyr::filter(!is.na(f_steady))
    if (!NROW(df_na) == 0) {
      while (c <= (NROW(df_na)-1)) {
        if (df_na$f_steady[c] == FALSE && df_na$f_steady[b] == TRUE && df_na$f_steady[a] == TRUE){
          df_na$f_steady[c] <- TRUE
        }
        c <- c + 1
        b <- c - 1
        a <- c + 1
      }
      df_na %>%
        dplyr::select(time,f_steady) ->
        df_na
      df %>%
        dplyr::select(-f_steady) %>%
        base::merge(df_na,by.x = "time",all = TRUE) ->
        df
    }

    # Steady: Calculate slopes ----
    slopes <- rle(df$f_steady)$lengths
    df_na <- df[!is.na(df$f_steady),]
    if (!all(df_na$f_steady == FALSE)){
      TRUE_slopes <- slopes
      TRUE_slopes[!rle(df$f_steady)$values] <- NA
      longestSlope <- base::max(as.numeric(TRUE_slopes), na.rm = TRUE)
    } else {
      longestSlope <- 0
    }
    if (longestSlope <= 1){
      steady_2020 <- NA
      steadyDuration <- 0
    } else {
      inds <- as.numeric(first(which(rle(df$f_steady)$lengths == longestSlope)))
      beginning <- ifelse(inds == 1,1,sum(slopes[1:(first(inds)-1)]) + 1)
      if ((beginning-1) + slopes[inds] == as.numeric(NROW(df$f_steady))){
        end <- as.numeric(NROW(df$f_steady))
      } else {
        end <- as.numeric(NROW(df$f_steady)) - sum(slopes[((inds[1])+1):as.numeric(NROW(slopes))])
      }
      steady_F <- df[beginning:end,]
      steady_2020 <- base::abs((dplyr::first(steady_F$formant)) - (dplyr::last(steady_F$formant))) / ((dplyr::last(steady_F$time) - dplyr::first(steady_F$time)))
      steadyDuration <- last(steady_F$time) - first(steady_F$time)
    }
    percentSteady_F <- (sum(df$f_steady, na.rm = TRUE)/as.numeric(length(df$f_steady)))*100
  }

  list <- list("onset" = onset,
               "offset" = offset,
               "extent" = extent,
               "slope" = slope,
               "f_active" = df$f_active,
               "f_steady" = df$f_steady,
               "percentActive" = percentActive_F,
               "f_change" = df$f_change,
               "f_slopeSegment" = active_F,
               "f_slope2020" = slope_2020,
               "f_steady2020" = steady_2020,
               "f_steadyDuration" = steadyDuration
  )
  return(list)
}
