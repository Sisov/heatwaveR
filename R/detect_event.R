detect_event <- function(data,
                         x = t,
                         y = temp,
                         seasClim = seas,
                         threshClim = thresh,
                         minDuration = 5,
                         joinAcrossGaps = TRUE,
                         maxGap = 2,
                         coldSpells = FALSE) {

  if(!(is.numeric(minDuration)))
    stop("Please ensure that 'minDuration' is a numeric/integer value.")
  if(!(is.logical(joinAcrossGaps)))
    stop("Please ensure that 'joinAcrossGaps' is either TRUE or FALSE.")
  if(!(is.numeric(maxGap)))
    stop("Please ensure that 'maxGap' is a numeric/integer value.")

  temp <- seas <- thresh <- NULL

  ts_x <- eval(substitute(x), data)
  ts_y <- eval(substitute(y), data)
  ts_seas <- eval(substitute(seasClim), data)
  ts_thresh <- eval(substitute(threshClim), data)
  t_series <- data.frame(ts_x, ts_y, ts_seas, ts_thresh)
  # rm(ts_x); rm(ts_y); rm(ts_seas); rm(ts_thresh)

  if (coldSpells) {
    t_series$ts_y <- -t_series$ts_y
    t_series$ts_seas <- -t_series$ts_seas
    t_series$ts_thresh <- -t_series$ts_thresh
  }

  t_series$ts_y[is.na(t_series$ts_y)] <- t_series$ts_seas[is.na(t_series$ts_y)]
  t_series$threshCriterion <- t_series$ts_y > t_series$ts_thresh

  proto_1 <- proto_event(t_series, criterion_column = 5,
                         minDuration = minDuration, maxGap = maxGap)

  t_series$durationCriterion <- rep(FALSE, nrow(t_series))

  for (i in 1:nrow(proto_1)) {
    t_series$durationCriterion[proto_1$index_start[i]:proto_1$index_end[i]] <-
      rep(TRUE, length = proto_1$duration[i])
  }

  proto_2 <- proto_event(t_series, criterion_column = 6,
                         minDuration = minDuration, gaps = TRUE,
                         maxGap = maxGap)

  if (ncol(proto_2) == 4)
    joinAcrossGaps <- FALSE

  if (joinAcrossGaps) {
    t_series$event <- t_series$durationCriterion
    for (i in 1:nrow(proto_2)) {
      t_series$event[proto_2$index_start[i]:proto_2$index_end[i]] <-
        rep(TRUE, length = proto_2$duration[i])
    }
  } else {
    t_series$event <- t_series$durationCriterion
  }

  proto_3 <- proto_event(t_series, criterion_column = 7,
                         minDuration = minDuration, maxGap = maxGap)

  t_series$event_no <- rep(NA, nrow(t_series))

  for (i in 1:nrow(proto_3)) {
    t_series$event_no[proto_3$index_start[i]:proto_3$index_end[i]] <-
      rep(i, length = proto_3$duration[i])
  }

  intensity_mean <- intensity_max <- intensity_cumulative <- intensity_mean_relThresh <-
    intensity_max_relThresh <- intensity_cumulative_relThresh <- intensity_mean_abs <-
    intensity_max_abs <- intensity_cumulative_abs <- rate_onset <- rate_decline <-
    mhw_rel_thresh <- mhw_rel_seas <- event_no <- row_index <- index_peak <-  NULL

  events <- t_series %>%
    dplyr::mutate(row_index = 1:nrow(t_series),
                  mhw_rel_seas = ts_y - ts_seas,
                  mhw_rel_thresh = ts_y - ts_thresh) %>%
    dplyr::filter(stats::complete.cases(event_no)) %>%
    dplyr::group_by(event_no) %>%
    dplyr::summarise(index_start = min(row_index),
                     index_peak = row_index[mhw_rel_seas == max(mhw_rel_seas)][1],
                     index_end = max(row_index),
                     duration = n(),
                     date_start = min(ts_x),
                     date_peak = ts_x[mhw_rel_seas == max(mhw_rel_seas)][1],
                     date_end = max(ts_x),
                     intensity_mean = mean(mhw_rel_seas),
                     intensity_max = max(mhw_rel_seas),
                     intensity_var = sqrt(stats::var(mhw_rel_seas)),
                     intensity_cumulative = max(cumsum(mhw_rel_seas)),
                     intensity_mean_relThresh = mean(mhw_rel_thresh),
                     intensity_max_relThresh = max(mhw_rel_thresh),
                     intensity_var_relThresh = sqrt(stats::var(mhw_rel_thresh)),
                     intensity_cumulative_relThresh = max(cumsum(mhw_rel_thresh)),
                     intensity_mean_abs = mean(ts_y),
                     intensity_max_abs = max(ts_y),
                     intensity_var_abs = sqrt(stats::var(ts_y)),
                     intensity_cumulative_abs = max(cumsum(ts_y)))

  mhw_rel_seas <- t_series$ts_y - t_series$ts_seas
  A <- mhw_rel_seas[events$index_start]
  B <- t_series$ts_y[events$index_start - 1]
  C <- t_series$ts_seas[events$index_start - 1]
  if (length(B) + 1 == length(A)) {
    B <- c(NA, B)
    C <- c(NA, C)
  }

  mhw_rel_seas_start <- 0.5 * (A + B - C)

  events$rate_onset <- ifelse(
    events$index_start > 1,
    (events$intensity_max - mhw_rel_seas_start) / (as.numeric(
      difftime(events$date_peak, events$date_start, units = "days")) + 0.5),
    NA
  )

  D <- mhw_rel_seas[events$index_end]
  E <- t_series$ts_y[events$index_end + 1]
  F <- t_series$ts_seas[events$index_end + 1]
  mhw_rel_seas_end <- 0.5 * (D + E - F)

  events$rate_decline <- ifelse(
    events$index_end < nrow(t_series),
    (events$intensity_max - mhw_rel_seas_end) / (as.numeric(
      difftime(events$date_end, events$date_peak, units = "days")) + 0.5),
    NA
  )

  if (coldSpells) {
    events <- events %>% dplyr::mutate(
      intensity_mean = -intensity_mean,
      intensity_max = -intensity_max,
      intensity_cumulative = -intensity_cumulative,
      intensity_mean_relThresh = -intensity_mean_relThresh,
      intensity_max_relThresh = -intensity_max_relThresh,
      intensity_cumulative_relThresh = -intensity_cumulative_relThresh,
      intensity_mean_abs = -intensity_mean_abs,
      intensity_max_abs = -intensity_max_abs,
      intensity_cumulative_abs = -intensity_cumulative_abs,
      rate_onset = -rate_onset,
      rate_decline = -rate_decline
    )
  }

  data_clim <- cbind(data, t_series[,5:8])

  list(climatology = tibble::as_tibble(data_clim),
       event = tibble::as_tibble(events))
}

