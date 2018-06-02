proto_event <- function(t_series,
                        criterion_column,
                        minDuration,
                        gaps = FALSE,
                        maxGap) {

  index_start <- index_end <- duration <- NULL

  # t_series <- as.data.frame(t_series) ###
  ex <- rle(as.vector(t_series[, criterion_column]))
  ind <- rep(seq_along(ex$lengths), ex$lengths)
  s <- split(1:nrow(t_series), ind)

  if (!(gaps)) {

    proto_event_value <- s[ex$values == TRUE]

    proto_event_rng <-
      lapply(proto_event_value, function(x)
        data.frame(index_start = min(x), index_end = max(x)))

    proto_events <- do.call(rbind, proto_event_rng) %>%
      dplyr::mutate(event_no = cumsum(ex$values[ex$values == TRUE])) %>%
      dplyr::mutate(duration = index_end - index_start + 1) %>%
      dplyr::filter(duration >= minDuration) %>%
      dplyr::mutate(date_start = t_series$ts_x[index_start]) %>%
      dplyr::mutate(date_end = t_series$ts_x[index_end])

    return(proto_events)

  } else {

    proto_gap_value <- s[ex$values == FALSE]

    proto_gap_rng <-
      lapply(proto_gap_value, function(x)
        data.frame(index_start = min(x), index_end = max(x)))

    proto_gaps <- do.call(rbind, proto_gap_rng) %>%
      dplyr::mutate(event_no = c(1:length(ex$values[ex$values == FALSE]))) %>%
      dplyr::mutate(duration = index_end - index_start + 1)

    if (any(proto_gaps$duration >= 1 & proto_gaps$duration <= maxGap)) {
      proto_gaps <- proto_gaps %>%
        dplyr::mutate(date_start = t_series$ts_x[index_start]) %>%
        dplyr::mutate(date_end = t_series$ts_x[index_end]) %>%
        dplyr::filter(duration >= 1 & duration <= maxGap)

    }

    return(proto_gaps)

  }
}
