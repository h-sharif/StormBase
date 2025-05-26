#' @keywords internal
#' @noRd
EVENT_ANALYSIS <- function(BEGINNING_RAIN, END_RAIN,
                           BEGINNING_FLOW, END_FLOW,
                           rain, flow, time, datetime,
                           flag, multiple) {
  # This function calculates durations, volumes, and runoff_ratio for the
  # identified events.
  # And, returns a data frame with following columns for all identified events:
  # waterInput_start, waterInput_end, waterInput_duration, waterInput_volume,
  # stormflow_start, stormflow_end, stormflow_duration, stormflow_volume, and
  # runoff_ratio

  # For runoff_ratio and runoff volume calculations:
  # if flag == TRUE: baseflow is being identified and separated from the provided
  # flow signal, which is the total streamflow in this case. Otherwise, when
  # flag ==  FALSE, flow signal assumed to be the stormflow signal without
  # the baseflow contributions.

  # If we have NA in flow values, those values are treated as zero for runoff
  # VOLUME and runoff ratio, so it's better to exclude events with missing
  # flow values.

  # Initialize vector
  # Calculated durations in the unit of provided forcing data
  DURATION_RAIN <- (END_RAIN - BEGINNING_RAIN) + 1
  DURATION_RUNOFF <- (END_FLOW - BEGINNING_FLOW) + 1
  VOLUME_RAIN <- numeric(length(BEGINNING_RAIN))
  VOLUME_RUNOFF <- numeric(length(BEGINNING_FLOW))

  # Volume of rain events [mm]
  for (h in seq_along(BEGINNING_RAIN)) {
    idx_start <- which(time == BEGINNING_RAIN[h])
    idx_end <- which(time == END_RAIN[h])
    if (length(idx_start) > 0 && length(idx_end) > 0) {
      VOLUME_RAIN[h] <- sum(rain[idx_start:idx_end], na.rm = TRUE) * multiple
    } else {
      VOLUME_RAIN[h] <- NA
    }
  }

  # Volume of runoff events [mm]
  if (!flag) {
    # Using flow as runoff (baseflow has been subtracted already)
    for (h in seq_along(BEGINNING_FLOW)) {
      idx_start <- which(time == BEGINNING_FLOW[h])
      idx_end <- which(time == END_FLOW[h])
      if (length(idx_start) > 0 && length(idx_end) > 0) {
        VOLUME_RUNOFF[h] <- sum(flow[idx_start:idx_end], na.rm = TRUE) * multiple
      } else {
        VOLUME_RUNOFF[h] <- NA
      }
    }
  } else {
    # Subtract baseflow
    # baseflow is in the unit of flow and rain at hourly time scale
    baseflow <- BASEFLOW_CURVE(BEGINNING_FLOW, END_FLOW, flow, time)
    for (h in seq_along(BEGINNING_FLOW)) {
      idx_start <- which(time == BEGINNING_FLOW[h])
      idx_end <- which(time == END_FLOW[h])
      if (length(idx_start) > 0 && length(idx_end) > 0) {
        q <- flow[idx_start:idx_end]
        qb <- baseflow[idx_start:idx_end]
        VOLUME_RUNOFF[h] <- sum(q - qb, na.rm = TRUE) * multiple
      } else {
        VOLUME_RUNOFF[h] <- NA
      }
    }
  }

  # Runoff ratio
  RUNOFF_RATIO <- VOLUME_RUNOFF / VOLUME_RAIN

  return(
    data.frame(
      waterInput_start = datetime[BEGINNING_RAIN],
      waterInput_end = datetime[END_RAIN],
      waterInput_duration = DURATION_RAIN,
      waterInput_volume = VOLUME_RAIN,
      stormflow_start = datetime[BEGINNING_FLOW],
      stormflow_end = datetime[END_FLOW],
      stormflow_duration = DURATION_RUNOFF,
      stormflow_volume = VOLUME_RUNOFF,
      runoff_ratio = RUNOFF_RATIO
    )
  )
}


# test -----------------
# source("R/from_matlab_step01_step02.R")
# source("R/from_matlab_step03_core_identification.R")
# source("R/from_matlab_step04_end_rain_events.R")
# source("R/from_matlab_step05_begining_rain_events.R")
# source("R/from_matlab_step06_checks_on_rain_events.R")
# source("R/from_matlab_step07_end_flow_events.R")
# source("R/from_matlab_step08_begining_flow_events.R")
# source("R/from_matlab_step09_checks_on_flow_events.R")
# source("R/from_matlab_step10_checks_on_flow_events.R")
# source("R/from_matlab_baseflow_curve.R")
# source("R/from_matlab_event_identification_dmca.R")
# load("data/testcatchment_precip.RDS")
# load("data/testcatchment_streamflow.RDS")
# dmca_res <- EVENT_IDENTIFICATION_DMCA(
#   rain = df_p$precipitation_mmd / 24,
#   flow = df_q$streamflow_mmd / 24,
#   time = c(1:nrow(df_p)),
#   rain_min = 0.02,
#   max_window = 100
# )
# res <- EVENT_ANALYSIS(
#   BEGINNING_RAIN = dmca_res$BEGINNING_RAIN,
#   END_RAIN = dmca_res$END_RAIN,
#   BEGINNING_FLOW = dmca_res$BEGINNING_FLOW,
#   END_FLOW = dmca_res$END_FLOW,
#   rain = df_p$precipitation_mmd / 24,
#   flow = df_q$streamflow_mmd / 24,
#   time = c(1:nrow(df_p)),
#   datetime = df_p$Date,
#   flag = TRUE,
#   multiple = 24
# )
# plot(res$waterInput_volume, type = "l")
# plot(res$stormflow_volume, type = "l")

