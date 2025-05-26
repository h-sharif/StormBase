#' @keywords internal
#' @noRd
STEP1_STEP2_Tr_and_fluctuations_timeseries <- function(rain, flow, rain_min, max_window) {
  # Variables -----------------------
  # rain_min: minimum rainfall intensity considered significant at the resolution
  #           of data in hourly time scale.
  # max_window: a sensible upper limit to test and explore for the catchment
  #             response time. In case of daily data, 16 days means catchment
  #             response time was searched in the (0, ~8 days] region.
  # rain: vector values of rainfall in hourly time scale such as mm/hr
  # flow: vector values of catchment flow response in hourly time scale such as
  #       mm/hr
  # rain and flow units should be consistent: being unit per catchment area or
  # total volume.

  # Descriptions --------------------
  # Only odd window lengths will be explored as Tr = (best window - 1)/2
  # rain_min should be given in a unit of mm/hr (conversion function is provided)
  # rain and flow are vector of time series for rainfall and flow inputs
  # flow can be the total streamflow or stormflow as a result of subtracting
  # baseflow from the streamflow signal.

  rain <- as.numeric(rain)
  flow <- as.numeric(flow)
  rain_int <- cumsum(ifelse(is.na(rain), 0, rain))
  flow_int <- cumsum(ifelse(is.na(flow), 0, flow))
  T_len <- length(rain)

  num_windows <- length(seq(3, max_window, by = 2))
  F_rain <- numeric(num_windows)
  F_flow <- numeric(num_windows)
  F_rain_flow <- numeric(num_windows)
  rho <- numeric(num_windows)

  fluct_rain_list <- vector("list", num_windows)
  fluct_flow_list <- vector("list", num_windows)

  idx_win <- 0
  for (window in seq(3, max_window, by = 2)) {
    idx_win <- idx_win + 1

    # Centered rolling mean using zoo
    rain_movmean <- zoo::rollapply(rain_int, width = window, FUN = mean,
                                   align = "center", partial = TRUE)
    flow_movmean <- zoo::rollapply(flow_int, width = window, FUN = mean,
                                   align = "center", partial = TRUE)

    # Fluctuations
    fr <- rain_int - rain_movmean
    ff <- flow_int - flow_movmean

    # Save fluctuations at current window
    fluct_rain_list[[idx_win]] <- fr
    fluct_flow_list[[idx_win]] <- ff

    # Use only valid (non-NA) central range for metrics
    idx_valid <- which(!is.na(fr) & !is.na(ff))
    if (length(idx_valid) > 0) {
      F_rain[idx_win] <- mean(fr[idx_valid]^2)
      F_flow[idx_win] <- mean(ff[idx_valid]^2)
      F_rain_flow[idx_win] <- mean(fr[idx_valid] * ff[idx_valid])
      rho[idx_win] <- F_rain_flow[idx_win] / (sqrt(F_rain[idx_win]) * sqrt(F_flow[idx_win]))
    } else {
      rho[idx_win] <- NA
    }
  }

  # Find the window with minimum DMCA-based correlation
  Tr <- which.min(rho)

  # if there is not any lag between rain and flow time series, the algorithm
  # fails as user need to provide higher resolution data in time.
  # This leads to getting a Tr less than at least one time step.
  if (is.na(Tr) | length(Tr) == 0) {
    stop("There should be at least a lag between water_input and flow signals!\nIdeal catchment response time is less a single time step!")
  }
  if (Tr < 1) {
    stop("There should be at least a lag between water_input and flow signals!\nIdeal catchment response time is less a single time step!")
  }
  window_Tr <- seq(3, max_window, by = 2)[Tr]

  # Tolerances
  tol_fluct_rain <- (rain_min / (2 * Tr + 1)) * (((2 * Tr + 1) - 1) / 2)
  tol_fluct_flow <- tail(flow_int, 1) / 1e15

  # Get fluctuations corresponding to Tr
  fr_Tr <- fluct_rain_list[[Tr]]
  ff_Tr <- fluct_flow_list[[Tr]]

  # Apply thresholds for fluctuations
  fr_Tr[abs(fr_Tr) < tol_fluct_rain] <- 0
  ff_Tr[abs(ff_Tr) < tol_fluct_flow] <- 0

  fluct_bivariate_Tr <- fr_Tr * ff_Tr

  return(list(
    Tr = Tr,
    fluct_rain_Tr = fr_Tr,
    fluct_flow_Tr = ff_Tr,
    fluct_bivariate_Tr = fluct_bivariate_Tr
  ))
}


# test ----------------------------
# load("data/testcatchment_precip.RDS")
# load("data/testcatchment_streamflow.RDS")
# res <- STEP1_STEP2_Tr_and_fluctuations_timeseries(
#   rain = df_p$precipitation_mmd / 24,
#   flow = df_q$streamflow_mmd / 24,
#   rain_min = 0.02,
#   max_window = 100
# )
#
# res$Tr
# plot(res$fluct_rain_Tr, type = "l")
# plot(res$fluct_flow_Tr, type = "l")
# plot(res$fluct_bivariate_Tr, type = "l")

