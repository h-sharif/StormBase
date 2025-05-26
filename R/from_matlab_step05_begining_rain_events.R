#' @keywords internal
#' @noRd
STEP5_beginning_rain_events <- function(beginning_core, end_rain, rain, fluct_rain_Tr, rain_min) {
  # starting from core events, this function set the start of event delimiters.
  # output of this function is a vector containing start delimiter of each
  # rain event
  rain <- as.numeric(rain)
  fluct_rain_Tr <- as.numeric(fluct_rain_Tr)
  n <- length(rain)

  beginning_rain <- numeric(length(beginning_core))

  for (g in seq_along(beginning_core)) {
    beginning_rain[g] <- beginning_core[g]  # Preliminary guess

    # BEFORE CORE STARTS RAIN FLUCTUATIONS ARE ZERO (cases 1 and 2)
    if (beginning_core[g] > 2 &&
        sum(fluct_rain_Tr[(beginning_core[g] - 2):(beginning_core[g] - 1)] == 0, na.rm = TRUE) == 2) {

      # CASE 1: Rain is zero at core start -> move forward to first non-zero
      if (rain[beginning_core[g]] == 0) {
        while ((beginning_rain[g] + 1) < n && rain[beginning_rain[g]] == 0) {
          beginning_rain[g] <- beginning_rain[g] + 1
        }

      } else {
        # CASE 2: Rain > 0 at core start -> move backward to last insignificant rain
        prev_event_end <- which(!is.na(end_rain[1:(g - 1)]))
        if (length(prev_event_end) > 0) {
          while ((beginning_rain[g] - 1) > 0 &&
                 rain[beginning_rain[g]] > rain_min &&
                 beginning_rain[g] > end_rain[prev_event_end[length(prev_event_end)]]) {
            beginning_rain[g] <- beginning_rain[g] - 1
          }
          beginning_rain[g] <- beginning_rain[g] + 1
        } else {
          while ((beginning_rain[g] - 1) > 0 &&
                 rain[beginning_rain[g]] > rain_min) {
            beginning_rain[g] <- beginning_rain[g] - 1
          }
          beginning_rain[g] <- beginning_rain[g] + 1
        }
      }

    } else {
      # CASE 3: Flow fluctuation ends -> move backward until rain < rain_min
      prev_event_end <- which(!is.na(end_rain[1:(g - 1)]))
      if (length(prev_event_end) > 0) {
        while ((beginning_rain[g] - 1) > 0 &&
               rain[beginning_rain[g]] > rain_min &&
               beginning_rain[g] > end_rain[prev_event_end[length(prev_event_end)]]) {
          beginning_rain[g] <- beginning_rain[g] - 1
        }
        beginning_rain[g] <- beginning_rain[g] + 1
      } else {
        while ((beginning_rain[g] - 1) > 0 &&
               rain[beginning_rain[g]] > rain_min) {
          beginning_rain[g] <- beginning_rain[g] - 1
        }
        beginning_rain[g] <- beginning_rain[g] + 1
      }
    }
  }

  return(beginning_rain)
}


# test -----------------
# source("R/from_matlab_step01_step02.R")
# source("R/from_matlab_step03_core_identification.R")
# source("R/from_matlab_step04_end_rain_events.R")
# load("data/testcatchment_precip.RDS")
# load("data/testcatchment_streamflow.RDS")
# res1 <- STEP1_STEP2_Tr_and_fluctuations_timeseries(
#   rain = df_p$precipitation_mmd / 24,
#   flow = df_q$streamflow_mmd / 24,
#   rain_min = 0.02,
#   max_window = 100
# )
# res2 <- STEP3_core_identification(res1$fluct_bivariate_Tr)
# end_rain_vec <- STEP4_end_rain_events(
#   beginning_core = res2$beginning_core,
#   end_core = res2$end_core,
#   rain = df_p$precipitation_mmd / 24,
#   fluct_rain_Tr = res1$fluct_rain_Tr,
#   rain_min = 0.02
# )
# begining_rain_vec <- STEP5_beginning_rain_events(
#   beginning_core = res2$beginning_core,
#   end_rain = end_rain_vec,
#   rain = df_p$precipitation_mmd / 24,
#   fluct_rain_Tr = res1$fluct_rain_Tr,
#   rain_min = 0.02
# )
# plot(begining_rain_vec, type = "l")

