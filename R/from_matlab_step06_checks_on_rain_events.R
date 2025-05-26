#' @keywords internal
#' @noRd
STEP6_checks_on_rain_events <- function(beginning_rain, end_rain, rain, rain_min) {
  # This function checks for anomalies in the identified rain events such as
  # events that end before starting or not delimited by period of rain lower
  # than rain_min.
  rain <- as.numeric(rain)
  n <- length(rain)
  beginning_rain_checked <- numeric(length(beginning_rain))
  end_rain_checked <- numeric(length(end_rain))

  for (g in seq_along(beginning_rain)) {
    # Check boundary conditions safely
    before_start <- ifelse(beginning_rain[g] > 1, rain[beginning_rain[g] - 1], 0)
    after_end <- ifelse(end_rain[g] < n, rain[end_rain[g] + 1], 0)

    if (beginning_rain[g] > end_rain[g] || before_start > rain_min || after_end > rain_min) {
      beginning_rain_checked[g] <- NA
      end_rain_checked[g] <- NA
    } else {
      beginning_rain_checked[g] <- beginning_rain[g]
      end_rain_checked[g] <- end_rain[g]
    }
  }

  return(list(beginning_rain_checked = beginning_rain_checked,
              end_rain_checked = end_rain_checked))
}

# test -----------------
# source("R/from_matlab_step01_step02.R")
# source("R/from_matlab_step03_core_identification.R")
# source("R/from_matlab_step04_end_rain_events.R")
# source("R/from_matlab_step05_begining_rain_events.R")
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
# res3 <- STEP6_checks_on_rain_events(
#   beginning_rain = begining_rain_vec,
#   end_rain = end_rain_vec,
#   rain = df_p$precipitation_mmd / 24,
#   rain_min = 0.02
# )
# plot(res3$beginning_rain_checked, type = "l")
# plot(res3$end_rain_checked, type = "l")


