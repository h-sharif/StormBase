#' @keywords internal
#' @noRd
STEP8_beginning_flow_events <- function(beginning_rain_checked, end_rain_checked,
                                        end_flow, beginning_core,
                                        fluct_rain_Tr, fluct_flow_Tr) {
  # This function identifies start of flow events given the start of rain events.

  beginning_flow <- rep(NA_real_, length(beginning_rain_checked))
  n <- length(fluct_flow_Tr)

  for (g in seq_along(beginning_rain_checked)) {

    if (!is.na(beginning_rain_checked[g]) && !is.na(end_rain_checked[g])) {

      # CASE 1: Rain fluctuation is zero before core starts
      if (beginning_core[g] > 2 &&
          sum(fluct_rain_Tr[(beginning_core[g] - 2):(beginning_core[g] - 1)] == 0) == 2) {

        beginning_flow[g] <- beginning_rain_checked[g]

        while (beginning_flow[g] < end_flow[g] &&
               fluct_flow_Tr[beginning_flow[g]] > 0) {
          beginning_flow[g] <- beginning_flow[g] + 1
        }

      } else {
        # CASE 2: Flow fluctuation is zero before core starts
        beginning_flow[g] <- beginning_core[g]

        while (beginning_flow[g] <= end_flow[g] &&
               fluct_flow_Tr[beginning_flow[g]] >= 0) {
          beginning_flow[g] <- beginning_flow[g] + 1
        }
      }

    } else {
      # Invalid or anomalous rain event
      beginning_flow[g] <- NA
    }
  }

  return(beginning_flow)
}


# test -----------------
# source("R/from_matlab_step01_step02.R")
# source("R/from_matlab_step03_core_identification.R")
# source("R/from_matlab_step04_end_rain_events.R")
# source("R/from_matlab_step05_begining_rain_events.R")
# source("R/from_matlab_step06_checks_on_rain_events.R")
# source("R/from_matlab_step07_end_flow_events.R")
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
# end_flow_vec <- STEP7_end_flow_events(
#   beginning_rain_checked = res3$beginning_rain_checked,
#   end_rain_checked = res3$end_rain_checked,
#   beginning_core = res2$beginning_core,
#   end_core = res2$end_core,
#   flow = df_q$streamflow_mmd / 24,
#   fluct_rain_Tr = res1$fluct_rain_Tr,
#   fluct_flow_Tr = res1$fluct_flow_Tr,
#   Tr = res1$Tr
# )
# begining_flow_vec <- STEP8_beginning_flow_events(
#   beginning_rain_checked = res3$beginning_rain_checked,
#   end_rain_checked = res3$end_rain_checked,
#   end_flow = end_flow_vec,
#   beginning_core = res2$beginning_core,
#   fluct_rain_Tr = res1$fluct_rain_Tr,
#   fluct_flow_Tr = res1$fluct_flow_Tr
# )
# plot(begining_flow_vec, type = "l")

