#' @keywords internal
#' @noRd
STEP9_checks_on_flow_events <- function(beginning_rain_checked, end_rain_checked,
                                        beginning_flow, end_flow, fluct_flow_Tr) {
  # This function filters out wrong flow events such as starting after ending
  # or with wrong fluctuation signs.

  n <- length(beginning_flow)

  beginning_flow_checked <- rep(NA_real_, n)
  end_flow_checked <- rep(NA_real_, n)

  for (g in seq_len(n)) {

    if (!is.na(beginning_flow[g]) && !is.na(end_flow[g])) {
      invalid_event <- end_flow[g] <= beginning_flow[g] ||
        fluct_flow_Tr[beginning_flow[g]] > 0 ||
        fluct_flow_Tr[end_flow[g]] < 0 ||
        beginning_flow[g] < beginning_rain_checked[g] ||
        end_flow[g] < end_rain_checked[g]

      if (invalid_event) {
        beginning_flow_checked[g] <- NA
        end_flow_checked[g] <- NA
      } else {
        beginning_flow_checked[g] <- beginning_flow[g]
        end_flow_checked[g] <- end_flow[g]
      }
    }
  }

  # Select only valid events where all components are non-NA
  valid_idx <- which(!is.na(beginning_rain_checked) &
                       !is.na(end_rain_checked) &
                       !is.na(beginning_flow_checked) &
                       !is.na(end_flow_checked))

  beginning_rain_ungrouped <- beginning_rain_checked[valid_idx]
  end_rain_ungrouped <- end_rain_checked[valid_idx]
  beginning_flow_ungrouped <- beginning_flow_checked[valid_idx]
  end_flow_ungrouped <- end_flow_checked[valid_idx]

  return(list(
    beginning_rain_ungrouped = beginning_rain_ungrouped,
    end_rain_ungrouped = end_rain_ungrouped,
    beginning_flow_ungrouped = beginning_flow_ungrouped,
    end_flow_ungrouped = end_flow_ungrouped
  ))
}


# test -----------------
# source("R/from_matlab_step01_step02.R")
# source("R/from_matlab_step03_core_identification.R")
# source("R/from_matlab_step04_end_rain_events.R")
# source("R/from_matlab_step05_begining_rain_events.R")
# source("R/from_matlab_step06_checks_on_rain_events.R")
# source("R/from_matlab_step07_end_flow_events.R")
# source("R/from_matlab_step08_begining_flow_events.R")
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
# res4 <- STEP9_checks_on_flow_events(
#   beginning_rain_checked = res3$beginning_rain_checked,
#   end_rain_checked = res3$end_rain_checked,
#   beginning_flow = begining_flow_vec,
#   end_flow = end_flow_vec,
#   fluct_flow_Tr = res1$fluct_flow_Tr
# )
# plot(res4$beginning_rain_ungrouped, type = "l")
# plot(res4$end_rain_ungrouped, type = "l")
# plot(res4$beginning_rain_ungrouped, type = "l")
# plot(res4$end_flow_ungrouped, type = "l")

