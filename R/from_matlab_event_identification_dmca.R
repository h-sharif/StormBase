#' @keywords internal
#' @noRd
EVENT_IDENTIFICATION_DMCA <- function(rain, flow, time, rain_min, max_window) {
  # This function identifies rainfall-streamflow events.
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
  # time should be the 1 to length of input data: index of data

  # STEP1 and STEP2: Tr + rainfall and flow fluctuations
  step1_step2 <- STEP1_STEP2_Tr_and_fluctuations_timeseries(rain, flow, rain_min, max_window)
  Tr <- step1_step2$Tr
  fluct_rain_Tr <- step1_step2$fluct_rain_Tr
  fluct_flow_Tr <- step1_step2$fluct_flow_Tr
  fluct_bivariate_Tr <- step1_step2$fluct_bivariate_Tr

  # STEP3: Core identification
  core <- STEP3_core_identification(fluct_bivariate_Tr)
  beginning_core <- core$beginning_core
  end_core <- core$end_core

  # STEP4: End of rain events
  end_rain <- STEP4_end_rain_events(beginning_core, end_core, rain, fluct_rain_Tr, rain_min)

  # STEP5: Beginning of rain events
  beginning_rain <- STEP5_beginning_rain_events(beginning_core, end_rain, rain, fluct_rain_Tr, rain_min)

  # STEP6: Check rain events
  rain_checked <- STEP6_checks_on_rain_events(beginning_rain, end_rain, rain, rain_min)
  beginning_rain_checked <- rain_checked$beginning_rain_checked
  end_rain_checked <- rain_checked$end_rain_checked

  # STEP7: End of flow events
  end_flow <- STEP7_end_flow_events(beginning_rain_checked, end_rain_checked, beginning_core, end_core, flow, fluct_rain_Tr, fluct_flow_Tr, Tr)

  # STEP8: Beginning of flow events
  beginning_flow <- STEP8_beginning_flow_events(beginning_rain_checked, end_rain_checked, end_flow, beginning_core, fluct_rain_Tr, fluct_flow_Tr)

  # STEP9: Check flow events
  flow_checked <- STEP9_checks_on_flow_events(beginning_rain_checked, end_rain_checked, beginning_flow, end_flow, fluct_flow_Tr)
  beginning_rain_ungrouped <- flow_checked$beginning_rain_ungrouped
  end_rain_ungrouped <- flow_checked$end_rain_ungrouped
  beginning_flow_ungrouped <- flow_checked$beginning_flow_ungrouped
  end_flow_ungrouped <- flow_checked$end_flow_ungrouped

  # STEP10: Group overlapping events
  result <- STEP10_checks_on_overlapping_events(
    beginning_rain_ungrouped,
    end_rain_ungrouped,
    beginning_flow_ungrouped,
    end_flow_ungrouped,
    time
  )

  BEGINNING_RAIN <- result$BEGINNING_RAIN
  END_RAIN <- result$END_RAIN
  BEGINNING_FLOW <- result$BEGINNING_FLOW
  END_FLOW <- result$END_FLOW

  return(list(
    BEGINNING_RAIN = BEGINNING_RAIN,
    END_RAIN = END_RAIN,
    BEGINNING_FLOW = BEGINNING_FLOW,
    END_FLOW = END_FLOW
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
# source("R/from_matlab_step09_checks_on_flow_events.R")
# source("R/from_matlab_step10_checks_on_flow_events.R")
# load("data/testcatchment_precip.RDS")
# load("data/testcatchment_streamflow.RDS")
# dmca_res <- EVENT_IDENTIFICATION_DMCA(
#   rain = df_p$precipitation_mmd / 24,
#   flow = df_q$streamflow_mmd / 24,
#   time = c(1:nrow(df_p)),
#   rain_min = 0.02,
#   max_window = 100
# )
# plot(dmca_res$BEGINNING_RAIN, type = "l")
# plot(dmca_res$END_RAIN, type = "l")
# plot(dmca_res$BEGINNING_FLOW, type = "l")
# plot(dmca_res$END_FLOW, type = "l")


