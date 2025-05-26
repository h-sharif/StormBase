#' @keywords internal
#' @noRd
BASEFLOW_CURVE <- function(BEGINNING_FLOW, END_FLOW, flow, time) {
  # This function calculates the baseflow signal when necessary, such as
  # providing the total streamflow signal as the input.

  # Description ----------------------
  # time should be the 1 to length of input data: index of data.
  # flow should be in the hourly format.
  # returned baseflow will be also in the unit of the given flow at the hourly
  # time scale.

  # Initialize baseflow with observed flow
  baseflow <- flow

  # Build an ordered vector of all BEGIN and END event times
  beg_end_series <- as.vector(rbind(BEGINNING_FLOW, END_FLOW))

  for (k in seq_len(length(beg_end_series) - 1)) {
    index_beg <- which(time == beg_end_series[k])
    index_end <- which(time == beg_end_series[k + 1])

    # Skip if either index is missing
    if (length(index_beg) == 0 || length(index_end) == 0) next

    segment_flow <- flow[index_beg:index_end]

    # Skip interpolation if too many NAs in the segment
    if (sum(is.na(segment_flow)) >= 0.9 * length(segment_flow)) {
      baseflow[index_beg:index_end] <- NA
      next
    }

    # If only one timestep between events
    if ((index_end - index_beg) == 1) {
      baseflow[index_beg] <- flow[index_beg]
      baseflow[index_end] <- flow[index_end]
      next
    }

    # Check if endpoints are non-NA before interpolation
    if (!is.na(flow[index_beg]) && !is.na(flow[index_end])) {
      if (flow[index_beg] < flow[index_end]) {
        increment <- (flow[index_end] - flow[index_beg]) / (index_end - index_beg)
        for (i in (index_beg + 1):(index_end - 1)) {
          baseflow[i] <- baseflow[index_beg] + increment * (i - index_beg)
        }
      } else if (flow[index_beg] > flow[index_end]) {
        increment <- (flow[index_beg] - flow[index_end]) / (index_end - index_beg)
        for (i in (index_beg + 1):(index_end - 1)) {
          baseflow[i] <- baseflow[index_beg] - increment * (i - index_beg)
        }
      }
    }
  }

  # Ensure baseflow does not exceed observed flow
  # This happens where the line intersects the streamflow and baseflow becomes
  # larger.
  for (m in seq_along(baseflow)) {
    if (!is.na(baseflow[m]) && !is.na(flow[m]) && baseflow[m] > flow[m]) {
      baseflow[m] <- flow[m]
    }
  }

  return(baseflow)
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
# res5 <- STEP10_checks_on_overlapping_events(
#   beginning_rain_ungrouped = res4$beginning_rain_ungrouped,
#   end_rain_ungrouped = res4$end_rain_ungrouped,
#   beginning_flow_ungrouped = res4$beginning_flow_ungrouped,
#   end_flow_ungrouped = res4$end_flow_ungrouped,
#   time = c(1:nrow(df_p))
# )
# baseflow_result <- BASEFLOW_CURVE(
#   BEGINNING_FLOW = res5$BEGINNING_FLOW,
#   END_FLOW = res5$END_FLOW,
#   flow = df_q$streamflow_mmd / 24,
#   time = c(1:nrow(df_p))
# )
# plot(baseflow_result, type = "l")

