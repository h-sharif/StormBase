#' @keywords internal
#' @noRd
STEP10_checks_on_overlapping_events <- function(beginning_rain_ungrouped, end_rain_ungrouped,
                                                beginning_flow_ungrouped, end_flow_ungrouped,
                                                time) {
  # time should be the 1 to length of input data: index of data
  # This function groups overlapping rain and flow events.
  n <- length(end_rain_ungrouped)
  marker_overlapping <- c()

  for (g in 1:(n - 1)) {
    if (end_rain_ungrouped[g] > beginning_rain_ungrouped[g + 1] ||
        end_flow_ungrouped[g] > beginning_flow_ungrouped[g + 1]) {
      marker_overlapping <- c(marker_overlapping, g)
    }
  }

  if (length(marker_overlapping) > 0) {
    q <- 1
    while (q <= length(marker_overlapping)) {
      to_group <- marker_overlapping[q]
      while (q < length(marker_overlapping) &&
             marker_overlapping[q + 1] == marker_overlapping[q] + 1) {
        to_group <- c(to_group, marker_overlapping[q + 1])
        q <- q + 1
      }

      # Merge events
      beginning_rain_ungrouped[to_group[1]] <- beginning_rain_ungrouped[to_group[1]]
      beginning_flow_ungrouped[to_group[1]] <- beginning_flow_ungrouped[to_group[1]]
      end_flow_ungrouped[to_group[1]] <- end_flow_ungrouped[to_group[length(to_group)] + 1]
      end_rain_ungrouped[to_group[1]] <- end_rain_ungrouped[to_group[length(to_group)] + 1]

      if (length(to_group) > 1) {
        beginning_rain_ungrouped[to_group[-1]] <- NA
        beginning_flow_ungrouped[to_group[-1]] <- NA
        end_flow_ungrouped[to_group[-1]] <- NA
        end_rain_ungrouped[to_group[-1]] <- NA
      }

      idx <- to_group[length(to_group)] + 1
      if (idx <= length(beginning_rain_ungrouped)) {
        beginning_rain_ungrouped[idx] <- NA
        beginning_flow_ungrouped[idx] <- NA
        end_flow_ungrouped[idx] <- NA
        end_rain_ungrouped[idx] <- NA
      }
      q <- q + 1
    }
  }

  # Select only valid events
  valid_idx <- which(!is.na(beginning_rain_ungrouped) &
                       !is.na(end_rain_ungrouped) &
                       !is.na(beginning_flow_ungrouped) &
                       !is.na(end_flow_ungrouped))

  beginning_flow_grouped <- beginning_flow_ungrouped[valid_idx]
  end_flow_grouped <- end_flow_ungrouped[valid_idx]
  beginning_rain_grouped <- beginning_rain_ungrouped[valid_idx]
  end_rain_grouped <- end_rain_ungrouped[valid_idx]

  BEGINNING_RAIN <- time[beginning_rain_grouped]
  END_RAIN <- time[end_rain_grouped]
  BEGINNING_FLOW <- time[beginning_flow_grouped]
  END_FLOW <- time[end_flow_grouped]

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
# plot(res5$BEGINNING_RAIN, type = "l")
# plot(res5$END_RAIN, type = "l")
# plot(res5$BEGINNING_FLOW, type = "l")
# plot(res5$END_FLOW, type = "l")

