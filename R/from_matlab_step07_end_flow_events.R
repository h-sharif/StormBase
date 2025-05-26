#' @keywords internal
#' @noRd
STEP7_end_flow_events <- function(beginning_rain_checked, end_rain_checked,
                                  beginning_core, end_core,
                                  flow, fluct_rain_Tr, fluct_flow_Tr, Tr) {
  # This function starts from the beginning of rain events and sets the end
  # delimiters of the flow events.

  n <- length(flow)
  end_flow <- rep(NA_real_, length(end_rain_checked))

  for (g in seq_along(end_rain_checked)) {
    # Proceed only if valid rain event is identified
    if (!is.na(end_rain_checked[g]) && !is.na(beginning_rain_checked[g])) {

      if (end_core[g] + 2 < n && sum(fluct_rain_Tr[(end_core[g] + 1):(end_core[g] + 2)] == 0) == 2) {
        # CASE 1: Core ends because rain fluctuation becomes zero
        end_flow[g] <- end_rain_checked[g]

        # Look for next valid rain event start after g
        next_event_index <- which(!is.na(beginning_rain_checked[(g + 1):length(beginning_rain_checked)]))

        if (length(next_event_index) > 0) {
          next_begin <- beginning_rain_checked[g + next_event_index[1]]
          upper_bound <- min(n, next_begin + Tr)

          while (end_flow[g] + 1 < n && fluct_flow_Tr[end_flow[g]] <= 0 && end_flow[g] < upper_bound) {
            end_flow[g] <- end_flow[g] + 1
          }
          while (end_flow[g] + 1 < n && fluct_flow_Tr[end_flow[g]] > 0 && end_flow[g] < upper_bound) {
            end_flow[g] <- end_flow[g] + 1
          }
          end_flow[g] <- end_flow[g] - 1

        } else {
          # CASE 1: Last event
          while (end_flow[g] + 1 < n && fluct_flow_Tr[end_flow[g]] <= 0) {
            end_flow[g] <- end_flow[g] + 1
          }
          while (end_flow[g] + 1 < n && fluct_flow_Tr[end_flow[g]] > 0) {
            end_flow[g] <- end_flow[g] + 1
          }
          end_flow[g] <- end_flow[g] - 1
        }
      } else {
        # CASE 2: Core ends because flow fluctuation is zero
        end_flow[g] <- end_core[g]
        while (end_flow[g] > beginning_core[g] && fluct_flow_Tr[end_flow[g]] <= 0) {
          end_flow[g] <- end_flow[g] - 1
        }
      }
    } else {
      # Invalid event
      end_flow[g] <- NA
    }
  }

  return(end_flow)
}

# test -----------------
# source("R/from_matlab_step01_step02.R")
# source("R/from_matlab_step03_core_identification.R")
# source("R/from_matlab_step04_end_rain_events.R")
# source("R/from_matlab_step05_begining_rain_events.R")
# source("R/from_matlab_step06_checks_on_rain_events.R")
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
# plot(end_flow_vec, type = "l")
