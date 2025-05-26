#' @keywords internal
#' @noRd
STEP4_end_rain_events <- function(beginning_core, end_core, rain, fluct_rain_Tr, rain_min) {
  # This function sets the end delimiter for the rain events
  # end_rain is the output of this function containing nd delimiter for each
  # rainfall events.

  rain <- as.numeric(rain)
  fluct_rain_Tr <- as.numeric(fluct_rain_Tr)
  n <- length(rain)

  end_rain <- numeric(length(end_core))

  for (g in seq_along(end_core)) {
    end_rain[g] <- end_core[g]  # Preliminary guess

    # CORE ENDS BECAUSE RAIN FLUCTUATIONS ARE ZERO (cases 1 and 2)
    if ((end_core[g] + 2) < n &&
        sum(fluct_rain_Tr[(end_core[g] + 1):(end_core[g] + 2)] == 0, na.rm = TRUE) == 2) {

      # CASE 1: Rain ends at zero -> move backwards until rain > 0
      if (rain[end_rain[g]] == 0) {
        while (end_rain[g] - 1 > 0 && rain[end_rain[g]] == 0) {
          end_rain[g] <- end_rain[g] - 1
        }

      } else {
        # CASE 2: Rain ends > 0 -> move forward until rain < rain_min
        next_core_indices <- which(!is.na(beginning_core[(g + 1):length(beginning_core)]))
        if (length(next_core_indices) > 0) {
          next_core_start <- beginning_core[g + next_core_indices[1]]
          while ((end_rain[g] + 1) < n &&
                 rain[end_rain[g]] > rain_min &&
                 end_rain[g] < next_core_start) {
            end_rain[g] <- end_rain[g] + 1
          }
          end_rain[g] <- end_rain[g] - 1
        } else {
          # CASE 2: last event, no next core
          while ((end_rain[g] + 1) < n && rain[end_rain[g]] > rain_min) {
            end_rain[g] <- end_rain[g] + 1
          }
          end_rain[g] <- end_rain[g] - 1
        }
      }

    } else {
      # CASE 3: Core ends because flow fluctuation is zero
      # Step back during dry/nearly-dry period
      while ((end_rain[g] - 1 > 0) &&
             (rain[end_rain[g]] > rain_min) &&
             (end_rain[g] >= beginning_core[g])) {
        end_rain[g] <- end_rain[g] - 1
      }

      # Move further back to last significant rain
      while ((end_rain[g] - 1 > 0) &&
             (rain[end_rain[g]] < rain_min) &&
             (end_rain[g] >= beginning_core[g])) {
        end_rain[g] <- end_rain[g] - 1
      }
    }
  }

  return(end_rain)
}


# test -----------------
# source("R/from_matlab_step01_step02.R")
# source("R/from_matlab_step03_core_identification.R")
# load("data/testcatchment_precip.RDS")
# load("data/testcatchment_streamflow.RDS")
# res1 <- STEP1_STEP2_Tr_and_fluctuations_timeseries(
#   rain = df_p$precipitation_mmd / 24,
#   flow = df_q$streamflow_mmd / 24,
#   rain_min = 0.02,
#   max_window = 100
# )
# res2 <- STEP3_core_identification(res1$fluct_bivariate_Tr)
# end_rain <- STEP4_end_rain_events(
#   beginning_core = res2$beginning_core,
#   end_core = res2$end_core,
#   rain = df_p$precipitation_mmd / 24,
#   fluct_rain_Tr = res1$fluct_rain_Tr,
#   rain_min = 0.02
# )
# plot(end_rain, type = "l")

