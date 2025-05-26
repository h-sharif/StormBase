#' @keywords internal
#' @noRd
STEP3_core_identification <- function(fluct_bivariate_Tr) {
  # This function identifies event cores (periods when both rain and flow fluctuate)
  # based on the bivariate fluctuation time series.

  beginning_core <- numeric(0)
  end_core <- numeric(0)

  g <- 1
  q <- 1
  n <- length(fluct_bivariate_Tr)

  while (q + 1 <= n) {
    if (abs(fluct_bivariate_Tr[q]) > 0) {
      beginning_core[g] <- q

      # need to consecutive timestep of zero fluctuations to end the core.
      # one time step can only just be a change of sign during fluctuations.
      while (q + 1 < n && sum(abs(fluct_bivariate_Tr[q:(q + 1)])) > 0) {
        q <- q + 1
        if (q >= n) break # scanned the entire time series
      }

      end_core[g] <- q - 1
      g <- g + 1
    }
    q <- q + 1
  }

  return(list(
    beginning_core = beginning_core,
    end_core = end_core
  ))
}

# test -----------------
# source("R/from_matlab_step01_step02.R")
# load("data/testcatchment_precip.RDS")
# load("data/testcatchment_streamflow.RDS")
# res1 <- STEP1_STEP2_Tr_and_fluctuations_timeseries(
#   rain = df_p$precipitation_mmd / 24,
#   flow = df_q$streamflow_mmd / 24,
#   rain_min = 0.02,
#   max_window = 100
# )
# res2 <- STEP3_core_identification(res1$fluct_bivariate_Tr)
# plot(res2$beginning_core, type = "l")
# plot(res2$end_core, type = "l")
