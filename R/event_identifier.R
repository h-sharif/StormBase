#' Identify Stormflow Events in a Water Input-Flow Time Series
#'
#' Identifies stormflow events based on rainfall (or water input) and flow time series
#' using the Detrending Moving-Average Cross-Correlation Analysis (DMCA) method.
#' The function supports both daily and hourly input data. Flow and water input
#' units should be consistent (e.g., mm/day, m³/hr).
#'
#' When daily data is used, the function internally converts values to an hourly
#' scale for event detection. If total streamflow is provided, setting
#' \code{baseflow_flag = TRUE} subtracts a baseflow signal from the flow values
#' for calculation of runoff volume and ratio calculations.
#'
#' @param datetime A vector of class \code{Date} (for daily data) or \code{POSIXct} (for hourly data)
#'   representing the timestamps of observations. Must be the same length as \code{water_input} and \code{flow}.
#' @param water_input A numeric vector representing the time series of water input
#'   (e.g., rainfall or rainfall + snowmelt), in daily or hourly time scale.
#' @param flow A numeric vector of catchment response (e.g., streamflow) in the same
#'   time scale and unit as \code{water_input}.
#' @param is_daily Logical. If \code{TRUE}, assumes inputs are in daily time scale.
#'   If \code{FALSE}, assumes hourly data.
#' @param Rmin Numeric. Minimum rainfall intensity threshold (in hourly units)
#'   used to terminate stormflow events. Should match the hourly unit of \code{water_input}.
#' @param Lmax Integer. Maximum window length to explore for the catchment
#'   response time, in daily or hourly units depending on \code{is_daily}. Only
#'   odd values in the range 3:2:Lmax are tested.
#' @param baseflow_flag Logical. If \code{TRUE}, subtracts a baseflow signal from
#'   the flow before calculating runoff volume and ratio.
#' @param exclude_na Logical. If \code{TRUE}, excludes events that contain \code{NA}
#'   values in the flow data. Treated as zeros otherwise.
#'
#' @details
#' - The length of \code{datetime}, \code{water_input}, and \code{flow} must be equal.
#' - When \code{is_daily = TRUE}, flow and water input are adjusted to an hourly scale
#'   for the event identification procedure.
#' - \code{Rmin} should be specified in hourly units, even if \code{is_daily = TRUE}.
#' - \code{Lmax} defines the upper limit for catchment response lag. For example,
#'   \code{Lmax = 300} (hours) searches a range of up to ~6.25 days.
#'
#' @return A data frame with one row per identified stormflow event, and the following columns:
#' \itemize{
#'   \item \code{waterInput_start}: Start time of the water input event
#'   \item \code{waterInput_end}: End time of the water input event
#'   \item \code{waterInput_duration}: Duration of the water input event
#'   \item \code{waterInput_volume}: Total water input volume over the event
#'   \item \code{stormflow_start}: Start time of the corresponding stormflow response
#'   \item \code{stormflow_end}: End time of the stormflow response
#'   \item \code{stormflow_duration}: Duration of the stormflow event
#'   \item \code{stormflow_volume}: Total stormflow volume during the response
#'   \item \code{runoff_ratio}: Ratio of stormflow volume to water input volume
#'   \item \code{flow_missing_fraction}: Fraction of missing values in the flow data during the event
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr pull
#' @importFrom lubridate is.Date is.POSIXct
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @export
event_identifier <- function(datetime, water_input, flow, is_daily = TRUE,
                             Rmin = 0.02, Lmax = 100,
                             baseflow_flag = TRUE,
                             exclude_na = TRUE) {

  # Controlling Inputs ---------------------------------------------------------
  # 1. Input lengths
  lens <- c(length(datetime), length(water_input), length(flow))
  if (length(unique(lens)) != 1) {
    stop("Forcing data is not consistent in term of the data length!")
  }

  # 2. datetime & is_daily correspondence
  if (is_daily & !is.Date(datetime)) {
    stop("datetime should be as an instsance of Class Date with daily data!")
  }
  if (!is_daily & !is.POSIXct(datetime)) {
    stop("datetime should be as an instsance of Class POSIXct with hourly data!")
  }

  # 3. missing in water_input is not allowed
  if (sum(is.na(water_input)) != 0) {
    stop("Missing data is not allowed in water_input!")
  }

  # 4. Controlling empty flow
  if (length(na.omit(flow)) < 1) {
    stop("flow should have available values!")
  }

  # 5. Controlling class of other variables
  if (!is.numeric(water_input) | !is.numeric(flow)) {
    stop("water_input and flow should be numeric vectors!")
  }

  # 6. Warning for short data
  if (lens[1] < Lmax) {
    warning("Length of forcing data should be higher than Lmax! Your data might be too short for event delineation!")
  }

  # 7. baseflow warning
  if (!baseflow_flag) {
    warning("You should have provided stormflow with baseflow values deducted from streamflow.\nOtherwise, runoff volumes and ratios will not be hydrologically correct!")
  }

  # 8. events with NA flows warning
  if (!exclude_na) {
    warning("Some events may have missing flow values treated as zero for runoff volume and runoff ratio calculations!")
  }

  # 9. Lmax should be integer & Rmin should be a numeric. Checking logical inputs
  # as well
  if (!is.numeric(Lmax) || Lmax %% 1 != 0) {
    stop("Lmax must be a whole number (integer).")
  }
  if (!is.numeric(Rmin)) {
    stop("Rmin should be a numeric!")
  }
  if (!is.logical(is_daily)) {
    stop("is_daily should be TRUE or FALSE!")
  }
  if (!is.logical(baseflow_flag)) {
    stop("baseflow_flag should be TRUE or FALSE!")
  }
  if (!is.logical(exclude_na)) {
    stop("exclude_na should be TRUE or FALSE!")
  }

  # Execution of event delineation + adding further attributes -----------------
  # adjusting flow and water_input values if it's daily
  multiple <- 1
  if (is_daily) {
    multiple <- 24
  }
  flow_adj <- flow / multiple
  water_input_adj <- water_input / multiple
  timestamps <- c(1:lens[1])

  dmca_res <- EVENT_IDENTIFICATION_DMCA(
    water_input_adj,
    flow_adj,
    timestamps,
    Rmin,
    Lmax
  )

  # event identification results with following columns
  # waterInput_start, waterInput_end, waterInput_duration, waterInput_volume,
  # stormflow_start, stormflow_end, stormflow_duration, stormflow_volume, and
  # runoff_ratio
  event_analysis_res <- EVENT_ANALYSIS(
    dmca_res$BEGINNING_RAIN,
    dmca_res$END_RAIN,
    dmca_res$BEGINNING_FLOW,
    dmca_res$END_FLOW,
    water_input_adj,
    flow_adj,
    timestamps,
    datetime,
    baseflow_flag,
    multiple
  ) %>%
    # This filtering has to be done according to Giani et al. (2022)
    dplyr::filter(runoff_ratio > 0,
                  runoff_ratio <= 1)

  forcing_df <- data.frame(
    DateTime = datetime,
    water_input = water_input,
    flow = flow
  )

  if (nrow(event_analysis_res) == 0) {
    # This mostly happens in dry climate where water_input and flow pulse are
    # located with a distance much larger than the selected Lmax value.
    warning("Could not identify any events! Try again with a larger Lmax value!")
    return(event_analysis_res)
  } else {
    # Getting missing ratio attribute
    flow_missing_ratios <- as.numeric(nrow(event_analysis_res))
    for (j in c(1:nrow(event_analysis_res))) {
      stormflow_start_iter <- event_analysis_res$stormflow_start[j]
      stormflow_end_iter <- event_analysis_res$stormflow_end[j]
      flow_vec <- forcing_df %>%
        dplyr::filter(
          DateTime >= stormflow_start_iter,
          DateTime <= stormflow_end_iter,
        ) %>%
        pull(flow)
      flow_missing_ratios[j] <- sum(is.na(flow_vec)) / length(flow_vec)
    }
    event_analysis_res$flow_missing_fraction = flow_missing_ratios
  }

  if (exclude_na) {
    return(
      event_analysis_res %>%
        dplyr::filter(flow_missing_fraction == 0)
    )
  } else {
    return(event_analysis_res)
  }
}


# test -----------------
# load("data/testcatchment_precip.RDS")
# load("data/testcatchment_streamflow.RDS")
# res <- event_identifier(
#   datetime = df_p$Date,
#   water_input = df_p$precipitation_mmd,
#   flow = df_q$streamflow_mmd
# )
# plot(res$waterInput_volume, res$stormflow_volume)


