#' Extract Baseflow Component from Streamflow Time Series
#'
#' Identifies stormflow events and estimates a baseflow signal from a total streamflow
#' time series using the DMCA method.
#' The function assumes total streamflow is provided and subtracts a computed baseflow curve
#' based on event timing derived from water input and flow time series.
#'
#' @param water_input Numeric vector. Time series of water input (e.g., precipitation),
#'        in daily or hourly resolution. Must not contain \code{NA}.
#' @param flow Numeric vector. Time series of total streamflow or catchment response,
#'        in the same temporal resolution and units as \code{water_input}.
#' @param is_daily Logical. Whether the input data is in daily time scale (\code{TRUE}) or hourly (\code{FALSE}).
#'        Default is \code{TRUE}. Internally adjusts both series to an hourly resolution.
#' @param Rmin Numeric. Minimum rainfall intensity (in hourly units) used to delimit storm events. Default is \code{0.02}.
#' @param Lmax Integer. Maximum window length (in time steps) to explore catchment response time. Default is \code{100}.
#'        Should be chosen based on temporal resolution (e.g., 100 hours or 100 days).
#'
#' @details
#' This function computes the baseflow component by identifying the start and end of stormflow events,
#' estimating a separation line (baseflow) between events. It is based on the same DMCA logic
#' used in \code{\link{event_identifier}}, with post-processing to extract the underlying
#' baseflow signal.
#'
#' The water_input and flow inputs are assumed to be in the same units and temporal resolution.
#' If data is daily, the function converts both to an hourly scale by dividing values by 24 to
#' match the DMCA's hourly processing, then scales the baseflow result back to daily.
#'
#' @return A numeric vector representing the estimated baseflow component, in the same
#'         units and time scale as the input \code{flow} vector.
#'
#' @seealso \code{\link{event_identifier}}
#' @importFrom lubridate is.Date is.POSIXct
#' @importFrom stats na.omit
#' @importFrom utils tail
#' @export
baseflow_extractor <- function(water_input, flow, is_daily = TRUE,
                               Rmin = 0.02, Lmax = 100) {
  # Controlling Inputs ---------------------------------------------------------
  # 1. Input lengths
  lens <- c(length(water_input), length(flow))
  if (length(unique(lens)) != 1) {
    stop("Forcing data is not consistent in term of the data length!")
  }

  # 2. missing in water_input is not allowed
  if (sum(is.na(water_input)) != 0) {
    stop("Missing data is not allowed in water_input!")
  }

  # 3. Controlling empty flow
  if (length(na.omit(flow)) < 1) {
    stop("flow should have available values!")
  }

  # 4. Controlling class of other variables
  if (!is.numeric(water_input) | !is.numeric(flow)) {
    stop("water_input and flow should be numeric vectors!")
  }

  # 5. Warning for short data
  if (lens[1] < Lmax) {
    warning("Length of forcing data should be higher than Lmax! Your data might be too short for event delineation!")
  }

  # 6. Lmax should be integer & Rmin should be a numeric. Checking is_daily as well.
  if (!is.numeric(Lmax) || Lmax %% 1 != 0) {
    stop("Lmax must be a whole number (integer).")
  }
  if (!is.numeric(Rmin)) {
    stop("Rmin should be a numeric!")
  }
  if (!is.logical(is_daily)) {
    stop("is_daily should be TRUE or FALSE!")
  }

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
  baseflow_result <- BASEFLOW_CURVE(
    BEGINNING_FLOW = dmca_res$BEGINNING_FLOW,
    END_FLOW = dmca_res$END_FLOW,
    flow = flow_adj,
    time = timestamps
  ) * multiple

  return(baseflow_result)
}
