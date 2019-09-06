#' @export
#'
#' @title Create a POSIXct time range
#'
#' @param starttime Desired start datetime (ISO 8601).
#' @param endtime Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret datetimes.
#'
#' @description
#' Uses incoming parameters to return a pair of \code{POSIXct} times in the
#' proper order.
#'
#' The required \code{timezone} parameter must be one of those found in
#' \code{\link[base]{OlsonNames}}.
#'
#' Datetimes can be anything that is understood by
#' \code{lubrdiate::parse_date_time()} including either of the following
#' recommended formats:
#'
#' \itemize{
#'   \item{\code{"YYYYmmddHH[MMSS]"}}
#'   \item{\code{"YYYY-mm-dd HH:MM:SS"}}
#' }
#'
#' @section POSIXct inputs:
#' When \code{starttime} or \code{endtime} are already \code{POSIXct} values,
#' they are converted to the timezone specified by \code{timezone} without
#' altering the physical instant in time the input represents. This is different
#' from the behavior of \code{\link[lubridate]{parse_date_time}} (which powers
#' this function), which will force \code{POSIXct} inputs into a new timezone,
#' altering the physical moment of time the input represents.
#'
#' @return A vector of two \code{POSIXct}s.
#'
#' @examples
#' timeRange("2019-01-08 10:12:15", "20190109101215", timezone = "UTC")
#' 
# timeRange <- function(
#   starttime = NULL,
#   endtime = NULL,
#   timezone = NULL
# ) {
# 
#   # ----- Validate parameters --------------------------------------------------
# 
#   MazamaCoreUtils::stopIfNull(starttime)
#   MazamaCoreUtils::stopIfNull(endtime)
#   MazamaCoreUtils::stopIfNull(timezone)
#   
#   if ( !timezone %in% base::OlsonNames() )
#     stop(paste0("Timezone '", timezone, "' is not recognized."))
# 
#   # ----- Prepare POSIXct inputs -----------------------------------------------
# 
#   ## NOTE on hadling POSIXct inputs:
#   #  When given a POSIXct time `lubridate::parse_date_time()` forces the time
#   #  into the timezone given to `lubridate::parse_date_time()`. This alters the
#   #  physical instant in time the original POSIXct represents, so we must
#   #  properly convert a POSIXct start or end date to the proper timezone before
#   #  passing it to `lubridate::parse_date_time()`
# 
#   if ( lubridate::is.POSIXct(starttime) )
#     starttime <- lubridate::with_tz(starttime, tzone = timezone)
# 
#   if ( lubridate::is.POSIXct(endtime) )
#     endtime <- lubridate::with_tz(endtime, tzone = timezone)
# 
#   # ----- Parse inputs ---------------------------------------------------------
# 
#   orders <- c("Ymd", "YmdH", "YmdHM", "YmdHMS")
# 
#   endtime <-
#     endtime %>%
#     lubridate::parse_date_time(orders = orders, tz = timezone)
#   
#   starttime <-
#     starttime %>%
#     lubridate::parse_date_time(orders = orders, tz = timezone)
#   
#   # ----- Order output time limits ---------------------------------------------
# 
#   if ( starttime < endtime ) {
#     tlim <- c(starttime, endtime)
#   } else {
#     # just in case
#     tlim <- c(endtime, starttime)
#   }
# 
#   return(tlim)
# 
# }
