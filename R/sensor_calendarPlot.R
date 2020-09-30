#' @export
#' @importFrom rlang .data
#' 
#' @title Plot time series values in conventional calendar format
#'
#' @param sensor An 'airsensor' object
#' @param colors Colours to be used for plotting. Options include "aqi", "scaqmd", “default”, 
#' “increment”, “heat”, “jet” and \pkg{RColorBrewer} colours — see the \pkg{openair}
#' \code{openColours} function for more details. For user defined the user can 
#' supply a list of colour names recognised by R (type \code{colours()} to see 
#' the full list). An example would be cols = \code{c("yellow", "green", "blue")} 
#' @param breaks 	If a categorical scale is required then these breaks will be 
#' used. For example, \code{breaks = c(0, 50, 100, 1000)}. In this case “good” 
#' corresponds to values berween 0 and 50 and so on. Users should set the 
#' maximum value of breaks to exceed the maximum data value to ensure it is 
#' within the maximum final range e.g. 100–1000 in this case.
#' @param labels If a categorical scale is required then these labels will be 
#' used. Note there is one less label than break. For example, 
#' \code{labels = c("good", "bad", "very bad")}. breaks must also be supplied if 
#' labels are given.
#' @param limits Use this option to manually set the colour scale limits. This 
#' is useful in the case when there is a need for two or more plots and a 
#' consistent scale is needed on each. Set the limits to cover the maximimum 
#' range of the data for all plots of interest. For example, if one plot had 
#' data covering 0–60 and another 0–100, then set \code{limits = c(0, 100)}. 
#' Note that data will be ignored if outside the limits range.
#' @param title Optional title. If \code{NULL}, a default title will be constructed.
#' @param data.thresh Data capture threshold passed to \code{openair::timeAverage()}. 
#' For example, \code{data.thresh = 75} means that at least 75% of the data must 
#' be available in a day for the value to be calculate, else the data is removed.
#' 
#' @description Function for plotting PM2.5 concentration in a calendar format. 
#' This function wraps the \pkg{openair} \code{calendarPlot()} function.
#' 
#' @details
#' Data are trimmed to the local-time year or month boundaries as appropriate.
#' 
#' Two special options are provided to specify a set of \code{colors}, 
#' \code{breaks} and \code{labels}.
#' 
#' Using \code{colors = "aqi"} will use US EPA Air Quality Index colors and breaks
#' defined by \code{breaks <- c(-Inf, 12, 35.5, 55.5, 150.5, 250.5, Inf)}.
#' 
#' Using \code{colors = "scaqmd"} will use a custom set of colors and breaks
#' defined by \code{breaks <- c(-Inf, 12, 35, 55, 75, Inf)}.
#' 
#' @note Daily averages are calculated using LST (Local Standard Time) day 
#' boundaries as specified by the US EPA. LST assumes that standard time applies
#' all year round and guarantees that every day has 24 hours -- no "spring forward"
#' or "fall back". Because of this, LST daily averages calculated during months
#' where daylight savings time is in effect will differ very slightly from daily 
#' averages calculated using local "clock time". 
#' 
#' @seealso 
#' \url{https://davidcarslaw.github.io/openair/reference/calendarPlot.html} 
#' 
#' @return A plot and an object of class "openair".
#'
#' @references \href{https://aqs.epa.gov/aqsweb/documents/AQS_Data_Dictionary.html}{EPA AQS Data Dictionary}
#' 
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#'
#' # Monthly plot
#' sensor <- 
#'   sensor_loadMonth("scaqmd", 202007) %>%
#'   sensor_filterMeta(label == "SCSC_33")
#' 
#' sensor_calendarPlot(sensor)
#' 
#' # Annual plot
#' sensor <- 
#'   sensor_loadYear("scaqmd", 2020) %>%
#'   sensor_filterMeta(label == "SCSC_33")
#' 
#' sensor_calendarPlot(sensor)
#' 
#' # SCAQMD colors
#' sensor_calendarPlot(sensor, "scaqmd")
#' 
#' # Custom continuous color palette from RColorBrewer
#' sensor_calendarPlot(
#'   sensor,
#'   colors = "BuPu",
#'   title = "2020 Purple Scale",
#'   limits = range(sensor$data[,-1], na.rm = TRUE) # don't use data$datetime
#' )
#' 
#' 
#' # Custom categorical colors
#' sensor_calendarPlot(
#'   sensor,
#'   colors = c("springgreen2", "gold", "tomato3"),
#'   breaks = c(-Inf, 25, 50, Inf),
#'   labels = c("Good", "Fair", "Poor"),
#'   title = "2020 -- Air Quality Stoplight"
#' )
#' 
#' }

sensor_calendarPlot <- function(
  sensor = NULL,
  colors = NULL,
  breaks = NULL,
  labels = NULL,
  limits = c(0, 100),
  title = NULL,
  data.thresh = 50
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(sensor)
  
  if ( !sensor_isSensor(sensor) )
    stop("Parameter 'sensor' is not a valid 'airsensor' object.") 
  
  if ( sensor_isEmpty(sensor) ) 
    stop("Required parameter 'sensor' has no data.")
  
  if ( nrow(sensor$meta) == 0 )
    stop("Parameter 'sensor' contains no ensors")
  
  if ( nrow(sensor$meta) > 1 )
    stop("Parameter 'sensor' contains more than one sensor")
  
  # ----- Trim data ------------------------------------------------------------

  # NOTE:  Because data returned by sensor_loadYear() and sensor_loadMonth() are 
  # NOTE:  padded front and back, we want to trim to local time year before proceeding.
  
  year <- 
    sensor$data$datetime[round(length(sensor$data$datetime)/2)] %>%
    lubridate::year()
  
  month <- 
    sensor$data$datetime[round(length(sensor$data$datetime)/2)] %>%
    lubridate::month()
  
  sensor <- 
    sensor %>%
    sensor_filterDate(
      paste0(year, "0101"),
      paste0((year + 1), "0101"),
      timezone = sensor$meta$timezone
    )
  
  # Check to see if we should trim to a specific month
  dayCount <-
    difftime(max(sensor$data$datetime), min(sensor$data$datetime), units = "days") %>%
    as.numeric()
  
  singleMonth <- FALSE
  
  # Trim to a specific month
  if ( dayCount < 35 ) {
    
    singleMonth <- TRUE
    
    startdate <- 
      sensor$data$datetime[round(length(sensor$data$datetime)/2)] %>%
      lubridate::with_tz(tzone = sensor$meta$timezone) %>%
      lubridate::floor_date(unit = "month")
    
    enddate <-
      {startdate + lubridate::ddays(40)} %>%
      lubridate::with_tz(tzone = sensor$meta$timezone) %>%
      lubridate::floor_date(unit = "month")
    
    # NOTE:  Because we use LST day boundaries, we need to adjust day boundaries
    # NOTE:  by one hour during Daylight Savings to make sure that we end up
    # NOTE:  with only a single month in the final plot.
    
    if ( lubridate::dst(startdate) ) {
      startdate <- startdate + lubridate::dhours(1)
      enddate <- enddate + lubridate::dhours(1)
    }
    
    # NOTE:  Use sensor_filterDatetime() becuase we don't want to trim to the
    # NOTE:  local clock-time day.
    
    sensor <- 
      sensor %>%
      sensor_filterDatetime(
        startdate,
        enddate,
        timezone = sensor$meta$timezone
      )
    
  }
  
  # ----- Create datetime_LST --------------------------------------------------
  
  # NOTE:  In order to use openair::calendarPlot() we must convert the time 
  # NOTE:  axis from UTC to LST (local-standard-time all-year-round). To 
  # NOTE:  guarantee that the time axis is monotonic (no spring forward or 
  # NOTE:  fall back), we have to convert it to UTC but with the hours that
  # NOTE:  would have been seen in local standard time.
  # NOTE:  
  # NOTE:  Thus, we have to create a UTC time that is shifted by the UTC offset.
  
  # Calculate Local Standard Time as it would appear if it were UTC
  
  UTC_offset <-
    MazamaSpatialUtils::SimpleTimezones@data %>% 
    dplyr::filter(.data$timezone == sensor$meta$timezone) %>% 
    dplyr::pull(UTC_offset)
  
  datetime_LST_as_UTC <-
    sensor$data$datetime + lubridate::dhours(UTC_offset)
  
  # ----- Assemble data --------------------------------------------------------
    
  # PM2.5 df 
  mydata <- 
    dplyr::tibble(
      "date" = datetime_LST_as_UTC, 
      "pm25" = sensor$data[[2]] 
    )
  
  # ----- Plot defaults --------------------------------------------------------
  
  if ( is.null(colors) ) 
    colors <- "heat"
    
  if ( is.null(breaks) ) 
    breaks <- NA
  
  if ( is.null(labels) ) 
    labels <- NA
  
  key.header = "Sensor PM2.5"
  
  # Overrides for "AQI" and "SCAQMD" colors
  
  if ( length(colors) == 1 ) {
    
    if ( tolower(colors) == "aqi" ) {
      breaks <- PWFSLSmoke::AQI$breaks_24
      colors <- PWFSLSmoke::AQI$colors
      labels <- PWFSLSmoke::AQI$names
      key.header = "Air Quality Index"
    } else if ( tolower(colors) == "scaqmd" ) {
      breaks <- c(-Inf, 12, 35, 55, 75, Inf)
      colors <- c("#ABEBFF", "#3B8AFF", "#002ADE", "#9F00DE", "#6B0096")
      labels <- c("Very Low", "Low", "Medium", "High", "Very High")
      key.header = "Sensor PM2.5"
    }
    
  }
  
  if ( is.null(title) ) {
    
    middleTime <- datetime_LST_as_UTC[round(length(datetime_LST_as_UTC)/2)]
    
    if ( singleMonth ) {
      
      title <- sprintf(
        "%s -- %s sensor: %s",
        strftime(middleTime, "%B, %Y", tz = "UTC"),
        sensor$meta$communityRegion, 
        sensor$meta$siteName
      ) %>%
        stringr::str_trim()
      
    } else {
      
      title <- sprintf(
        "%s -- %s sensor: %s",
        strftime(middleTime, "%Y", tz = "UTC"),
        sensor$meta$communityRegion, 
        sensor$meta$siteName
      ) %>%
        stringr::str_trim()
      
    }
  }
  
  # ----- Create plot ----------------------------------------------------------
  
  return({
    
    openair::calendarPlot(
      mydata = mydata,
      pollutant = "pm25",
      # year = 2003,
      # month = 1:12,
      # type = "default",
      annotate = "date",
      statistic = "mean",
      cols = colors,
      limits = limits,
      # lim = NULL,
      # col.lim = c("grey30", "black"),
      # col.arrow = "black",
      # font.lim = c(1, 2),
      # cex.lim = c(0.6, 1),
      # digits = 0,
      data.thresh = data.thresh,
      labels = labels,
      breaks = breaks,
      # w.shift = 0,
      remove.empty = TRUE,
      main = title,
      key.header = key.header,
      # key.footer = "",
      # key.position = "right",
      # key = TRUE,
      # auto.text = TRUE,
      # ...
    )
    
  })
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(AirSensor)
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
  
  sensor <- 
    sensor_loadYear("scaqmd", 2020) %>%
    sensor_filterMeta(label == "SCSC_33")
  
  sensor <- 
    sensor_loadMonth("scaqmd", 202007) %>%
    sensor_filterMeta(label == "SCSC_33")
  
  sensor_calendarPlot(sensor, colors = "scaqmd")
  
  colors <- "heat"
  breaks <- c(0,5,10,20,50,100,1e6)
  labels <- letters[1:6]
  limits <- c(0, 100)
  title <- NULL
  
  colors <- "SCAQMD"
  
  colors <- "aqi"
  
  sensor_calendarPlot(
    sensor,
    colors = colors,
    limits = limits,
    breaks = breaks,
    labels = labels,
    title = title
  )
  
  
}

