#' @export
#' 
#' @title Plot time series values in convential calendar format
#'
#' @param sensor an 'airsensor' object
#' @param colors colors 
#' @param breaks breaks
#' @param limits limits
#' @param title Optional title. If \code{NULL}, a default title will be constructed.
#' 
#' @description Function for plotting PM2.5 concentration in a calendar format. 
#' 
#' @seealso 
#' \url{http://davidcarslaw.github.io/openair/reference/calendarPlot.html} 
#' 
#' @return a plot and dataframe
#'
#' @examples
#' \donttest{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#'
#' sensor <- 
#'   sensor_loadYear("scaqmd", 2020) %>%
#'   sensor_filterMeta(label == "SCSC_33") %>%
#'   sensor_filterDate(20200101, 20210101, timezone = "America/Los_Angeles")
#' 
#' # Calendar plot
#' sensor_calendarPlot(sensor)
#' }

sensor_calendarPlot <- function(
  sensor = NULL,
  colors = NULL,
  breaks = NULL,
  labels = NULL,
  limits = c(0, 100),
  title = NULL
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
  
  # ----- Create datetime_LST --------------------------------------------------
  
  # NOTE:  In order to use openair::calendarPlot() we must convert the time 
  # NOTE:  axis from UTC to LST (local-standard-time all-year-round).
  
  # Calculate Local Standard Time
  
  UTC_offset <-
    MazamaSpatialUtils::SimpleTimezones@data %>% 
    dplyr::filter(timezone == sensor$meta$timezone) %>% 
    dplyr::pull(UTC_offset)
  
  datetime_LST <-
    sensor$data$datetime + lubridate::dhours(UTC_offset)
  
  # ----- Assemble data --------------------------------------------------------
    
  # PM2.5 df 
  mydata <- 
    dplyr::tibble(
      "date" = datetime_LST, 
      "pm25" = sensor$data[[2]] 
    )
  
  # ----- Plot defaults --------------------------------------------------------
  
  if ( is.null(colors) )
    colors <- "heat"
    
  if ( is.null(breaks) ) 
    breaks <- NA
  
  if ( is.null(labels) ) 
    labels <- NA
  
  if ( is.null(title) ) {
    title <- sprintf(
      "%d -- %s sensor: %s",
      lubridate::year(datetime_LST[round(length(datetime_LST)/2)]),
      sensor$meta$communityRegion, 
      sensor$meta$siteName
    ) %>%
      stringr::str_trim()
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
      data.thresh = 0,
      labels = labels,
      breaks = breaks,
      # w.shift = 0,
      remove.empty = FALSE,
      main = title
      # key.header = "",
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
  
  colors <- "heat"
  breaks <- c(0,5,10,20,50,100,1e6)
  labels <- letters[1:6]
  limits <- c(0, 100)
  title <- NULL
  
  breaks <- PWFSLSmoke::AQI$breaks_24
  colors <- PWFSLSmoke::AQI$colors
  labels <- PWFSLSmoke::AQI$names
  
  
  
  sensor_calendarPlot(
    sensor,
    colors = colors,
    limits = limits,
    breaks = breaks,
    labels = labels,
    title = title
  )
  
  
}

