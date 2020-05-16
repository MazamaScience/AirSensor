#' @export
#' @importFrom MazamaCoreUtils logger.isInitialized logger.trace logger.warn logger.error
#'
#' @importFrom rlang .data
#'
#' @title Download PurpleAir timeseries data
#'
#' @param id PurpleAir sensor 'deviceDeploymentID'.
#' @param label PurpleAir sensor 'label'.
#' @param pas PurpleAir Synoptic \emph{pas} object.
#' @param startdate Desired start time (ISO 8601).
#' @param enddate Desired end time (ISO 8601).
#' @param timezone Timezone used to interpret start and end dates.
#' @param baseUrl Base URL for Thingspeak API.
#' 
#' @return List containing multiple timeseries dataframes.
#' 
#' @description Downloads timeseries data for a specific PurpleAir sensor 
#' from the ThingSpeak API and parses the content into individual dataframes. This 
#' function will always return dataframes with the appropriate columns even if no 
#' data are returned from ThingSpeak.
#'
#' The returned list contains the following dataframes:
#' 
#' \itemize{
#' \item{\code{meta} -- \code{pas} records for the specified sensor}
#' \item{\code{A_PRIMARY} -- channel A primary dataset}
#' \item{\code{A_SECONDARY} -- channel A secondary dataset}
#' \item{\code{B_PRIMARY} -- channel B primary dataset}
#' \item{\code{B_SECONDARY} -- channel B secondary dataset}
#' }
#' 
#' These dataframes contain \strong{ALL} data available from ThingSpeak for the
#' specified sensor and time period.
#' 
#' See the references.
#' 
#' @references https://www2.purpleair.com/community/faq#!hc-sd-card-csv-file-header
#' 
#' @examples
#' \dontrun{
#' library(AirSensor)
#' 
#' setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
#' 
#' pas <- pas_load()
#' 
#' id <- '78df3c292c8448f7_21257'
#' 
#' label <- NULL
#' startdate <- NULL
#' enddate <- NULL
#' timezone <- NULL
#' baseUrl <- "https://api.thingspeak.com/channels/"
#' 
#' pat_rawList <- pat_downloadParseData(
#'   id = "78df3c292c8448f7_21257",
#'   pas = pas
#' )
#' 
#' lapply(pat_rawList, head)
#' }

pat_downloadParseData <- function(
  id = NULL,
  label = NULL,
  pas = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  baseUrl = "https://api.thingspeak.com/channels/"
) {
  
  # NOTE:  This function should never stop(). Instead, if no data are returned
  # NOTE:  from ThingSpeak, it will create log messages and return an empty
  # NOTE:  dataframe with the proper columns so that pat_createNew() can use
  # NOTE:  bind_rows().
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(baseUrl)
  
  # Get the deviceDeploymentID
  if ( is.null(id) && is.null(label) ) {
    
    stop(paste0("label or id must be provided"))
    
  } else if ( is.null(id) && !is.null(label) ) {
    
    if ( is.null(pas) )
      stop(paste0("pas must be provided when loading by label"))
    
    if ( !label %in% pas$label )
      stop(sprintf("label '%s' is not found in the 'pas' object", label))
    
    # Get the deviceDeploymentID from the label
    pattern <- paste0("^", label, "$")
    deviceDeploymentID <- pas_getDeviceDeploymentIDs(pas, pattern = pattern)
    
    if ( length(deviceDeploymentID) > 1 )
      stop(sprintf("label '%s' matches more than one sensor", label))
    
  } else {
    
    # Use id whenever it is defined, potentially ignoring label
    deviceDeploymentID <- id
    
  }
  
  # ----- Determine date sequence ----------------------------------------------
  
  # Find a single, parent record
  pas_single <-
    pas %>%
    dplyr::filter(is.na(.data$parentID)) %>%
    dplyr::filter(.data$deviceDeploymentID == !!deviceDeploymentID)
  
  if ( nrow(pas_single) > 1 ) {
    stop(paste0("Multilpe sensors share deviceDeploymentID: ",
                deviceDeploymentID, "'"))
  } 
  
  # Get the timezone associated with this sensor
  if ( is.null(timezone) ) {
    timezone <-
      pas_single %>%
      dplyr::pull(.data$timezone)
  }
  
  # Create a valid dateRange
  if ( !is.null(startdate) && !is.null(enddate) ) {
    # Don't require day boundaries
    dateRange <- MazamaCoreUtils::timeRange(startdate, 
                                            enddate, 
                                            timezone = timezone)
  } else {
    # Default to 7 days with day boundaries
    dateRange <- MazamaCoreUtils::dateRange(startdate, 
                                            enddate, 
                                            timezone, 
                                            days = 7,
                                            unit = "min")
  }
  
  startString <- strftime(dateRange[1], "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  endString <- strftime(dateRange[2], "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  
  # Determine which channel was given and access the other channel from it
  if ( is.na(pas_single$parentID) ) {
    A_meta <- pas_single
    B_meta <- dplyr::filter(pas, .data$parentID == A_meta$ID)
  } else {
    B_meta <- pas_single
    A_meta <- dplyr::filter(pas, .data$ID == B_meta$parentID)
  }
  
  # Get identifiers from the A channel
  sensorID <- A_meta$ID
  sensorLabel <- A_meta$label
  
  # Combine channel A and B monitor metadata
  meta <- dplyr::bind_rows(A_meta, B_meta)

  # ----- Request data from ThingSpeak -----------------------------------------
  
  A_PRIMARY <- 
    .getThingSpeakDataframe(
      meta,
      channel = "A",
      dataset = "PRIMARY",
      startString = startString,
      endString = endString,
      baseUrl = baseUrl
    ) %>%
    dplyr::distinct()
  
  A_SECONDARY <- 
    .getThingSpeakDataframe(
      meta,
      channel = "A",
      dataset = "SECONDARY",
      startString = startString,
      endString = endString,
      baseUrl = baseUrl
    ) %>%
    dplyr::distinct()
  
  B_PRIMARY <- 
    .getThingSpeakDataframe(
      meta,
      channel = "B",
      dataset = "PRIMARY",
      startString = startString,
      endString = endString,
      baseUrl = baseUrl
    ) %>%
    dplyr::distinct()
  
  B_SECONDARY <- 
    .getThingSpeakDataframe(
      meta,
      channel = "B",
      dataset = "SECONDARY",
      startString = startString,
      endString = endString,
      baseUrl = baseUrl
    ) %>%
    dplyr::distinct()
  
  # ----- Return ---------------------------------------------------------------
  
  returnList <- list(
    meta = meta,
    A_PRIMARY = A_PRIMARY,
    A_SECONDARY = A_SECONDARY,
    B_PRIMARY = B_PRIMARY,
    B_SECONDARY = B_SECONDARY
  )
  
  return(returnList)
    

}


# ===== INTERNAL FUNCTIONS =====================================================


#' @keywords internal
#'
.getThingSpeakDataframe <- function(
  meta,
  channel = NULL,
  dataset = NULL,
  startString = NULL,
  endString = NULL,
  baseUrl = "https://api.thingspeak.com/channels/"
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(meta)
  MazamaCoreUtils::stopIfNull(channel)
  MazamaCoreUtils::stopIfNull(dataset)
  MazamaCoreUtils::stopIfNull(startString)
  MazamaCoreUtils::stopIfNull(endString)
  MazamaCoreUtils::stopIfNull(baseUrl)
  
  channel <- toupper(channel)
  dataset <- toupper(dataset)
  
  if ( !channel %in% c("A", "B") )
    stop(sprintf("Channel '%s' is not recognized.", channel))
  
  if ( !dataset %in% c("PRIMARY", "SECONDARY") )
    stop(sprintf("Dataset '%s' is not recognized.", dataset))
  
  # ----- create URL -----------------------------------------------------------

  if ( channel == "A" ) {
    channelMeta <- meta[is.na(meta$parentID),]
  }  else {
    channelMeta <- meta[!is.na(meta$parentID),]
  }
  
  if ( dataset == "PRIMARY" ) {
    ID <- channelMeta$THINGSPEAK_PRIMARY_ID
    READ_KEY <- channelMeta$THINGSPEAK_PRIMARY_ID_READ_KEY
  } else {
    ID <- channelMeta$THINGSPEAK_SECONDARY_ID
    READ_KEY <- channelMeta$THINGSPEAK_SECONDARY_ID_READ_KEY
  }

  webserviceUrl <-
    paste0(
      baseUrl,
      ID,
      "/feeds.csv?api_key=", READ_KEY,
      "&start=", startString,
      "&end=", endString
    )
  
  # ----- Prepare parsing ------------------------------------------------------
  
  # NOTE:  See documents/Using_PA_Data_2020-05-15.pdf
  
  if ( channel == "A" && dataset == "PRIMARY" ) {
    
    # > readr::read_csv(webserviceUrl) %>% head()
    # # A tibble: 6 x 10
    #   created_at              entry_id field1 field2 field3 field4 field5 field6 field7 field8
    #   <chr>                      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    # 1 2020-05-08 07:01:18 UTC   129534   4.18   5.95   6.53   3767    -90     63     41   5.95
    # 2 2020-05-08 07:03:24 UTC   129535   3.74   5.76   5.97   3769    -90     62     41   5.76
    
    col_types <- "cidddidddd"
    
    col_names <- c(
      "created_at", "entry_id", 
      "pm1.0_cf1", "pm2.5_cf1", "pm10.0_cf1", "uptime",
      "rssi", "temperature", "humidity", "pm2.5_atm"
    )
    
    # In case of trouble, build up a tibble with a single record full of NAs
    emptyTbl <- dplyr::tibble(
      "created_at" = as.character(NA),
      "entry_id" = as.character(NA),
      "pm1.0_cf1" = as.numeric(NA),
      "pm2.5_cf1" = as.numeric(NA),
      "pm10.0_cf1" = as.numeric(NA),
      "uptime" = as.integer(NA),
      "rssi" = as.numeric(NA),
      "temperature" = as.numeric(NA),
      "humidity" = as.numeric(NA),
      "pm2.5_atm" = as.numeric(NA),
    )
    
  } else if ( channel == "A" && dataset == "SECONDARY" ) {
    
    # > readr::read_csv(webserviceUrl) %>% head()
    # # A tibble: 6 x 10
    #   created_at              entry_id field1 field2 field3 field4 field5 field6 field7 field8
    #   <chr>                      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    # 1 2020-05-08 07:01:20 UTC   129558   979.   268.   33.0   2.05   0.54   0.26   4.18   6.53
    # 2 2020-05-08 07:03:26 UTC   129559   907.   249.   30.9   2.69   0.33   0      3.74   5.97
    
    col_types <- "cidddddddd"

    col_names <- c(
      "created_at", "entry_id", 
      "counts_0.3", "counts_0.5", "counts_1.0", "counts_2.5",
      "counts_5.0", "counts_10.0", "pm1.0_atm", "pm10.0_atm"
    )
    
    # In case of trouble, build up a tibble with a single record full of NAs
    emptyTbl <- dplyr::tibble(
      "created_at" = as.character(NA),
      "entry_id" = as.character(NA),
      "counts_0.3" = as.numeric(NA),
      "counts_0.5" = as.numeric(NA),
      "counts_1.0" = as.numeric(NA),
      "counts_2.5" = as.numeric(NA),
      "counts_5.0" = as.numeric(NA),
      "counts_10.0" = as.numeric(NA),
      "pm1.0_atm" = as.numeric(NA),
      "pm10.0_atm" = as.numeric(NA),
    )
    
  } else if ( channel == "B" && dataset == "PRIMARY" ) {
    
    # > readr::read_csv(webserviceUrl) %>% head()
    # # A tibble: 6 x 10
    #   created_at              entry_id field1 field2 field3 field4 field5 field6 field7 field8
    #   <chr>                      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <lgl>   <dbl>
    # 1 2020-05-08 07:01:22 UTC   129570   21.4   26.1   26.5  19512      0  1022. NA       25.6
    # 2 2020-05-08 07:03:28 UTC   129571   19.7   24.5   25.0  19848      0  1022. NA       24.1
    
    col_types <- "cidddidddd"

    col_names <- c(
      "created_at", "entry_id", 
      "pm1.0_cf1", "pm2.5_cf1", "pm10.0_cf1", "heap",
      "adc0", "pressure", "bsec_iaq", "pm2.5_atm"
    )
    
    # In case of trouble, build up a tibble with a single record full of NAs
    emptyTbl <- dplyr::tibble(
      "created_at" = as.character(NA),
      "entry_id" = as.character(NA),
      "pm1.0_cf1" = as.numeric(NA),
      "pm2.5_cf1" = as.numeric(NA),
      "pm10.0_cf1" = as.numeric(NA),
      "heap" = as.integer(NA),
      "adc0" = as.numeric(NA),
      "pressure" = as.numeric(NA),
      "bsec_iaq" = as.numeric(NA),
      "pm2.5_atm" = as.numeric(NA),
    )
    
    
  } else if ( channel == "B" && dataset == "SECONDARY" ) {
    
    # > readr::read_csv(webserviceUrl) %>% head()
    # # A tibble: 6 x 10
    #   created_at              entry_id field1 field2 field3 field4 field5 field6 field7 field8
    #   <chr>                      <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    # 1 2020-05-08 07:01:23 UTC   129569  4786.  1260.   43.5   14.4   0.71   0.36   19.8   26.5
    # 2 2020-05-08 07:03:30 UTC   129570  4429.  1171.   43.4   13.6   0.42   0      18.7   25.0
    
    col_types <- "cidddddddd"

    col_names <- c(
      "created_at", "entry_id", 
      "counts_0.3", "counts_0.5", "counts_1.0", "counts_2.5",
      "counts_5.0", "counts_10.0", "pm1.0_atm", "pm10.0_atm"
    )
    
    # In case of trouble, build up a tibble with a single record full of NAs
    emptyTbl <- dplyr::tibble(
      "created_at" = as.character(NA),
      "entry_id" = as.character(NA),
      "counts_0.3" = as.numeric(NA),
      "counts_0.5" = as.numeric(NA),
      "counts_1.0" = as.numeric(NA),
      "counts_2.5" = as.numeric(NA),
      "counts_5.0" = as.numeric(NA),
      "counts_10.0" = as.numeric(NA),
      "pm1.0_atm" = as.numeric(NA),
      "pm10.0_atm" = as.numeric(NA),
    )
    
  }
  
  # ----- Download/parse -------------------------------------------------------
  
  # NOTE:  using Hadley Wickham style:
  # NOTE:  https://github.com/hadley/httr/blob/master/vignettes/quickstart.Rmd
  r <- httr::GET(webserviceUrl)
  
  # Handle the response
  status_code <- httr::status_code(r)
  content <- httr::content(r, as = "text") # don't interpret
  
  if ( httr::http_error(r) ) { 
    
    # Web service error response
    
    err_msg <- sprintf(
      "Channel %s web service error %s from:\n  %s\n\n%s",
      channel,
      status_code, 
      webserviceUrl,
      httpcode::http_code(status_code)$explanation
    )
    
    if ( logger.isInitialized() )
      logger.warn("Channel %s: %s", channel, err_msg)
    
    message(err_msg)
    
    # TODO:  Return empty tibble
    # Now search for an entry_id we won't find to end up with an empty tibble
    # with the correct column names.
    dataTbl <-
      emptyTbl %>%
      dplyr::filter(.data$entry_id == "Rumplestiltskin")
    
  } else { 
    
    # Web service success response
    
    dataTbl <-
      readr::read_csv(
        content,
        skip = 1,
        col_types = col_types,
        col_names = col_names
      ) %>%
      dplyr::mutate(
        created_at = MazamaCoreUtils::parseDatetime(.data$created_at, timezone = "UTC")
      )
    
  }
    
  return(dataTbl)
  
}
  
# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(AirSensor)
  
  setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1") # SCAQMD sensors
  
  pas <- pas_load()
  
  # id <- "122c6aed66d0c29c_21067"
  # id <- "e8582a00f22db27f_12947"
  # id <- "cb5adde8c100d4fa_13081"
  # id <- "eaf020885e1bf678_21049"
  id <- '78df3c292c8448f7_21257'
  
  label <- NULL
  startdate <- NULL
  enddate <- NULL
  timezone <- NULL
  baseUrl <- "https://api.thingspeak.com/channels/"
  
  pat_rawList <- pat_downloadParseData(
    id,
    label,
    pas,
    startdate,
    enddate,
    timezone,
    baseUrl
  )
  
}
