#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.debug
#' 
#' @title Load latest PurpleAir synoptic data
#' 
#' @description Download, parse and enhance synoptic data from PurpleAir and
#' return the results as a useful tibble with class \code{pa_synoptic}.
#'
#' Steps include:
#' 
#' 1) Download and parse synoptic data
#'
#' 2) Replace variable with more consistent, more human readable names.
#'
#' 3) Add spatial metadata for each monitor including:
#' \itemize{
#'   \item{timezone -- olson timezone}
#'   \item{countryCode -- ISO 3166-1 alpha-2}
#'   \item{stateCode -- ISO 3166-2 alpha-2}
#' }
#'
#' 4) Convert data types from character to \code{POSIXct} and \code{numeric}.
#'
#' 5) Add distance and monitorID for the closest PWFSL monitor
#'
#' Subsetting by country may be performed by specifying the \code{countryCodes}
#' argument.
#'
#' @param baseUrl Base URL for synoptic data.
#' @param countryCodes ISO country codes used to subset the data.
#' @param includePWFSL Logical specifying whether to calculate distances from 
#'   PWFSL monitors.
#' @param lookbackDays Number of days to "look back" for valid data. Data are
#'   filtered to only include sensors with data more recent than 
#'   \code{lookbackDays} ago.
#' 
#' @return A PurpleAir Synoptic \emph{pas} object.
#' 
#' @seealso \link{pas_load}
#' @seealso \link{downloadParseSynopticData}
#' 
#' @examples
#' \dontrun{
#' initializeMazamaSpatialUtils()
#' pas <- pas_createNew()
#' pas %>% 
#'   pas_filter(stateCode == "CA") %>%
#'   pas_leaflet()
#' }

pas_createNew <- function(
  baseUrl = 'https://www.purpleair.com/json',
  countryCodes = c('US'),
  includePWFSL = TRUE,
  lookbackDays = 1
) {
  
  logger.debug("----- pas_createNew() -----")
  
  # Validate parameters --------------------------------------------------------
  
  # Guarantee uppercase codes
  countryCodes <- toupper(countryCodes)
  if ( any(!(countryCodes %in% countrycode::codelist$iso2c)) ) 
    stop("parameter 'countryCodes' has values that are not recognized as ISO-2 country codes")
  
  # Gaurantee includePWFSL is a logial value
  if ( !is.logical(includePWFSL) )
    stop("parameter 'includePWFSL' is not a logical value")
  
  # Guarantee lookbackDays at least 1
  if ( lookbackDays < 1 )
    stop("parameter 'lookbackDays' is less than one")
  
  # Load data ------------------------------------------------------------------
  
  # Download, parse and enhance synoptic data
  pas_raw <- downloadParseSynopticData(baseUrl)
  pas <- enhanceSynopticData(pas_raw, countryCodes, includePWFSL)
  
  # Filter for age
  starttime <- lubridate::now(tzone = "UTC") - lubridate::ddays(lookbackDays)
  pas <- dplyr::filter(pas, .data$lastSeenDate >= starttime)
  
  # Add a class name
  class(pas) <- c('pa_synoptic', class(pas))
  
  return(pas)
  
}

# ===== DEBUGGING ============================================================

if ( FALSE ) {
  
  baseUrl <- 'https://www.purpleair.com/json'
  countryCodes <- c('US')
  includePWFSL <- TRUE
  lookbackDays <- 1
  
}

