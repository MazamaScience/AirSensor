#' @export
#' @importFrom rlang .data
#' 
#' @title Area filtering for \emph{pas} objects
#' 
#' @param pas \emph{pa_synoptic} dataframe
#' @param w west edge of bounding box (deg E)
#' @param e east edge of bounding box (deg E)
#' @param s south edge of bounding box (deg N)
#' @param n north edge of bounding box (deg N)
#' 
#' @description Filters \emph{pa_synoptic} objects based on a bounding box.
#' 
#' @return A subset of the incoming \code{pas}.
#' 
#' @seealso \link{pas_load}
#' @examples
#' pas <- example_pas
#' range(pas$longitude)
#' range(pas$latitude)
#' scsb <- 
#'   pas %>%
#'   pas_filterArea(w = -118.10,
#'                  e = -118.07,
#'                  s = 33.75,
#'                  n = 33.78)
#' range(scsb$longitude)
#' range(scsb$latitude)
#'
pas_filterArea <- function(
  pas,
  w = NULL,
  e = NULL,
  s = NULL,
  n = NULL
) { 
  
  # Validate parameters --------------------------------------------------------
  
  if ( !pas_isPas(pas) )
    stop("Required parameter 'pas' is not a valid 'pa_synoptic' object.")
  
  if ( pas_isEmpty(pas) )
    stop("Required parameter 'pas' has no data.")
  
  if ( is.null(w) ) w <- min(pas$longitude, na.rm = TRUE)
  if ( is.null(e) ) e <- max(pas$longitude, na.rm = TRUE)
  if ( is.null(s) ) s <- min(pas$latitude, na.rm = TRUE)
  if ( is.null(n) ) n <- min(pas$latitude, na.rm = TRUE)
  
  # Filter the tibble ----------------------------------------------------------
  
  pas <- 
    pas %>% 
    dplyr::filter(.data$longitude >= w & .data$longitude <= e) %>%
    dplyr::filter(.data$latitude >= s & .data$latitude <= n)
  
  return(pas)
  
}
