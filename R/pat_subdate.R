#' @export
#' @title Subset PurpleAir time series intervals
#' @param pat Purple Air Timeseries "pat" object from \code{createPATimeseriesObject()}
#' @param sd A start-date following ISO 8601 date standard format
#' @param ed A end-date following ISO 8601 date standard format
#' @param days Number of days to include in the subdate interval
#' @param weeks Number of weeks to include in the subdate interval
#' @description Subset PurpleAir time series intervals by date intervals, days, 
#' and weeks.  
#' 
#' @return A subset "pat" list with \code{meta} and \code{data} elements
#' 

pat_subdate <- function(pat, sd, ed=NULL, days=NULL, weeks=NULL) {
  
  sub <-
    list(meta = pat$meta, data = .data$data)
  
  if ( !is.null(ed) ) { 
    
    sub$data <- pat$data %>% 
      filter(.data$datetime >= lubridate::ymd(sd) & 
               .data$datetime <= lubridate::ymd(ed))
    
  } else if ( !is.null(days) ) {
    
    sub$data <- pat$data %>% 
      filter(.data$datetime >= lubridate::ymd(sd) & 
               .data$datetime <= lubridate::ymd(sd) + lubridate::ddays(days))
    
  } else if ( !is.null(weeks) ) {
    
    sub$data <- pat$data %>% 
      filter(.data$datetime >= lubridate::ymd(sd) & 
               .data$datetime <= lubridate::ymd(sd) + lubridate::dweeks(weeks))
    
  } else if ( is.null(c(ed, days, weeks)) ) { 
    
    stop(paste0("Must provide a valid interval parameter.")) 
    
  }
  
  class(sub) <- c("pa_timeseries", class(sub))
  return(sub)
  
}
