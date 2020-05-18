#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains case_when
#' @importFrom ggplot2 aes
#' 
#' @title Daily State of Health metric plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param ncol Number of columns in the faceted plot.
#' 
#' @description This function plots a subset of the most useful State of Health 
#' metrics calculated by the \code{pat_dailySoH} function. The function 
#' runs \code{pat_dailySoH} internally and uses the output to create 
#' the plot.
#' 
#' @seealso \link{pat_dailySoH}
#' 
#' @examples
#' library(AirSensor)
#' 
#' pat_dailySoHPlot(example_pat_failure_B)

pat_dailySoHPlot <- function(
  pat = NULL,
  ncol = 2
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ----- Create the SoH_tidy object -------------------------------------------
  
  # Calculate the SoH 
  SoH <- pat_dailySoH(pat)
  
  # Select only the useful metrics of interest from the full SoH
  SoH_sub <- dplyr::select(
    SoH, 
    "datetime", 
    "pm25_A_pctReporting",
    "pm25_B_pctReporting",
    "pm25_A_pctValid",
    "pm25_B_pctValid",
    "pm25_A_pctDC", 
    "pm25_B_pctDC", 
    "pm25_A_pm25_B_slope",
    "pm25_A_pm25_B_intercept",
    "pm25_A_pm25_B_rsquared",
    "pm25_A_temperature_rsquared"
  )
  
  # Copy the datetime column from the SoH to use later when creating the dummy 
  # data.
  datetime <- SoH_sub$datetime
  
  # Create a tidy dataframe from the SoH
  SoH_tidy <-
    SoH_sub %>%
    tidyr::gather(key = "metric", value = "value", -datetime) %>%
    # create a factor based on the metric name for expected value association
    dplyr::mutate(expectedValue = as.integer(factor(.data$metric))) 
  
  # Assign associated expected values based on the original column
  SoH_tidy <- 
    SoH_tidy %>%
    dplyr::mutate(expectedValue = case_when(
      grepl("_pctReporting", SoH_tidy$metric) ~ 100,
      grepl("_pctValid", SoH_tidy$metric) ~ 100,
      grepl("_pctDC", SoH_tidy$metric) ~ 0,
      grepl("pm25_A_pm25_B_slope", SoH_tidy$metric) ~ 1,
      grepl("pm25_A_pm25_B_intercept", SoH_tidy$metric) ~ 0,
      grepl("pm25_A_pm25_B_rsquared", SoH_tidy$metric) ~ 1,
      grepl("pm25_A_temperature_rsquared", SoH_tidy$metric) ~ 0)
    )
  
  # Create factor for ordering the facets later on
  SoH_tidy$metric <- factor(SoH_tidy$metric, 
                            levels = c(
                              "pm25_A_pctReporting",
                              "pm25_B_pctReporting",
                              "pm25_A_pctValid",
                              "pm25_B_pctValid",
                              "pm25_A_pctDC",
                              "pm25_B_pctDC",
                              "pm25_A_pm25_B_slope",
                              "pm25_A_pm25_B_intercept",
                              "pm25_A_pm25_B_rsquared",
                              "pm25_A_temperature_rsquared"
                            ))
  
  # ----- Create plot metrics ------------------------------------------------
  
  # Create the dummy metrics which contain just the min and max expected 
  # values for each metric in order to set an appropriate range in the facets
  
  pm25_A_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
  pm25_B_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
  pm25_A_pctValid <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_B_pctValid <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_A_pctDC <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_B_pctDC <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_A_pm25_B_slope <- rep_len(c(-5, 5), length.out = length(SoH_sub$datetime))
  pm25_A_pm25_B_intercept <- rep_len(c(-50, 50), length.out = length(SoH_sub$datetime))
  pm25_A_pm25_B_rsquared <- rep_len(c(0, 1), length.out = length(SoH_sub$datetime))
  pm25_A_temperature_rsquared <- rep_len(c(0, 1), length.out = length(SoH_sub$datetime))

  # Add all the dummy metrics to the dummy dataframe
  dummy <- data.frame(
    datetime,
    pm25_A_pctReporting, 
    pm25_B_pctReporting,
    pm25_A_pctValid,
    pm25_B_pctValid,
    pm25_A_pctDC,
    pm25_B_pctDC,
    pm25_A_pm25_B_slope,
    pm25_A_pm25_B_intercept,
    pm25_A_pm25_B_rsquared,
    pm25_A_temperature_rsquared
  )
  
  # Tidy the dummy data to mimic the real data
  dummy_tidy <-
    dummy %>%
    tidyr::gather(key = "metric", value = "value", -.data$datetime) %>%
    dplyr::mutate(expectedValue = as.integer(factor(.data$metric))) 
  
  colors <- c("salmon")
  
  # Pull out the station name for labeling the plot
  station_name <- pat$meta$label
  
  # ----- Create plot ----------------------------------------------------------
  
  gg <- ggplot2::ggplot(SoH_tidy, aes(.data$datetime, .data$value)) +
    # plot the flat-lined, expected values
    ggplot2::geom_line(aes(x = .data$datetime, y = .data$expectedValue),  
                       color = colors, alpha = 0.8) +
    # plot the dummy data to establish a uniform range from station to station
    ggplot2::geom_line(aes(x = dummy_tidy$datetime, y = dummy_tidy$value), 
                       color = "black", alpha = 0) +
    # plot the actual SoH data as outlined in the initial aes()
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(breaks=scales::pretty_breaks(3)) +
    ggplot2::facet_wrap(vars(SoH_tidy$metric), ncol = ncol, strip.position = c("top"), scales = "free_y") +
    ggplot2::labs(title = paste0("State of Health - ", station_name)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("datetime") +
    ggplot2::ylab(NULL)
  
  # ----- Return ---------------------------------------------------------------
  
  return(gg)
  
}



