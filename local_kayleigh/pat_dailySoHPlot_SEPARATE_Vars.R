#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains 
#' 
#' @title Daily State of Health metric plot
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' 
#' @description This function plots as subset of the most useful State of Health 
#' metrics calculated by the \code{pat_dailySoH} function. The function 
#' runs \code{pat_dailySoH} internally and uses the output to create 
#' the plot.
#' 
#' 


pat_dailySoHPlot_SEPARATE_Vars <- function(
  pat = NULL
) {
  
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  # ----- Create the SoH object, and SoH plot ----------------------------------
  
  # calculate the SoH 
  SoH <- pat_dailySoH(pat)
  
  # select only the useful metrics of interest from the full SoH
  SoH_sub <- dplyr::select(SoH, "datetime", 
                           "pm25_A_pctReporting",
                           "pm25_B_pctReporting",
                           "pm25_A_pctValid",
                           "pm25_B_pctValid",
                           "pm25_A_pctDC", 
                           "pm25_B_pctDC",
                           "pm25_A_pm25_B_slope",
                           "pm25_A_pm25_B_intercept",
                           "pm25_A_pm25_B_rsquared",
                           #"pm25_A_pm25_B_p_value",
                           "pm25_A_temperature_rsquared") 
  SoH_pvalue <- dplyr::select(SoH, "datetime", 
                              "pm25_A_pm25_B_p_value")
  
  # copy the datetime column from the SoH to use later when creating the dummy 
  # data.
  datetime <- SoH_sub$datetime
  
  # create a tidy dataframe from the SoH
  SoH_tidy <-
    SoH_sub %>%
    tidyr::gather(key ="variable", value = "value", -datetime) %>%
    # create a factor based on the variable name for expected value association
    dplyr::mutate(expectedValue = as.integer(factor(.data$variable))) 
  
  SoH_pvalue_tidy <-
    SoH_pvalue %>%
    tidyr::gather(key ="variable", value = "value", -datetime) %>%
    # create a factor based on the variable name for expected value association
    dplyr::mutate(expectedValue = as.integer(factor(.data$variable))) 
  
  # assign associated expected values based on the original column
  SoH_tidy <- 
    SoH_tidy %>%
    dplyr::mutate(expectedValue = case_when(
      grepl("_pctReporting", SoH_tidy$variable) ~ 100,
      grepl("_pctValid", SoH_tidy$variable) ~ 100,
      grepl("_pctDC", SoH_tidy$variable) ~ 0,
      grepl("pm25_A_pm25_B_slope", SoH_tidy$variable) ~ 1,
      grepl("pm25_A_pm25_B_intercept", SoH_tidy$variable) ~ 0,
      grepl("pm25_A_pm25_B_rsquared", SoH_tidy$variable) ~ 1,
      #grepl("pm25_A_pm25_B_p_value", SoH_tidy$variable) ~ 1, 
      grepl("pm25_A_temperature_rsquared", SoH_tidy$variable) ~ 0))
     
  SoH_pvalue_tidy <- 
    SoH_pvalue_tidy %>%
    dplyr::mutate(expectedValue = case_when(
      grepl("pm25_A_pm25_B_p_value", SoH_pvalue_tidy$variable) ~ 1))
  
  # create factor for ordering the facets later on
  SoH_tidy$variable <- factor(SoH_tidy$variable, levels=c(
    "pm25_A_pctReporting",
    "pm25_B_pctReporting",
    "pm25_A_pctValid",
    "pm25_B_pctValid",
    "pm25_A_pctDC",
    "pm25_B_pctDC",
    "pm25_A_pm25_B_slope",
    "pm25_A_pm25_B_intercept",
    "pm25_A_pm25_B_rsquared",
    #"pm25_A_pm25_B_p_value",
    "pm25_A_temperature_rsquared"
  ))
  
  # create the dummy variables which contain just the min and max expected 
  # values for each variable in order to set an appropriate range in the facets
  pm25_A_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
  pm25_B_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
  pm25_A_pctValid <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_B_pctValid <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_A_pctDC <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_B_pctDC <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
  pm25_A_pm25_B_slope <- rep_len(c(-5, 5), length.out = length(SoH_sub$datetime))
  pm25_A_pm25_B_intercept <- rep_len(c(-50, 50), length.out = length(SoH_sub$datetime))
  pm25_A_pm25_B_rsquared <- rep_len(c(0, 1), length.out = length(SoH_sub$datetime))
  #pm25_A_pm25_B_p_value <- rep_len(c(0, 1), length.out = length(SoH_sub$datetime))
  pm25_A_temperature_rsquared <- rep_len(c(0, 1), length.out = length(SoH_sub$datetime))
 
  
  # add all the dummy variables to the dummy dataframe
  dummy <- data.frame(datetime,
                      pm25_A_pctReporting, 
                      pm25_B_pctReporting,
                      pm25_A_pctValid,
                      pm25_B_pctValid,
                      pm25_A_pctDC,
                      pm25_B_pctDC,
                      pm25_A_pm25_B_slope,
                      pm25_A_pm25_B_intercept,
                      pm25_A_pm25_B_rsquared,
                      #pm25_A_pm25_B_p_value,
                      pm25_A_temperature_rsquared)
  
  # tidy the dummy data to mimic the real data
  dummy_tidy <-
    dummy %>%
    tidyr::gather(key = "variable", value = "value", -.data$datetime) %>%
    dplyr::mutate(expectedValue = as.integer(factor(.data$variable))) 
  
  colors <- c("salmon")
  
  # pull out the station name for labeling the plot
  station_name <- pat$meta$label
  
  gg1 <- ggplot(SoH_tidy, aes(.data$datetime, .data$value)) +
    # plot the flat-lined, expected values
    geom_line(aes(x = SoH_tidy$datetime, y = SoH_tidy$expectedValue),  
              color = colors, alpha = 0.8) +
    # plot the dummy data to establish a uniform range from station to station
    geom_line(aes(x = dummy_tidy$datetime, y = dummy_tidy$value), 
              color = "black", alpha = 0) +
    # plot the actual SoH data as outlined in the initial aes()
    geom_line() +
    scale_y_continuous(breaks=scales::pretty_breaks(3)) +
    facet_wrap(vars(SoH_tidy$variable), ncol = 1, strip.position = c("top"), scales = "free_y") +
    labs(title = paste0("State of Health - ", station_name)) +
    ylab(NULL) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(legend.position = "none")
  
  #breaks=seq(1e-40, 1, length.out = 3)
  #breaks = scales::trans_breaks("log10", function(x) 10^x)
  #breaks = scales::trans_breaks("log10",function(x) 10^x, n =  3)
  #breaks = c(min(SoH_pvalue_tidy$value, na.rm = TRUE), max(SoH_pvalue_tidy$value, na.rm = TRUE), 1)
  #breaks = scales::trans_breaks("log10", function(x) min(x, na.rm = TRUE))
  
  gg2 <- ggplot(SoH_pvalue_tidy, aes(.data$datetime, .data$value)) +
    geom_line(aes(x = SoH_pvalue_tidy$datetime, y = SoH_pvalue_tidy$expectedValue),  
              color = colors, alpha = 0.8) +
    geom_line() +
    #scale_y_continuous(trans = "log10", breaks=c(1e-80, 1e-40, 1e-20, 1), lim = c(1e-80, 1)) +
    #scale_y_continuous(trans = "log10", breaks=breaks) +
    scale_y_continuous(trans = "log10", breaks = as.numeric(waiver())) +
    #scale_y_log10(breaks = NULL) +
    facet_wrap(vars(SoH_pvalue_tidy$variable), ncol = 1, strip.position = c("top"), scales = "free_y") +
    #theme(plot.margin = unit(c(0,0.2,0.2,0.4), "cm")) +
    ylab(NULL) +
    xlab("date")
  
  grid.newpage()
  gg <- grid.draw(rbind(ggplotGrob(gg1), ggplotGrob(gg2), size = "last"))
  
  
# ----- Return -------------------------------------------------------------
  return(gg)
  
}



