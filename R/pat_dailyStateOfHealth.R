#' @export
#' @importFrom rlang .data
#' @importFrom dplyr contains
#' 
#' @title Daily state of health
#' 
#' @param pat PurpleAir Timeseries \emph{pat} object.
#' @param FUNs Vector of function names. All the passed in functions must output
#' tibbles with the same number of rows
#' 
#' @description This function combines the output of the State of Health (SoH) 
#' function arguments into a single tibble. The output tibble should have 
#' 
#' @examples  
#' SoH <- 
#'   example_pat_failure_B %>%
#'   pat_dailyStateOfHealth() 
#' head(SoH)
#' #timeseriesTbl_multiplot(tbl, ylim = c(0,100))

pat_dailyStateOfHealth <- function(
  pat = NULL,
  FUNs = c("SoH_dailyPctDC", "SoH_dailyPctReporting", "SoH_dailyPctValid", "SoH_dailyCorrelation")
) {
  
  # ----- Validate parameters --------------------------------------------------
  
  MazamaCoreUtils::stopIfNull(pat)
  
  if ( !pat_isPat(pat) )
    stop("Parameter 'pat' is not a valid 'pa_timeseries' object.")
  
  if ( pat_isEmpty(pat) )
    stop("Parameter 'pat' has no data.") 
  
  
  # ----- pat_dailyStateOfHealth() ---------------------------------------------------

  # Initialize a list to store the output of each function
  SoH <- list()
  
  for (f in FUNs) {
    
    # Isolate each passed in function
    FUN <- get(f)
    
    # Run the pat through each function and store it in the list
    SoH[[f]] <- FUN(pat)
    
  }
  
  # bind the all columns from the list into one dataframe with one datetime column
  SoH_tbl <- dplyr::bind_cols(SoH) %>%
    dplyr::select(unique("datetime"), contains("pm25"), contains("temperature"), contains("humidity"))
 
  return(SoH_tbl)
}












