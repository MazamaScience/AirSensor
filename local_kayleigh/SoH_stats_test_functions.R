##################### --------------------------------------------------------

# NOTE: Load the pats for one month and store them all in a list. 

# DATESTAMP format = 201901
# deviceDeploymentIDs need to be the same sensors from January for every month

loadMonthlyPats <- function(
  DATESTAMP = NULL,
  deviceDeploymentIDs = jan_deviceDeploymendIDs
) {
  
  patList <- list()
  patEmptyList <- list()
  
  count <- 0
  for ( id in deviceDeploymentIDs ) {
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(deviceDeploymentIDs), id)
    
    # Load January data and trim the date so we don't statistics for partial days
    pat <- 
      pat_loadMonth(id, datestamp = DATESTAMP) 
    
    # Can only trimDate if it isn't empty
    if ( !pat_isEmpty(pat) )
      pat <- pat_trimDate(pat)
    
    if ( pat_isEmpty(pat) ) {
      # TODO fix pat_isEmpty so it checks pat$data, not pat$meta
      patEmptyList[[id]] <- pat
    } else {
      patList[[id]] <- pat
    }
    
  }
  
  logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))
  
  return(patList)
}

################################ ---------------------------------------------

# Create State-of-Health metrics list where we have data for a whole month

createSoHList <- function(
  patList = NULL
) {
  
  SoHList <- list()
  
  count <- 0
  for ( id in names(patList) ) {
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_dailySoH(%s)", count, length(patList), id)
    
    SoHList[[id]] <- pat_dailySoH(patList[[id]])
    SoHList[[id]]$deviceDeploymentID <- id
    
  }
  
  return(SoHList)
}

########################################### ---------------------------------

# build one big monthly SOH tibble 

createSingleSoHTbl <- function(
  SoHList = NULL
) {
  
  SoH <- 
    dplyr::bind_rows(SoHList) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
  
  return(SoH)
}

########################################### -----------------------------------

# Create a dataframe of monthly means for each sensor

createMonthlyMeans <- function(
  DATESTAMP = NULL,
  SoHList = NULL
) {
  
  datetime <- MazamaCoreUtils::parseDatetime(DATESTAMP, timezone = "UTC")
  SoHMeansList <- list()
  
  for ( id in names(SoHList) ) {
    
    means <-
      SoHList[[id]] %>%
      dplyr::select_if(is.numeric) %>%
      colMeans() %>%
      as.list() %>%
      dplyr::as_tibble()
    
    means$datetime <- datetime
    means$id <- id
    
    SoHMeansList[[id]] <- means
    
  }
  
  SoHMeans <- dplyr::bind_rows(SoHMeansList)
  
  return(SoHMeans)
}

##################################### ----------------------------------------

# tidy means without datetime 

createTidyMeans <- function(
  SoHMeans = NULL
) {
  
  library(reshape2)
  tidyTbl <- 
    SoHMeans %>%
    dplyr::select(-datetime) %>%
    melt(id.vars='id')
  
  return(tidyTbl)
}

##################################### ----------------------------------------

# Calculate the daily SoH INDEX for each day in the month 
monthlySOH_index <- function(
  patList = NULL
) {
  
  indexList <- list()
  
  count <- 0
  for ( id in names(patList) ) {
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_dailySoHIndex(%s)", count, length(patList), id)
    
    indexList[[id]] <- pat_dailySoHIndex_00(patList[[id]])
    indexList[[id]]$deviceDeploymentID <- id
    
  }
  
  return(indexList)
  
}

##################################### ----------------------------------------


# build one big monthly SOH INDEX tibble 

createSingleSoH_Index_Tbl <- function(
  indexList = NULL
) {
  
  index <- 
    dplyr::bind_rows(indexList) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
  
  return(index)
}












