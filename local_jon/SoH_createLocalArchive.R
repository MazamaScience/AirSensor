# Network statistics
# Kayleigh Wilson & Jonathan Callahan

# This top portion of this script has several sections that loop through each month to complete 
# various tasks including gathering all pat objects into a monthly list, calculating
# an SoH object for each of those pat objects, calculating SoH Indices, forming lists
# and tibbles. Some of these sections can take quite a while to run so the scripts first
# check to see if the data exist locally and will load/save data accordingly.
# The bottom portion of this script deals with plotting the data

# TODO:  Add roxygen2 docs

# createLocalArchive <- function(
#   year = 2019,
#   stateCode = "WA",
#   pattern = "^MV Clean Air Ambassador @ B",
#   collection = "MVCAA_B",
#   baseDir = path.expand("~/Data/PA_networks"),
#   verbose = TRUE
# ) {

  year = 2019
  stateCode = "WA"
  pattern = "^MV Clean Air Ambassador @ B"
  collection = "MVCAA_B"
  baseDir = path.expand("~/Data/PA_networks")
  verbose = TRUE

  # ----- Setup ----------------------------------------------------------------
  
  if ( verbose ) {
    # Set up logging
    logger.setup()
    logger.setLevel(TRACE)
  }
  
  localArchiveDir <- file.path(baseDir, collection)
  
  if ( !dir.exists(localArchiveDir) )
    dir.create(localArchiveDir)
  
  # Month identifiers
  datestamps <- (year * 100) + 1:12
  
  
  # ----- Get the PAS data for Jan 31, 2020 ------------------------------------
  
  # The PAS object contains all of the metadata associated with each sensor.

  filePath <- file.path(localArchiveDir, "pas_20200131.rda")
  
  if ( file.exists(filePath) ) {
    
    logger.trace("loading %s", filePath)
    pas_20200131 <- get(load(filePath))
    
  } else {
    
    # Guarantee we look for data on the web
    removeArchiveBaseDir()
    setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
    
    pas_20200131 <- 
      pas_load(datestamp = "20200131", archival = TRUE)
    
    # Save it
    save(list = "pas_20200131", file = filePath)
    
  }
  
  
  # ----- Get all PATs for January ---------------------------------------------
  
  # Each PAT contains all of the timeseries data for a single sensor. 
  #
  # We will combine all of the sensors for a single month into a 
  # list object and then save the monthly collections of PATs.
  
  # BUT FIRST -- We want to make sure that the sensors we look at had data
  #              in January. We use this month to create our collection of
  #              deviceDeploymentIDs
  
  i <- 1
  
  logger.trace("----- Working on patList for %s -----", month.name[i])
  
  datestamp <- datestamps[i]
  
  patList <- list()
  patEmptyList <- list()
  
  fileName <- sprintf("patList_%s_%s.rda", collection, datestamp)
  filePath <- file.path(localArchiveDir, fileName)
  
  if ( file.exists(filePath) ) {
    
    logger.trace("loading %s", filePath)
    patList <- get(load(filePath))
    
  } else {
    
    # Guarantee we look for data on the web
    removeArchiveBaseDir()
    setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
    
    deviceDeploymentIDs <- 
      pas_20200131 %>%
      pas_filter(stateCode == stateCode) %>%
      pas_getDeviceDeploymentIDs(pattern = pattern)
    
    count <- 0
    for ( id in deviceDeploymentIDs ) {
      
      count <- count + 1
      logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(deviceDeploymentIDs), id)
      
      # Load January data and trim the date so we don't statistics for partial days
      result <- try({
        pat <-
          pat_loadMonth(id, datestamp = datestamp)
        
        # Can only trimDate if it isn't empty
        if ( !pat_isEmpty(pat) )
          pat <- pat_trimDate(pat)
        
        # Check again to see if it is empty now
        if ( pat_isEmpty(pat) ) {
          patEmptyList[[id]] <- pat
        } else {
          patList[[id]] <- pat
        }
      }, silent = FALSE)
      
      if ( "try-error" %in% class(result) ) {
        patList[[id]] <- pat
      }
      
    }
    
    logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))
    
    save(list = "patList", file = filePath)
    
  }
  
  objectName <- sprintf("patList_%s", month.abb[i])
  assign(objectName, patList)
  
  # Only use devices found in January
  deviceDeploymentIDs <- names(patList)
  
  idString <- paste0(deviceDeploymentIDs, collapse = ", ")
  logger.trace("January ids with data:\n%s", idString)
  
  # ----- Get all PATs for Feb-Dec ---------------------------------------------
  
  # Each PAT contains all of the timeseries data for a single sensor. 
  #
  # We will combine all of the sensors for a single month into a 
  # list object and then save the monthly collections of PATs.
  
  for ( i in 2:12 ) {
    
    logger.trace("----- Working on patList for %s -----", month.name[i])
    
    datestamp <- datestamps[i]
    
    patList <- list()
    patEmptyList <- list()
    
    fileName <- sprintf("patList_%s_%s.rda", collection, datestamp)
    filePath <- file.path(localArchiveDir, fileName)
    
    if ( file.exists(filePath) ) {
      
      logger.trace("loading %s", filePath)
      patList <- get(load(filePath))
      
    } else {
      
      # Guarantee we look for data on the web
      removeArchiveBaseDir()
      setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
      
      count <- 0
      for ( id in deviceDeploymentIDs ) {
        
        count <- count + 1
        logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(deviceDeploymentIDs), id)
        
        # Load January data and trim the date so we don't statistics for partial days
        result <- try({
          pat <-
            pat_loadMonth(id, datestamp = datestamp)
          
          # Can only trimDate if it isn't empty
          if ( !pat_isEmpty(pat) )
            pat <- pat_trimDate(pat)
          
          # Check again to see if it is empty now
          if ( pat_isEmpty(pat) ) {
            patEmptyList[[id]] <- pat
          } else {
            patList[[id]] <- pat
          }
        }, silent = FALSE)
        
        if ( "try-error" %in% class(result) ) {
          patList[[id]] <- pat
        }
        
      }
      
      logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))
      
      save(list = "patList", file = filePath)
      
    }
    
    objectName <- sprintf("patList_%s", month.abb[i])
    assign(objectName, patList)
    
  }
  
  # ----- Get all SoH data for Jan-Dec -----------------------------------------
  
  # The per-sensor State-of-Health dataframe (aka SoH) contains a bunch of daily 
  # metrics.  
  #
  # We will combine all of the SoH dataframes for a single month into a
  # list object and then save the monthly collections of SoHs.

  for ( i in 1:12 ) {
    
    logger.trace("----- Working on SoHList for %s -----", month.name[i])
    
    datestamp <- datestamps[i]
    
    SoHList <- list()
    
    fileName <- sprintf("SoHList_%s_%s.rda", collection, datestamp)
    filePath <- file.path(localArchiveDir, fileName)
    
    if ( file.exists(filePath) ) {
      
      logger.trace("loading %s", filePath)
      SoHList <- get(load(filePath))
      
    } else {
      
      objectName <- sprintf("patList_%s", month.abb[i])
      patList <- eval(as.symbol(objectName))
      
      count <- 0
      for ( id in names(patList) ) {
        
        count <- count + 1
        logger.trace("%04d/%d -- pat_dailySoH(%s)", count, length(patList), id)
        
        SoHList[[id]] <- pat_dailySoH(patList[[id]])
        SoHList[[id]]$deviceDeploymentIDs <- id
        
        objectName <- sprintf("SoHList_%s", month.abb[i])
        assign(objectName, SoHList)
      }
      
      save(list = objectName, file = filePath)
      
    }
    
  }
  
  # ----- Get all SoHIndex data for Jan-Dec ------------------------------------
  
  # The per-sensor State-of-Health Index dataframe (aka SoH) contains a single 
  # daily state-of-health metric between 0:1. 
  #
  # We will combine all of the SoHIndex dataframes for a single month into a 
  # list object and then save the monthly collections of SoHIndexs.
  
  for ( i in 1:12 ) {
    
    logger.trace("----- Working on SoHIndexList for %s -----", month.name[i])
    
    datestamp <- datestamps[i]
    
    SoHIndexList <- list()
    
    fileName <- sprintf("SoHIndexList_%s_%s.rda", collection, datestamp)
    filePath <- file.path(localArchiveDir, fileName)
    
    if ( file.exists(filePath) ) {
      
      logger.trace("loading %s", filePath)
      SoHList <- get(load(filePath))
      
    } else {
      
      objectName <- sprintf("patList_%s", month.abb[i])
      patList <- eval(as.symbol(objectName))
      
      count <- 0
      for ( id in names(patList) ) {
        
        count <- count + 1
        logger.trace("%04d/%d -- pat_dailySoHIndex_00(%s)", count, length(patList), id)
        
        SoHIndexList[[id]] <- pat_dailySoHIndex_00(patList[[id]])
        SoHIndexList[[id]]$deviceDeploymentIDs <- id
        
        objectName <- sprintf("SoHIndexList_%s", month.abb[i])
        assign(objectName, SoHIndexList)
        
      }
      
      save(list = objectName, file = filePath)
      
    }
    
  }
  
  # TODO:  Should the steps below be part of the steps above?
  
  # ----- Create monthly SoH objects -------------------------------------------
  
  for ( i in 1:12 ) {
    
    logger.trace("----- Working on SoH for %s -----", month.name[i])
    
    SoHList <- get(sprintf("SoHList_%s", month.abb[i]))
    
    SoH <-
      dplyr::bind_rows(SoHList) %>%
      dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
    
    objectName <- sprintf("SoH_%s", month.abb[i])
    assign(objectName, SoH)
    
  }
  
  # ----- Create SoH means -----------------------------------------------------
  
  for ( i in 1:12 ) {
    
    logger.trace("----- Working on SoHMeans for %s -----", month.name[i])
    
    datetime <- MazamaCoreUtils::parseDatetime(datestamps[i], timezone = "UTC")
    
    SoHList <- get(sprintf("SoHList_%s", month.abb[i]))
    
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
    
    objectName <- sprintf("SoHMeans_%s", month.abb[i])
    assign(objectName, SoHMeans)
    
  }
  
  
  # ----- Create monthly tidySoHMeans objects ----------------------------------
  
  for ( i in 1:12 ) {
    
    logger.trace("----- Working on tidySoHMeans for %s -----", month.name[i])
    
    SoHMeans <- get(sprintf("SoHMeans_%s", month.abb[i]))
    
    tidyTbl <-
      SoHMeans %>%
      dplyr::select(-datetime) %>%
      reshape2::melt(id.vars="id")
    
    objectName <- sprintf("tidySoHMeans_%s", month.abb[i])
    assign(objectName, tidyTbl)
    
  }
  
  
  # ----- Create monthly tidySoHIndex objects ----------------------------------
  
  for ( i in 1:12 ) {
    
    logger.trace("----- Working on tidySoHIndex for %s -----", month.name[i])
    
    SoHIndexList <- get(sprintf("SoHIndexList_%s", month.abb[i]))
    
    SoHIndex <-
      dplyr::bind_rows(SoHIndexList) %>%
      dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
    
    objectName <- sprintf("SoHIndex_%s", month.abb[i])
    assign(objectName, SoHIndex)
    
  }
  
  
# }

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(dplyr)
  library(reshape2)
  library(MazamaCoreUtils)
  library(AirSensor)
  
  createLocalArchive(
    year = 2019,
    stateCode = "WA",
    pattern = "^MV Clean Air Ambassador @ B",
    collection = "MVCAA_B",
    baseDir = path.expand("~/Data/PA_networks"),
    verbose = TRUE
  )  
  
}