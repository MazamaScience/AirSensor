# Network statistics
# Kayleigh Wilson & Jonathan Callahan

# This top portion of this script has several sections that loop through each month to complete 
# various tasks including gathering all pat objects into a monthly list, calculating
# an SoH object for each of those pat objects, calculating SoH Indices, forming lists
# and tibbles. Some of these sections can take quite a while to run so the scripts first
# check to see if the data exist locally and will load/save data accordingly.
# The bottom portion of this script deals with plotting the data

# TODO:  Add roxygen2 docs

createLocalArchive <- function(
  year = 2019,
  stateCode = "WA",
  pattern = "^MV Clean Air Ambassador @ B",
  collection = "MVCAA_B",
  baseDir = path.expand("~/Data/PA_networks"),
  verbose = TRUE
) {

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
  
  
  # ----- Get PAS data for Jan 31, 2020 ----------------------------------------
  
  # The PAS object contains all of the metadata associated with each sensor.

  filePath <- file.path(localArchiveDir, "pas_20200131.rda")
  
  if ( file.exists(filePath) ) {
    
    logger.trace("loading %s", filePath)
    pas <- get(load(filePath)) # GLOBAL
    
  } else {
    
    # Guarantee we look for data on the web
    removeArchiveBaseDir()
    setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
    
    pas <- 
      pas_load(datestamp = "20200131", archival = TRUE)
    
    # Save it
    save(list = "pas", file = filePath)
    
  } # END create data
  
  assign("pas", pas, env = .GlobalEnv)
  
  
  # ----- Get PAT data for Jan -------------------------------------------------
  
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
      pas %>%
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
      
    } # END id loop
    
    logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))
    
    save(list = "patList", file = filePath)
    
  } # END create data
  
  objectName <- sprintf("patList_%s", month.abb[i])
  assign(objectName, patList, envir = .GlobalEnv)
  
  # Only use devices found in January
  deviceDeploymentIDs <- names(patList)
  
  idString <- paste0(deviceDeploymentIDs, collapse = ", ")
  logger.trace("January ids with data:\n%s", idString)
  
  # ----- Get PAT data for Feb-Dec ---------------------------------------------
  
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
        
      } # # END id loop
      
      logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))
      
      save(patList, file = filePath)
      
    } # END create data
    
    objectName <- sprintf("patList_%s", month.abb[i])
    assign(objectName, patList, envir = .GlobalEnv)
    
  } # END month loop
  
  # ----- Get SoH data for Jan-Dec ---------------------------------------------
  
  # The per-sensor State-of-Health dataframe (aka SoH) contains a bunch of daily 
  # metrics.  
  #
  # We will combine all of the SoH dataframes for a single month into a
  # list object and then save the monthly collections of SoHs.

  for ( i in 1:12 ) {
    
    logger.trace("----- Working on SoH for %s -----", month.name[i])
    
    datestamp <- datestamps[i]
    
    # NOTE:  Assume that SoH, SoHMeans and SoHIndex are always created/found together
    fileName <- sprintf("SoH_%s_%s.rda", collection, datestamp)
    filePath <- file.path(localArchiveDir, fileName)
    
    index_fileName <- sprintf("SoHIndex_%s_%s.rda", collection, datestamp)
    index_filePath <- file.path(localArchiveDir, fileName)
    
    means_fileName <- sprintf("SoHMeans_%s_%s.rda", collection, datestamp)
    means_filePath <- file.path(localArchiveDir, fileName)
    
    if ( file.exists(filePath) ) {
      
      logger.trace("loading %s", filePath)
      SoH <- get(load(filePath))

      logger.trace("loading %s", index_filePath)
      SoHIndex <- get(load(index_filePath))

      logger.trace("loading %s", means_filePath)
      SoHMeans <- get(load(means_filePath))
      
    } else {
      
      objectName <- sprintf("patList_%s", month.abb[i])
      patList <- get(objectName)
      
      SoHList <- list()
      SoHMeansList <- list()
      SoHIndexList <- list()
      
      count <- 0
      for ( id in names(patList) ) {
        
        count <- count + 1
        logger.trace("%04d/%d -- pat_dailySoH(%s)", count, length(patList), id)
        
        SoHList[[id]] <- 
          pat_dailySoH(patList[[id]]) %>%
          dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
        
        SoHIndexList[[id]] <- PurpleAirSoH_dailyToIndex_00(SoHList[[id]])
        
        SoHList[[id]]$deviceDeploymentID <- id
        SoHIndexList[[id]]$deviceDeploymentID <- id
        
        means <-
          SoHList[[id]] %>%
          dplyr::select_if(is.numeric) %>%
          colMeans() %>%
          as.list() %>%
          dplyr::as_tibble()
        
        means$datetime <- MazamaCoreUtils::parseDatetime(datestamp, timezone = "UTC")
        means$deviceDeploymentID <- id
        
        SoHMeansList[[id]] <- means
        
      } # END id loop
      
      # * Combined SoH -----
      SoH <- dplyr::bind_rows(SoHList)
      save(SoH, file = filePath)
      
      # * Combined SoHMean -----
      SoHMeans <- dplyr::bind_rows(SoHMeansList)
      save(SoHMeans, file = means_filePath)
      
      # * Combined SoHIndex -----
      SoHIndex <- dplyr::bind_rows(SoHIndexList)
      save(SoHIndex, file = index_filePath)
      
      # Cleanup
      
    } # END create data
    
    objectName <- sprintf("SoH_%s", month.abb[i])
    assign(objectName, SoH, envir = .GlobalEnv)
    
    objectName <- sprintf("SoHMeans_%s", month.abb[i])
    assign(objectName, SoHMeans, envir = .GlobalEnv)
    
    objectName <- sprintf("SoHIndex_%s", month.abb[i])
    assign(objectName, SoHIndex, envir = .GlobalEnv)
    
  } # END month loop
  
  ###rm(means, patList, SoH, SoHList, SoHMeans, SoHMeansList, SoHIndex, SoHIndexList)
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
  
  library(dplyr)
  library(reshape2)
  library(MazamaCoreUtils)
  library(AirSensor)
  
  year = 2019
  stateCode = "WA"
  pattern = "^MV Clean Air Ambassador @ B"
  collection = "MVCAA_B"
  baseDir = path.expand("~/Data/PA_networks")
  verbose = TRUE
  
  createLocalArchive(
    year = year,
    stateCode = stateCode,
    pattern = pattern,
    collection = collection,
    baseDir = baseDir,
    verbose = verbose
  )  
  
}