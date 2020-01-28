library(MazamaCoreUtils)
library(PWFSLSmoke)
library(AirSensor)
library(reshape2)
library(dplyr)

####### Load then filter the pas which contains the meta for all the sensors

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")

logger.setup()
logger.setLevel(TRACE)

state_pas <- 
  pas_load(archival = TRUE) %>% 
  pas_filter(stateCode == "CA") #filter by CA

deviceDeploymentIDs <- pas_getDeviceDeploymentIDs(state_pas) # grab unique space/time sensor identification


#### NOTE: From here forward, each section will look for the created dataset to see if it is saved,
#### NOTE: if not, it will create the dataset. California is a big state, each section takes a while (5-25 min) to run.


####### ------------ Load the pats in a list. Each pat is labeled with with device deployment 
####### ------------ ID and contains time series and meta

archiveBaseDir <- path.expand("~/Data/CA_example_Roger") #check to see if the data directory exists, make it if not
if ( !dir.exists(archiveBaseDir) ) {
  dir.create(archiveBaseDir)
}
setArchiveBaseDir(archiveBaseDir)

patList_filePath <- file.path(archiveBaseDir, "CA_201908.rda")

if ( file.exists(patList_filePath) ) { #if the patlist already exists, load it
  patList <- get(load(patList_filePath))
}

if ( !file.exists(patList_filePath) ) {
  patList <- list()
  patEmptyList <- list()
  DATESTAMP <- "201908"
  
  count <- 0
  
  for ( id in deviceDeploymentIDs ) {
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(deviceDeploymentIDs), id)
    
    # Load January data and trim the date so we don't statistics for partial days
    
    result <- try({
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
    }, silent = FALSE)
    
    if ("try-error" %in% class(result)) {
      # place holder to handle error
    }
  }
  
  logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))
  
  filename <- c("CA_201908.rda")
  filepath <- file.path(archiveBaseDir, filename)
  save(list = "patList", file = filepath) 
  
}

####### ------------ subset to a single week for all the pats -----------------

week_patList_filePath <- file.path(archiveBaseDir, "CA_201908_week.rda")

if ( file.exists(week_patList_filePath) ) { #if the patlist already exists, load it
  week_patList <- get(load(week_patList_filePath))
}


if ( !file.exists(week_patList_filePath) ) {
  week_patList <- list()
  count <- 0
  for (id in names(patList) ) {
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_filterDate(%s)", count, length(patList), id)
    
    result <- try({
      week_patList[[id]] <- pat_filterDate(patList[[id]], startdate = 20190811, weeks = 1, timezone = "America/Los_Angeles")
      week_patList[[id]]$deviceDeploymentID <- id
    }, silent = FALSE)
    
    if ("try-error" %in% class(result)) {
      # place holder to handle error
    }
    filename <- c("CA_201908_week.rda")
    filepath <- file.path(archiveBaseDir, filename)
    save(list = "week_patList", file = filepath) 
  }
}

####### ------------ Calculate SoH index --------------------------------------

SoHIndexList_filePath <- file.path(archiveBaseDir, "CA_201908_week_SoH.rda")

if ( file.exists(SoHIndexList_filePath) ) { #if the patlist already exists, load it
  sohindex_list <- get(load(SoHIndexList_filePath))
}

if ( !file.exists(SoHIndexList_filePath) ) {
  
  sohindex_list <- list()
  count <- 0
  
  for (id in names(week_patList)){
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_dailySoHIndex_00(%s)", count, length(week_patList), id)
    result <- try({
      sohindex_list[[id]] <- pat_dailySoHIndex_00(week_patList[[id]])
    }, silent = FALSE)
    
    if ("try-error" %in% class(result)) {
      # place holder to handle error
    }
    filename <- c("CA_201908_week_SoH.rda")
    filepath <- file.path(archiveBaseDir, filename)
    save(list = "sohindex_list", file = filepath) 
  }
}

####### ------------ filter for sensors that were "good" all week --------------

goodPatList_filePath <- file.path(archiveBaseDir, "CA_201908_week_good.rda")

if ( file.exists(goodPatList_filePath) ) { #if the patlist already exists, load it
  good_patlist <- get(load(goodPatList_filePath))
}

if ( !file.exists(goodPatList_filePath) ) {
  
  
  sohindex_list_ids <- list()
  count <- 0
  
  # add device deployment ID to the individual tibbles
  for (id in names(sohindex_list)){
    
    count <- count + 1
    logger.trace("%04d/%d -- mutate(%s)", count, length(sohindex_list), id)
    result <- try({
      sohindex_list_ids[[id]] <- 
        dplyr::mutate(sohindex_list[[id]], deviceDeploymentID = id)
      
    }, silent = FALSE)
    
    if ("try-error" %in% class(result)) {
      # place holder to handle error
    }
  }
  # Make one giant tibble for filtering
  SoH_tidy <- 
    dplyr::bind_rows(sohindex_list_ids) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
  
  # Pull the ids of the sensors that were "good" all week 
  SoH_good_id <- 
    SoH_tidy %>%
    dplyr::group_by(.data$deviceDeploymentID) %>%
    dplyr::mutate(indexbin_sum = 
                    case_when(identical(.data$deviceDeploymentID, .data$deviceDeploymentID) ~ sum(.data$index_bin))) %>%
    dplyr::filter(.data$indexbin_sum == 21) %>%
    dplyr::pull(.data$deviceDeploymentID) %>%
    unique(.data$deviceDeploymentID)
  
  # Grab the pat's for Ids that were "good" all week   
  
  good_patlist <- list()
  count <- 0
  
  for (id in SoH_good_id){
    
    count <- count + 1
    logger.trace("%04d/%d -- copy(%s)", count, length(SoH_good_id), id)
    result <- try({
      good_patlist[[id]] <-  week_patList[[id]]
      
    }, silent = FALSE)
    
    if ("try-error" %in% class(result)) {
      # place holder to handle error
    }
    
  }
  filename <- c("CA_201908_week_good.rda")
  filepath <- file.path(archiveBaseDir, filename)
  save(list = "good_patlist", file = filepath) 
}

####### ------------ convert to an hourly aggregated “airsensor” object  -------

airsensorList_filePath <- file.path(archiveBaseDir, "CA_201908_week_good_airsensor.rda")

if ( file.exists(airsensorList_filePath) ) { #if the patlist already exists, load it
  airsensor_list <- get(load(airsensorList_filePath))
}

if ( !file.exists(airsensorList_filePath) ) {
  
  airsensor_list <- list()
  count <- 0
  
  for (id in names(good_patlist)){
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_createAirSensor(%s)", count, length(SoH_good_id), id)
    result <- try({
      airsensor_list[[id]] <-  
        pat_createAirSensor(
          pat = good_patlist[[id]],
          period = "1 hour",
          parameter = "pm25",
          channel = "a",
          qc_algorithm = "hourly_AB_01",
          min_count = 20,
          aggregation_FUN = pat_aggregate
        )
      
    }, silent = FALSE)
    
    if ("try-error" %in% class(result)) {
      # place holder to handle error
    }
  }
  filename <- c("CA_201908_week_good_airsensor.rda")
  filepath <- file.path(archiveBaseDir, filename)
  save(list = "airsensor_list", file = filepath) 
}

####### ------------ get daily mean  ------------------------------------------

# Combine into a single monitor object
monitor_singleList <- PWFSLSmoke::monitor_combine(airsensor_list)

# calculate the daily average for each day and each sensor
monitor_singleList_dailyAvg <-
  monitor_dailyStatisticList(
    monitor_singleList,
    FUN = get("mean"),
    dayStart = "midnight",
    na.rm = TRUE,
    minHours = 18
  )

daily_averages_data <- monitor_singleList_dailyAvg$`America/Los_Angeles`$data
daily_averages_meta <- monitor_singleList_dailyAvg$`America/Los_Angeles`$meta
dates <- daily_averages_data$datetime


daily_averages_data_transpose <- as.data.frame(t(daily_averages_data[,-1]))
colnames(daily_averages_data_transpose) <- paste0(dates, "_pm25_mean")

dailyMeans_goodCA <- 
  daily_averages_meta %>%
  dplyr::bind_cols(daily_averages_data_transpose)

filename <- c("CA_201908_week_good_dailyMeans.csv")
filepath <- file.path(archiveBaseDir, filename)
write.csv(dailyMeans_goodCA, file = filepath)
