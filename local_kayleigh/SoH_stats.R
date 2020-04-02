# Single state statistics
# Kayleigh Wilson

# This top portion of this script has several sections that loop through each month to complete 
# various tasks including gathering all pat objects into a monthly list, calculating
# an SoH object for each of those pat objects, calculating SoH Indices, forming lists
# and tibbles. Some of these sections can take quite a while to run so the scripts first
# check to see if the data exist locally and will load/save data accordingly.
# The bottom portion of this script deals with plotting the data

library(ggplot2)
library(MazamaCoreUtils)
library(AirSensor)
library(reshape2)
library(dbplyr)
library(ggridges)
library(viridis)
library(hrbrthemes)

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")

logger.setup()
logger.setLevel(TRACE)

state_pas_UT <- 
  pas_load(archival = TRUE) %>%
  pas_filter(stateCode == "UT") 


state_pas <- 
  pas_load(archival = TRUE) %>%
  pas_filter(stateCode == "WA")  #%>% # For testing or reducing the number of sensors
#pas_filter(stringr::str_detect(label, "^MV Clean Air Ambassador @ B"))


archiveBaseDir <- path.expand("~/Data/MonthlySohStats_data") #check to see if the data directory exists, make it if not
#archiveBaseDir <- path.expand("~/Data/MonthlySohStats_dataTEST") # For testing smaller batches of data
if ( !dir.exists(archiveBaseDir) ) {
  dir.create(archiveBaseDir)
}
setArchiveBaseDir(archiveBaseDir)

# NOTE: First, the patlist for January must be created because we need the list of 
# NOTE: device deployment ID's to use for the rest of the year.

patList_filename <-  "jan_patlist.rda"
patList_filePath <- file.path(archiveBaseDir, patList_filename)
if ( file.exists(patList_filePath) ) { #if the patlist already exists, load it
  patList <- get(load(patList_filePath))
}

if ( !file.exists(patList_filePath) ) {
  
  initial_deviceDeploymentIds <- pas_getDeviceDeploymentIDs(state_pas)
  DATESTAMP <- "201901"
  jan_patlist <- list()
  jan_patEmptyList <- list()
  
  count <- 0
  for ( id in initial_deviceDeploymentIds ) {
    
    
    
    count <- count + 1
    logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(initial_deviceDeploymentIds), id)
    
    # Load January data and trim the date so we don't statistics for partial days
    pat <-
      pat_loadMonth(id, datestamp = DATESTAMP)
    
    # Can only trimDate if it isn't empty
    if ( !pat_isEmpty(pat) )
      pat <- pat_trimDate(pat)
    
    if ( pat_isEmpty(pat) ) {
      # TODO fix pat_isEmpty so it checks pat$data, not pat$meta
      jan_patEmptyList[[id]] <- pat
    } else {
      jan_patlist[[id]] <- pat
    }
  }
  
  logger.trace("%d pats with data, %d without", length(jan_patlist), length(jan_patEmptyList))
  
  patList_filename <-  "jan_patlist.rda"
  patList_filePath <- file.path(archiveBaseDir, patList_filename)
  save(list = "jan_patlist", file = patList_filePath)
}
#NOTE: set up for data prep for the remainder of the year

jan_deviceDeploymentIds <- names(jan_patlist)

month_abbrev <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov")
DATESTAMP <- c("201901", "201902", "201903", "201904", "201905", "201906", 
               "201907", "201908", "201909", "201910", "201911")

# NOTE: Check to see if the patlist for each month exists in the directory,
# NOTE: load the months that exist and create the months that do not. This chunk
# NOTE: assumes that the monthly pat data are available at the ArchiveBaseURL, 
# NOTE: then loads and adds each pat object to a list for each month

for (i in seq(month_abbrev)){
  
  logger.trace("loading for month %s", month_abbrev[i])
  patList_filename <- paste0(month_abbrev[i], "_patlist.rda")
  patList_filePath <- file.path(archiveBaseDir, patList_filename)
  
  if ( file.exists(patList_filePath) ) { #if the patlist already exists, load it
    patList <- get(load(patList_filePath))
  }
  
  if ( !file.exists(patList_filePath) ) {
    
    print(DATESTAMP[i])
    
    patList <- list()
    patEmptyList <- list()
    
    count <- 0
    for ( id in jan_deviceDeploymentIds ) {
      
      count <- count + 1
      logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(jan_deviceDeploymentIds), id)
      
      # Load January data and trim the date so we don't statistics for partial days
      pat <-
        pat_loadMonth(id, datestamp = DATESTAMP[i])
      
      # Can only trimDate if it isn't empty
      if ( !pat_isEmpty(pat) )
        pat <- pat_trimDate(pat)
      
      if ( pat_isEmpty(pat) ) {
        # TODO fix pat_isEmpty so it checks pat$data, not pat$meta
        patEmptyList[[id]] <- pat
      } else {
        patList[[id]] <- pat
      }
      newname <- paste0(month_abbrev[i], "_patlist")
      assign(newname, patList)
    }
    
    logger.trace("%d pats with data, %d without", length(patList), length(patEmptyList))
    save(list = newname, file = patList_filePath)
  }
}

# NOTE: Check to see if the sohlist for each month exists in the directory,
# NOTE: load the months that exist and create the months that do not. This chunk
# NOTE: assumes that ALL monthly pat data are ALREADY LOADED, then creates a list
# NOTE: for each month containing the SOH data for each pat object

for (i in seq(month_abbrev)){
  
  logger.trace("loading for month %s", month_abbrev[i])
  sohList_filname <- paste0(month_abbrev[i], "_sohlist.rda")
  sohList_filePath <- file.path(archiveBaseDir, sohList_filname)
  
  if ( file.exists(sohList_filePath) ) { #if the patlist already exists, load it
    sohList <- get(load(sohList_filePath))
  }
  
  jan_deviceDeploymentIds <- names(jan_patlist)
  
  if ( !file.exists(sohList_filePath) ) {
    
    patlist_ref <- paste0(month_abbrev[i], "_patlist")
    patlist <- eval(as.symbol(patlist_ref))
    
    SoHList <- list()
    
    count <- 0
    for ( id in names(patlist) ) {
      
      count <- count + 1
      logger.trace("%04d/%d -- pat_dailySoH(%s)", count, length(patlist), id)
      
      SoHList[[id]] <- pat_dailySoH(patlist[[id]])
      SoHList[[id]]$deviceDeploymentIds <- id
      newname <- paste0(month_abbrev[i], "_sohlist")
      assign(newname, SoHList)
    }
    save(list = newname, file = sohList_filePath)
  }
}

# NOTE: Check to see if the sohIndexList for each month exists in the directory,
# NOTE: load the months that exist and create the months that do not. This chunk
# NOTE: assumes that ALL monthly pat lists are ALREADY LOADED

for (i in seq(month_abbrev)){
  
  logger.trace("loading for month %s", month_abbrev[i])
  sohIndexList_filname <- paste0(month_abbrev[i], "_sohIndexList.rda")
  sohIndexList_filePath <- file.path(archiveBaseDir, sohIndexList_filname)
  
  if ( file.exists(sohIndexList_filePath) ) { #if the patlist already exists, load it
    sohIndexList <- get(load(sohIndexList_filePath))
  }
  
  jan_deviceDeploymentIds <- names(jan_patlist)
  
  if ( !file.exists(sohIndexList_filePath) ) {
    
    patlist_ref <- paste0(month_abbrev[i], "_patlist")
    patlist <- eval(as.symbol(patlist_ref))
    indexList <- list()
    
    count <- 0
    for ( id in names(patlist) ) {
      
      count <- count + 1
      logger.trace("%04d/%d -- pat_dailySoHIndex(%s)", count, length(patlist), id)
      
      indexList[[id]] <- pat_dailySoHIndex_00(patlist[[id]])
      indexList[[id]]$deviceDeploymentID <- id
      newname <- paste0(month_abbrev[i], "_sohIndexlist")
      assign(newname, indexList)
    }
    
    save(list = newname, file = sohIndexList_filePath)
  }
}
# NOTE: Check to see if the airsensor list for each month exists in the directory,
# NOTE: load the months that exist and create the months that do not. This chunk
# NOTE: assumes that ALL monthly pat lists are ALREADY LOADED

for (i in seq(month_abbrev)){
  
  logger.trace("loading for month %s", month_abbrev[i])
  airsensorList_filename <- paste0(month_abbrev[i], "_airsensorList.rda")
  airsensorList_filePath <- file.path(archiveBaseDir, airsensorList_filename)
  
  if ( file.exists(airsensorList_filePath) ) { #if the patlist already exists, load it
    airsensorList <- get(load(airsensorList_filePath))
  }
  
  if ( !file.exists(airsensorList_filePath) ) {
    
    patlist_ref <- paste0(month_abbrev[i], "_patlist")
    patlist <- eval(as.symbol(patlist_ref))
    
    airsensorList <- list()
    count <- 0
    
    for (id in names(patlist)){
      
      count <- count + 1
      logger.trace("%04d/%d -- pat_createAirSensor(%s)", count, length(patlist), id)
      result <- try({
        airsensorList[[id]] <-  
          pat_createAirSensor(
            pat = patlist[[id]],
            period = "1 hour",
            parameter = "pm25",
            channel = "ab",
            qc_algorithm = "hourly_AB_01",
            min_count = 20,
            aggregation_FUN = pat_aggregate
          )
        
      }, silent = FALSE)
      
      if ("try-error" %in% class(result)) {
        # place holder to handle error
      }
    }
    newname <- paste0(month_abbrev[i], "_airsensorlist")
    assign(newname, airsensorList)
    save(list = newname, file = airsensorList_filePath)
    
  }
}
# NOTE: Check to see if the pat aggregation list for each month exists in the directory,
# NOTE: load the months that exist and create the months that do not. This chunk
# NOTE: assumes that ALL monthly pat lists are ALREADY LOADED

for (i in seq(month_abbrev)){
  
  logger.trace("loading for month %s", month_abbrev[i])
  aggpatList_filename <- paste0(month_abbrev[i], "_aggpatList.rda")
  aggpatList_filePath <- file.path(archiveBaseDir, aggpatList_filename)
  
  if ( file.exists(aggpatList_filePath) ) { #if the patlist already exists, load it
    aggpatList <- get(load(aggpatList_filePath))
  }
  
  if ( !file.exists(aggpatList_filePath) ) {
    
    patlist_ref <- paste0(month_abbrev[i], "_patlist")
    patlist <- eval(as.symbol(patlist_ref))
    
    aggpatList <- list()
    patlist_filt <- list()
    count <- 0
    
    for (id in names(patlist)){
      
      count <- count + 1
      logger.trace("%04d/%d -- pat_aggregate(%s)", count, length(patlist), id)
      result <- try({
        aggpatList[[id]] <-  
          pat_aggregate(
            pat = patlist[[id]],
            period = "1 day"
          )
        aggpatList[[id]]$deviceDeploymentIds <- id
      }, silent = FALSE)
      
      if ("try-error" %in% class(result)) {
        # place holder to handle error
      }
    }
    newname <- paste0(month_abbrev[i], "_aggpatList")
    assign(newname, aggpatList)
    save(list = newname, file = aggpatList_filePath)
    
  }
}

# NOTE: The following processes do not take as long or require as much storage
# NOTE: as the previous chunks so this chunk will not check for existence or save.
# NOTE: Build one big aggregated pat tibble per month

for (i in seq(month_abbrev)){
  logger.trace("Creating aggregation tibble for month %s", month_abbrev[i])
  aggtibble_name <- paste0(month_abbrev[i], "_aggtibble")
  
  patList_ref <- paste0(month_abbrev[i], "_aggpatList")
  patList <- eval(as.symbol(patList_ref))
  
  aggtibble <- 
    dplyr::bind_rows(patList) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
  
  assign(aggtibble_name, aggtibble)
}

# NOTE: The following processes do not take as long or require as much storage
# NOTE: as the previous chunks so this chunk will not check for existence or save.
# NOTE: Build one big SoH tibble per month

for (i in seq(month_abbrev)){
  logger.trace("Creating SoH tibble for month %s", month_abbrev[i])
  sohtibble_name <- paste0(month_abbrev[i], "_sohtibble")
  
  SoHList_ref <- paste0(month_abbrev[i], "_sohlist")
  SoHList <- eval(as.symbol(SoHList_ref))
  
  SoH <- 
    dplyr::bind_rows(SoHList) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
  
  assign(sohtibble_name, SoH)
}

# NOTE: The following processes do not take as long or require as much storage
# NOTE: as the previous chunks so this chunk will not check for existence or save.
# NOTE: Create a monthly dataframe of means for each sensor

for (i in seq(month_abbrev)){
  logger.trace("Creating monthly means tibble for month %s", month_abbrev[i])
  
  datetime <- MazamaCoreUtils::parseDatetime(DATESTAMP[i], timezone = "UTC")
  SoHList_ref <- paste0(month_abbrev[i], "_sohlist")
  SoHList <- eval(as.symbol(SoHList_ref))
  
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
  SoHMeans_name <- paste0(month_abbrev[i], "_sohMeanstibble") 
  assign(SoHMeans_name, SoHMeans)
}


# NOTE: The following processes do not take as long or require as much storage
# NOTE: as the previous chunks so this chunk will not check for existence or save.
# NOTE: Create monthly tidy means without datetime 

for (i in seq(month_abbrev)){
  logger.trace("Creating tidy monthly means for month %s", month_abbrev[i])
  
  SoHMeans_ref <- paste0(month_abbrev[i], "_sohMeanstibble")
  SoHMeans <- eval(as.symbol(SoHMeans_ref))
  
  tidyTbl <- 
    SoHMeans %>%
    dplyr::select(-datetime) %>%
    melt(id.vars='id')
  
  tidyTbl_name <- paste0(month_abbrev[i], "_sohMeansTidy") 
  assign(tidyTbl_name, tidyTbl)
  
}

# NOTE: The following processes do not take as long or require as much storage
# NOTE: as the previous chunks so this chunk will not check for existence or save.
# NOTE: build one big monthly SOH INDEX tibble 

for (i in seq(month_abbrev)){
  logger.trace("Creating monthly Soh Index tibble for month %s", month_abbrev[i])
  
  SoHIndex_ref <- paste0(month_abbrev[i], "_sohIndexlist")
  indexList <- eval(as.symbol(SoHIndex_ref))
  
  index <- 
    dplyr::bind_rows(indexList) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))
  
  index_name <- paste0(month_abbrev[i], "_sohIndextibble") 
  assign(index_name, index)
  
}


#-------------------- Plots -----------------------------------

#### box plot: distribution of single metric from single month

monthlySoH_tbl <- nov_sohtibble

ggplot(monthlySoH_tbl, aes(x = reorder(deviceDeploymentIds, pm25_A_pctReporting, mean), 
                           y = pm25_A_pctReporting)) +
  geom_boxplot() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_rug()

#### SoH index histogram for one month: 

soh_Indextibble <- nov_sohIndextibble
startdate <- "20191101"
enddate <- "20191201"

categorized_index <- 
  soh_Indextibble %>%
  dplyr::mutate(category = dplyr::case_when(index_bin == 1 ~ "poor",
                                            index_bin == 2 ~ "fair",
                                            index_bin == 3 ~ "good")) %>%
  dplyr::filter(.data$datetime >= parseDatetime(datetime = startdate, timezone = "America/Los_Angeles")) %>%
  dplyr::filter(.data$datetime < parseDatetime(datetime = enddate, timezone = "America/Los_Angeles")) 


percent_index <- 
  categorized_index %>%
  dplyr::group_by(.data$datetime, .data$index_bin) %>%
  dplyr::summarise_at(
    .vars = c("category"),
    .funs = function(x) { length(na.omit(x)) }
  ) %>% #this is awesome, make a new column to store the total for each day, populate with the sum calculated based on when 
  #the date is identical
  dplyr::mutate(totals = dplyr::case_when(identical(.data$datetime, .data$datetime) ~ sum(.data$category))) %>%
  dplyr::mutate(percent = .data$category/.data$totals*100) %>%
  dplyr::mutate(group = dplyr::case_when(index_bin == 1 ~ "poor",
                                         index_bin == 2 ~ "fair",
                                         index_bin == 3 ~ "good"))


#colors <- c("firebrick", "goldenrod1", "seagreen3")
#fillColors <- colors[percent_index$percent]
colors <- c("good" = "seagreen3", "fair" = "goldenrod1", "poor" = "firebrick" )

ordered_categories <- c("good", "fair", "poor")
percent_index$group <- factor(percent_index$group, levels = ordered_categories, order = TRUE)

ggplot(percent_index, aes(x = datetime, y = percent, fill = group ) ) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors)



#### box plot: one metric for all months

soh_metric <- "temperature_pctReporting"

monthly_metric <-
  jan_sohMeanstibble %>%
  dplyr::bind_rows(feb_sohMeanstibble) %>%
  dplyr::bind_rows(mar_sohMeanstibble) %>%
  dplyr::bind_rows(apr_sohMeanstibble) %>%
  dplyr::bind_rows(may_sohMeanstibble) %>%
  dplyr::bind_rows(jun_sohMeanstibble) %>%
  dplyr::bind_rows(jul_sohMeanstibble) %>%
  dplyr::bind_rows(aug_sohMeanstibble) %>%
  dplyr::bind_rows(sep_sohMeanstibble) %>%
  dplyr::bind_rows(oct_sohMeanstibble) %>%
  dplyr::bind_rows(nov_sohMeanstibble) %>%
  dplyr::select(soh_metric, "datetime") %>%
  dplyr::mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

ggplot(monthly_metric) +
  geom_boxplot(aes(x = datetime, y = eval(rlang::parse_expr(soh_metric))))


#### percent reporting all months ridgeline plot:

ggplot(monthly_metric, aes(x = eval(rlang::parse_expr(soh_metric)), y = datetime, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient2(low = "firebrick", high = "seagreen3", midpoint = 50) +
  labs(title = eval(soh_metric)) +
  xlab(label = eval(soh_metric)) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

#### SoH index ridgeline plot
startdate <- "20190101"
enddate <- "20191201"

allmonths_sohIndex <-
  jan_sohIndextibble %>%
  dplyr::bind_rows(feb_sohIndextibble) %>%
  dplyr::bind_rows(mar_sohIndextibble) %>%
  dplyr::bind_rows(apr_sohIndextibble) %>%
  dplyr::bind_rows(may_sohIndextibble) %>%
  dplyr::bind_rows(jun_sohIndextibble) %>%
  dplyr::bind_rows(jul_sohIndextibble) %>%
  dplyr::bind_rows(aug_sohIndextibble) %>%
  dplyr::bind_rows(sep_sohIndextibble) %>%
  dplyr::bind_rows(oct_sohIndextibble) %>%
  dplyr::bind_rows(nov_sohIndextibble) %>%
  dplyr::filter(.data$datetime >= parseDatetime(datetime = startdate, timezone = "America/Los_Angeles")) %>%
  dplyr::filter(.data$datetime < parseDatetime(datetime = enddate, timezone = "America/Los_Angeles")) %>%
  dplyr::mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC")) 

breaks = c(0, .2, .8, 1)
ggplot(allmonths_sohIndex, aes(x = index, y = datetime, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient2(low = "firebrick", mid ="goldenrod1",  high = "seagreen3", midpoint = 0.4, breaks = breaks) +
  labs(title = "SoH Index") +
  xlab(label = "SoH Index") +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  scale_y_discrete(limits = rev(sort(allmonths_sohIndex$datetime)))

#### aggregated stats ridgeline plot
startdate <- "20190101"
enddate <- "20191130"
x_metric <- "pm25_A_mean"

allmonths_aggstats <-
  jan_aggtibble %>%
  dplyr::bind_rows(feb_aggtibble) %>%
  dplyr::bind_rows(mar_aggtibble) %>%
  dplyr::bind_rows(apr_aggtibble) %>%
  dplyr::bind_rows(may_aggtibble) %>%
  dplyr::bind_rows(jun_aggtibble) %>%
  dplyr::bind_rows(jul_aggtibble) %>%
  dplyr::bind_rows(aug_aggtibble) %>%
  dplyr::bind_rows(sep_aggtibble) %>%
  dplyr::bind_rows(oct_aggtibble) %>%
  dplyr::bind_rows(nov_aggtibble) %>%
  dplyr::filter(.data$datetime >= parseDatetime(datetime = startdate, timezone = "America/Los_Angeles")) %>%
  dplyr::filter(.data$datetime < parseDatetime(datetime = enddate, timezone = "America/Los_Angeles")) %>%
  dplyr::mutate(A_B_mean_diff = .data$pm25_A_mean - .data$pm25_B_mean)

allmonths_aggstats_filt <-
  allmonths_aggstats %>%
  dplyr::filter(.data$temperature_mean > -40 ) %>%
  dplyr::filter(.data$temperature_mean < 185 ) %>%
  dplyr::filter(.data$pm25_A_mean < 300) %>%
  dplyr::filter(.data$pm25_B_mean < 300) #%>%
#dplyr::filter(.data$pm25_A_max < 1000) %>%
#dplyr::filter(.data$pm25_B_max < 1000) 

allmonths_aggstats_ridgeline <-
  allmonths_aggstats %>%
  dplyr::mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC")) %>%
   #%>%
# dplyr::filter(.data$A_B_mean_diff <= 70) %>%
# dplyr::filter(.data$A_B_mean_diff >= -50) 

# Unfiltered ridgeline plot of one channel
ggplot(allmonths_aggstats_ridgeline, aes(x = pm25_B_mean, y = datetime, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3,rel_min_height = 0.01, na.rm = TRUE)+
  xlim(-1, 100) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  scale_y_discrete(limits = rev(sort(allmonths_aggstats_ridgeline$datetime))) 

# Unfiltered ridgeline plot of both channels:
ggplot(allmonths_aggstats_ridgeline ) +
  geom_density_ridges_gradient(aes(x = pm25_A_mean, y = datetime, fill = ..x..), scale = 3,
                               rel_min_height = 0.01, na.rm = TRUE, color = "red") +
  geom_density_ridges_gradient(aes(x = pm25_B_mean, y = datetime, fill = ..x..), scale = 3,
                               rel_min_height = 0.01, na.rm = TRUE, color = "blue")+
  scale_fill_gradient2(low = "blue",  high = "white", midpoint = 50) +
  xlim(-1, 100) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  scale_y_discrete(limits = rev(sort(allmonths_aggstats_ridgeline$datetime))) 


# Check out the difference between filtered and unfiltered means:
ggplot(allmonths_aggstats, aes(x = datetime, y = pm25_A_mean )) +
  geom_point() +
  geom_point(data = allmonths_aggstats_filt, aes(x = datetime, y = pm25_A_mean ), color = "pink")

# All unfiltered data, scatterplot, one channel
ggplot(allmonths_aggstats, aes(x = datetime, y = A_B_mean_diff )) +
  geom_point(alpha = 0.2, shape = 15) +
  geom_smooth()+
  ylim(-15, 200)

ggplot(allmonths_aggstats, aes(x = datetime, y = humidity_mean )) +
  geom_point(alpha = 0.2, shape = 15) +
  geom_smooth() +
  ylim(-15, 200)

ggplot(allmonths_aggstats, aes(x = datetime, y = pm25_B_mean )) +
  geom_point(alpha = 0.2, shape = 15) +
  geom_rug(sides = 'l', position = "jitter") +
  geom_hline(data = AQI, yintercept = AQI$breaks_24[2:6], color = AQI$colors[2:6]) +
  geom_smooth() +
  ylim(-15, 250)

# Filtered data, scatterplot, one channel
ggplot(allmonths_aggstats_filt, aes(x = datetime, y = pm25_B_mean )) +
  geom_point(alpha = 0.2, shape = 15) +
  geom_rug(sides = 'l') +
  geom_hline(data = AQI, yintercept = AQI$breaks_24[2:6], color = AQI$colors[2:6])

# Channels A and B, filtered, with AQI lines, scatterplot:
ggplot(allmonths_aggstats_filt) +
  geom_point(aes(x = datetime, y = pm25_B_mean), color = "blue", alpha = 0.25, shape = 15) +
  geom_point(aes(x = datetime, y = pm25_A_mean), color = "red", alpha = 0.25, shape = 15) +
  geom_hline(data = AQI, yintercept = AQI$breaks_24[2:6], color = AQI$colors[2:6])

# Channels A and B, unfiltered, scatterplot:
ggplot(allmonths_aggstats) +
  geom_point(aes(x = datetime, y = pm25_B_mean), color = "blue", alpha = 0.25, shape = 15) +
  geom_point(aes(x = datetime, y = pm25_A_mean), color = "red", alpha = 0.25, shape = 15) +
  stat_smooth(geom = "line", aes(x = datetime, y = pm25_B_mean, color = "blue")) +
  stat_smooth(geom = "line", aes(x = datetime, y = pm25_A_mean, color = "red")) +
  ylim(0, 300)

dropout_days <- 
  jan_sohtibble %>%
  dplyr::bind_rows(feb_sohtibble) %>%
  dplyr::bind_rows(mar_sohtibble) %>%
  dplyr::bind_rows(apr_sohtibble) %>%
  dplyr::bind_rows(may_sohtibble) %>%
  dplyr::bind_rows(jun_sohtibble) %>%
  dplyr::bind_rows(jul_sohtibble) %>%
  dplyr::bind_rows(aug_sohtibble) %>%
  dplyr::bind_rows(sep_sohtibble) %>%
  dplyr::bind_rows(oct_sohtibble) %>%
  dplyr::bind_rows(nov_sohtibble) %>%
  dplyr::filter(.data$pm25_A_pctReporting <= 50) %>%
  dplyr::filter(.data$datetime >= parseDatetime(datetime = startdate, timezone = "America/Los_Angeles")) %>%
  dplyr::filter(.data$datetime < parseDatetime(datetime = enddate, timezone = "America/Los_Angeles")) %>%
  dplyr::mutate(datetime = strftime(datetime, format = "%Y/%m/%d", tz = "America/Los_Angeles")) %>%
  dplyr::count(.data$datetime) %>%
  dplyr::mutate(datetime = parseDatetime(.data$datetime, timezone = "America/Los_Angeles"))

# Channels A and B, unfiltered, PLUS dropout days, scatterplot:
ggplot(allmonths_aggstats_filt) +
  geom_point(aes(x = datetime, y = pm25_B_mean), color = "blue", alpha = 0.25, shape = 15) +
  geom_point(aes(x = datetime, y = pm25_A_mean), color = "red", alpha = 0.25, shape = 15) +
  geom_point(data = dropout_days, aes(x = datetime, y = n))+
  ylim(0, 50)

# R2 line plot
allmonths_R2 <-
  jan_sohtibble%>%
  dplyr::bind_rows(feb_sohtibble) %>%
  dplyr::bind_rows(mar_sohtibble) %>%
  dplyr::bind_rows(apr_sohtibble) %>%
  dplyr::bind_rows(may_sohtibble) %>%
  dplyr::bind_rows(jun_sohtibble) %>%
  dplyr::bind_rows(jul_sohtibble) %>%
  dplyr::bind_rows(aug_sohtibble) %>%
  dplyr::bind_rows(sep_sohtibble) %>%
  dplyr::bind_rows(oct_sohtibble) %>%
  dplyr::bind_rows(nov_sohtibble) 

ggplot(allmonths_R2, aes(x = datetime, y = pm25_A_pm25_B_rsquared )) +
  stat_smooth(geom = "line", method = "loess", se = FALSE, span =0.2,
              aes( color = factor(allmonths_R2$deviceDeploymentIds)))+
  scale_color_discrete(palette = function(n) {
    rep(adjustcolor("black", alpha = 0.05), n)
  }) +
  theme(legend.position="none")
  
# Separate East and West side of the state based on Leavenworth's longitude 
mid_state_long <- -120.799722
pas_WA_west <- state_pas %>% pas_filter(longitude <= mid_state_long )
pas_WA_east <- state_pas %>% pas_filter(longitude >= mid_state_long )
west_DDID <- pas_WA_west$deviceDeploymentID
east_DDID <- pas_WA_east$deviceDeploymentID

allmonths_R2_DDID_east<- dplyr::filter(allmonths_R2, 
                                           stringr::str_detect(deviceDeploymentIds, paste(east_DDID, collapse="|") ))


ggplot(allmonths_R2_DDID_east, aes(x = datetime, y = pm25_A_pm25_B_rsquared )) +
  stat_smooth(geom = "line", method = "loess", se = FALSE, span =0.2,
              aes( color = factor(allmonths_R2_DDID_east$deviceDeploymentIds)))+
  scale_color_discrete(palette = function(n) {
    rep(adjustcolor("black", alpha = 0.05), n)
  }) +
  theme(legend.position="none")+
  labs(title = "R2 for sensors EAST of the Cascades")


allmonths_R2_DDID_west<- dplyr::filter(allmonths_R2, 
                                       stringr::str_detect(deviceDeploymentIds, paste(west_DDID, collapse="|") ))


ggplot(allmonths_R2_DDID_west, aes(x = datetime, y = pm25_A_pm25_B_rsquared )) +
  stat_smooth(geom = "line", method = "loess", se = FALSE, span =0.2,
              aes( color = factor(allmonths_R2_DDID_west$deviceDeploymentIds)))+
  scale_color_discrete(palette = function(n) {
    rep(adjustcolor("black", alpha = 0.05), n)
  }) +
  theme(legend.position="none")+
  labs(title = "R2 for sensors WEST of the Cascades")

ggplot() +
  stat_smooth(data = allmonths_R2_DDID_east, 
              aes(x = allmonths_R2_DDID_east$datetime, y = allmonths_R2_DDID_east$pm25_A_pm25_B_rsquared ),
              geom = "line", method = "loess", se = FALSE, span =0.2,
              aes( color = factor(allmonths_R2_DDID_east$deviceDeploymentIds)))+
  scale_color_discrete(palette = function(n) {
    rep(adjustcolor("black", alpha = 0.05), n)
  }) +
  stat_smooth(data = allmonths_R2_DDID_west, 
              aes(x = allmonths_R2_DDID_west$datetime, y =allmonths_R2_DDID_west$pm25_A_pm25_B_rsquared ),
              geom = "line", method = "loess", se = FALSE, span =0.2,
              aes( color = factor(allmonths_R2_DDID_west$deviceDeploymentIds)))+
  scale_color_discrete(palette = function(n) {
    rep(adjustcolor("black", alpha = 0.05), n)
  }) +
  theme(legend.position="none")+
  labs(title = "R2 for sensors EAST of the Cascades")



##### box plot: average of each metric for single month

orderedParams <- c(
  "pm25_A_pctReporting",
  "pm25_B_pctReporting",
  "temperature_pctReporting",
  "humidity_pctReporting",
  "pm25_A_pctValid",
  "pm25_B_pctValid",
  "temperature_pctValid",
  "humidity_pctValid",
  "pm25_A_pctDC",
  "pm25_B_pctDC",
  "temperature_pctDC",
  "humidity_pctDC",
  "pm25_A_pm25_B_rsquared",
  "pm25_A_pm25_B_slope",
  "pm25_A_pm25_B_intercept",
  "pm25_A_pm25_B_p_value",
  "pm25_A_humidity_rsquared",
  "pm25_A_temperature_rsquared",
  "pm25_B_humidity_rsquared",
  "pm25_B_temperature_rsquared"
)

tidySoHMeans <- jan_sohMeansTidy
tidySoHMeans$variable <- factor(tidySoHMeans$variable, levels = orderedParams, order = TRUE)

ggplot(tidySoHMeans) +
  geom_boxplot(aes(x = variable, y = value)) +
  coord_flip() +
  labs(title = "January")


##### box plot: single month metrics on scale from 0:150 percent

orderedParams <- c(
  "pm25_A_pctReporting",
  "pm25_B_pctReporting",
  "temperature_pctReporting",
  "humidity_pctReporting",
  "pm25_A_pctValid",
  "pm25_B_pctValid",
  "temperature_pctValid",
  "humidity_pctValid",
  "pm25_A_pctDC",
  "pm25_B_pctDC",
  "temperature_pctDC",
  "humidity_pctDC"
)

tidySoHMeans <- jan_sohMeansTidy

tidySoHMeans <-
  tidySoHMeans %>%
  dplyr::filter(variable == "pm25_A_pctReporting"
                | variable ==  "pm25_B_pctReporting"
                | variable == "temperature_pctReporting"
                | variable == "humidity_pctReporting"
                | variable == "pm25_A_pctValid"
                | variable == "pm25_B_pctValid"
                | variable == "temperature_pctValid"
                | variable == "humidity_pctValid"
                | variable == "pm25_A_pctDC"
                | variable == "pm25_B_pctDC"
                | variable == "temperature_pctDC"
                | variable == "humidity_pctDC")

tidySoHMeans$variable <- factor(tidySoHMeans$variable, levels = orderedParams, order = TRUE)

ggplot(tidySoHMeans) +
  geom_boxplot(aes(x = variable, y = value)) +
  coord_flip() +
  labs(title = "January")



##### box plot: single month metrics on smaller scale (ie, NOT 0-150%)

orderedParams <- c(
  "pm25_A_pm25_B_rsquared",
  "pm25_A_pm25_B_slope",
  "pm25_A_pm25_B_intercept",
  "pm25_A_pm25_B_p_value",
  "pm25_A_humidity_rsquared",
  "pm25_A_temperature_rsquared",
  "pm25_B_humidity_rsquared",
  "pm25_B_temperature_rsquared"
)

tidySoHMeans <- jan_sohMeansTidy

tidySoHMeans <-
  tidySoHMeans %>%
  dplyr::filter(variable == "pm25_A_pm25_B_rsquared"
                | variable ==  "pm25_A_pm25_B_slope"
                | variable == "pm25_A_pm25_B_intercept"
                | variable == "pm25_A_pm25_B_p_value"
                | variable == "pm25_A_humidity_rsquared"
                | variable == "pm25_A_temperature_rsquared"
                | variable == "pm25_B_humidity_rsquared"
                | variable == "pm25_B_temperature_rsquared")

tidySoHMeans$variable <- factor(tidySoHMeans$variable, levels = orderedParams, order = TRUE)

ggplot(tidySoHMeans) +
  geom_boxplot(aes(x = variable, y = value)) +
  coord_flip() +
  labs(title = "January")




##### box plot: all metrics, all months, aka chaos:

# jan_all_metrics <-
#   tidyTbl %>%
#   dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(datetime = "201901", timezone = "UTC")) %>%
#   mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))
# feb_all_metrics <-
#   feb_tidyTbl %>%
#   dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(datetime = "201902", timezone = "UTC")) %>%
#   mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))
# 
# monthly_all_metrics <-
#   jan_all_metrics %>%
#   dplyr::bind_rows(feb_all_metrics)
# 
# orderedParams <- c(
#   "pm25_A_pctReporting",
#   "pm25_B_pctReporting",
#   "temperature_pctReporting",
#   "humidity_pctReporting",
#   "pm25_A_pctValid",
#   "pm25_B_pctValid",
#   "temperature_pctValid",
#   "humidity_pctValid",
#   "pm25_A_pctDC",
#   "pm25_B_pctDC",
#   "temperature_pctDC",
#   "humidity_pctDC",
#   "pm25_A_pm25_B_rsquared",
#   "pm25_A_pm25_B_slope",
#   "pm25_A_pm25_B_intercept",
#   "pm25_A_pm25_B_p_value",
#   "pm25_A_humidity_rsquared",
#   "pm25_A_temperature_rsquared",
#   "pm25_B_humidity_rsquared",
#   "pm25_B_temperature_rsquared"
# )
# 
# monthly_all_metrics$variable <- factor(monthly_all_metrics$variable, levels = orderedParams, order = TRUE)
# 
# ggplot(monthly_all_metrics) +
#   geom_boxplot(aes(x = variable, y = value)) +
#   coord_flip()+
#   facet_grid(cols = vars(monthly_all_metrics$datetime))


