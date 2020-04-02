# Single state statistics

DATESTAMP <- "201901"

library(ggplot2)

library(MazamaCoreUtils)
library(AirSensor)
setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")

logger.setup()
logger.setLevel(TRACE)

state_pas <- 
  pas_load(archival = TRUE) %>%
  pas_filter(stateCode == "WA")

deviceDeploymentIDs <- pas_getDeviceDeploymentIDs(state_pas)

# Get all PATs for January, 2019

patList <- list()
patEmptyList <- list()

count <- 0
for ( id in deviceDeploymentIDs ) {
  
  count <- count + 1
  logger.trace("%04d/%d -- pat_loadMonth(%s)", count, length(deviceDeploymentIDs), id)
  
  # Load Januar data and trim the date so we don't statistics for partial days
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

# Create State-of-Health metrics where we have data

SoHList <- list()

count <- 0
for ( id in names(patList) ) {
  
  count <- count + 1
  logger.trace("%04d/%d -- pat_dailySoH(%s)", count, length(patList), id)
  
  SoHList[[id]] <- pat_dailySoH(patList[[id]])
  SoHList[[id]]$deviceDeploymentID <- id
  
}

# Build one big tibble for use wigh ggplot

SoH <- 
  dplyr::bind_rows(SoHList) %>%
  dplyr::mutate_if(is.numeric, ~replace(., is.nan(.), as.numeric(NA)))

# Create a plot showing the range of temperature_pctReporting for every sensor

ggplot(SoH, aes(x = reorder(deviceDeploymentID, temperature_pctReporting, mean), 
                y = temperature_pctReporting)) +
  geom_boxplot() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Create a dataframe of monthly means for each sensor

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

# Now to plot boxplots for each metric showing the daily average across all sensors
# See: https://stackoverflow.com/questions/14785530/ggplot-boxplot-of-multiple-column-values

library(reshape2)
tidyTbl <- 
  SoHMeans %>%
  dplyr::select(-datetime) %>%
  melt(id.vars='id')

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

tidyTbl$variable <- factor(tidyTbl$variable, levels = orderedParams, order = TRUE)

ggplot(tidyTbl) +
  geom_boxplot(aes(x = variable, y = value)) +
  coord_flip()

# TODO:  Could generate separate plot for the 0-100 and 0-1 variables

# TODO:  Next level of roll-up statistics would be a set of monthly boxplots for
# TODO:  a single parameter to see how it evolves with time. It is probably important
# TODO:  to limit yourself to sensors that had data in January so we know we are
# TODO:  talking about a non-changing set of sensors.


