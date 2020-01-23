# Single state statistics
# start up code
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

# The following monthly lists/variables are created from functions stored in 
# local_kayleigh/SoH_stats_test_functions.R. The functions are stored there
# to simplify the code in this testing document. They are a wip.

#------------------------- January --------------------------------
#load monthly pats:
jan_patlist <- loadMonthlyPats(DATESTAMP = 201901, deviceDeploymentIDs = deviceDeploymentIDs)
#create monthly soh list:
jan_sohlist <- createSoHList(patList = jan_patlist)
#create monthly single soh tibble: 
jan_sohtbl <- createSingleSoHTbl(SoHList = jan_sohlist)
#create monthly soh means:
jan_sohmeans <- createMonthlyMeans(DATESTAMP = 201901, SoHList = jan_sohlist)
#tidy means:
jan_tidysohmeans <- createTidyMeans(SoHMeans = jan_sohmeans)
####IMPORTANT: gather january deploymentdeviceIDs for further use:
jan_devicedeploymentIDs <- jan_sohmeans$id

#----------------------- February -------------------------------
#load monthly pats:
feb_patlist <- loadMonthlyPats(DATESTAMP = 201902, deviceDeploymentIDs = jan_devicedeploymentIDs)
#create monthly soh list:
feb_sohlist <- createSoHList(patList = feb_patlist)
#create monthly single soh tibble: 
feb_sohtbl <- createSingleSoHTbl(SoHList = feb_sohlist)
#create monthly soh means:
feb_sohmeans <- createMonthlyMeans(DATESTAMP = 201902, SoHList = feb_sohlist)
#tidy means:
feb_tidysohmeans <- createTidyMeans(SoHMeans = feb_sohmeans)

#----------------------- March -------------------------------
#load monthly pats:
mar_patlist <- loadMonthlyPats(DATESTAMP = 201903, deviceDeploymentIDs = jan_devicedeploymentIDs)
#create monthly soh list:
mar_sohlist <- createSoHList(patList = mar_patlist)
#create monthly single soh tibble: 
mar_sohtbl <- createSingleSoHTbl(SoHList = mar_sohlist)
#create monthly soh means:
mar_sohmeans <- createMonthlyMeans(DATESTAMP = 201903, SoHList = mar_sohlist)
#tidy means:
mar_tidysohmeans <- createTidyMeans(SoHMeans = feb_sohmeans)




#-------------------- Plots -----------------------------------

#### box plot: distribution of single metric from single month

monthlySoH_tbl <- jan_sohtbl

ggplot(monthlySoH_tbl, aes(x = reorder(deviceDeploymentID, pm25_A_pctReporting, mean), 
                           y = pm25_A_pctReporting)) +
  geom_boxplot() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_rug()



#### box plot: one metric for all months

soh_metric <- "pm25_A_pm25_B_p_value"

jan_metric <- 
  jan_sohmeans %>%
  dplyr::select(soh_metric, "datetime") %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

feb_metric <- 
  feb_sohmeans %>%
  dplyr::select(soh_metric, "datetime") %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

mar_metric <-
  mar_sohmeans %>%
  dplyr::select(soh_metric, "datetime") %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

monthly_metric <-
  jan_metric %>%
  dplyr::bind_rows(feb_metric) %>%
  dplyr::bind_rows(mar_metric)


ggplot(monthly_metric) +
  geom_boxplot(aes(x = datetime, y = pm25_A_pm25_B_p_value)) 



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

tidySoHMeans <- jan_tidysohmeans
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

tidySoHMeans <- jan_tidysohmeans

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

tidySoHMeans <- jan_tidysohmeans

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

jan_all_metrics <-
  tidyTbl %>%
  dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(datetime = "201901", timezone = "UTC")) %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))
feb_all_metrics <-
  feb_tidyTbl %>%
  dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(datetime = "201902", timezone = "UTC")) %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

monthly_all_metrics <-
  jan_all_metrics %>%
  dplyr::bind_rows(feb_all_metrics)

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

monthly_all_metrics$variable <- factor(monthly_all_metrics$variable, levels = orderedParams, order = TRUE)

ggplot(monthly_all_metrics) +
  geom_boxplot(aes(x = variable, y = value)) +
  coord_flip()+
  facet_grid(cols = vars(monthly_all_metrics$datetime))


