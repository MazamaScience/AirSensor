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
#soh index: 
jan_sohindex <- monthlySOH_index(patList = jan_patlist)
jan_monthlyindex <- createSingleSoH_Index_Tbl(indexList = jan_sohindex)
####IMPORTANT: gather january deploymentdeviceIDs for further use:
jan_devicedeploymentIDs <- jan_sohmeans$id

filename <- c("jan_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "jan_patlist", file = filepath)

soh_filename <- c("jan_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "jan_sohlist", file = soh_filepath)
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

filename <- c("feb_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "feb_patlist", file = filepath)

soh_filename <- c("feb_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "feb_sohlist", file = soh_filepath)

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

filename <- c("mar_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "mar_patlist", file = filepath)

soh_filename <- c("mar_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "mar_sohlist", file = soh_filepath)

#----------------------- April -------------------------------
#load monthly pats:
apr_patlist <- loadMonthlyPats(DATESTAMP = 201904, deviceDeploymentIDs = jan_devicedeploymentIDs)
#create monthly soh list:
apr_sohlist <- createSoHList(patList = apr_patlist)
#create monthly single soh tibble: 
apr_sohtbl <- createSingleSoHTbl(SoHList = apr_sohlist)
#create monthly soh means:
apr_sohmeans <- createMonthlyMeans(DATESTAMP = 201904, SoHList = apr_sohlist)
#tidy means:
apr_tidysohmeans <- createTidyMeans(SoHMeans = apr_sohmeans)

filename <- c("apr_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "apr_patlist", file = filepath)

soh_filename <- c("apr_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "apr_sohlist", file = soh_filepath)

#----------------------- May -------------------------------
#load monthly pats:
may_patlist <- loadMonthlyPats(DATESTAMP = 201905, deviceDeploymentIDs = jan_devicedeploymentIDs)
#create monthly soh list:
may_sohlist <- createSoHList(patList = may_patlist)
#create monthly single soh tibble: 
may_sohtbl <- createSingleSoHTbl(SoHList = may_sohlist)
#create monthly soh means:
may_sohmeans <- createMonthlyMeans(DATESTAMP = 201905, SoHList = may_sohlist)
#tidy means:
may_tidysohmeans <- createTidyMeans(SoHMeans = may_sohmeans)

filename <- c("may_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "may_patlist", file = filepath)

soh_filename <- c("may_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "may_sohlist", file = soh_filepath)

#----------------------- June -------------------------------
#load monthly pats:
jun_patlist <- loadMonthlyPats(DATESTAMP = 201906, deviceDeploymentIDs = jan_devicedeploymentIDs)
#create monthly soh list:
jun_sohlist <- createSoHList(patList = jun_patlist)
#create monthly single soh tibble: 
jun_sohtbl <- createSingleSoHTbl(SoHList = jun_sohlist)
#create monthly soh means:
jun_sohmeans <- createMonthlyMeans(DATESTAMP = 201906, SoHList = jun_sohlist)
#tidy means:
jun_tidysohmeans <- createTidyMeans(SoHMeans = jun_sohmeans)

filename <- c("jun_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "jun_patlist", file = filepath)

soh_filename <- c("jun_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "jun_sohlist", file = soh_filepath)

#----------------------- July -------------------------------
#load monthly pats:
jul_patlist <- loadMonthlyPats(DATESTAMP = 201907, deviceDeploymentIDs = jan_devicedeploymentIDs)
#create monthly soh list:
jul_sohlist <- createSoHList(patList = jul_patlist)
#create monthly single soh tibble: 
jul_sohtbl <- createSingleSoHTbl(SoHList = jul_sohlist)
#create monthly soh means:
jul_sohmeans <- createMonthlyMeans(DATESTAMP = 201907, SoHList = jul_sohlist)
#tidy means:
jul_tidysohmeans <- createTidyMeans(SoHMeans = jul_sohmeans)

filename <- c("jul_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "jul_patlist", file = filepath)

soh_filename <- c("jul_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "jul_sohlist", file = soh_filepath)

#----------------------- August -------------------------------
#load monthly pats:
aug_patlist <- loadMonthlyPats(DATESTAMP = 201908, deviceDeploymentIDs = jan_devicedeploymentIDs)
#create monthly soh list:
aug_sohlist <- createSoHList(patList = aug_patlist)
#create monthly single soh tibble: 
aug_sohtbl <- createSingleSoHTbl(SoHList = aug_sohlist)
#create monthly soh means:
aug_sohmeans <- createMonthlyMeans(DATESTAMP = 201908, SoHList = aug_sohlist)
#tidy means:
aug_tidysohmeans <- createTidyMeans(SoHMeans = aug_sohmeans)

filename <- c("aug_patlist.rda")
filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", filename)
save(list = "aug_patlist", file = filepath)

soh_filename <- c("aug_sohlist.rda")
soh_filepath <- file.path("/Users/kayleigh/Data/MonthlySohStats_data/", soh_filename)
save(list = "aug_sohlist", file = soh_filepath)

#-------------------- Plots -----------------------------------

#### box plot: distribution of single metric from single month

monthlySoH_tbl <- jun_sohtbl

ggplot(monthlySoH_tbl, aes(x = reorder(deviceDeploymentID, pm25_A_pctReporting, mean), 
                           y = pm25_A_pctReporting)) +
  geom_boxplot() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_rug()

#### SoH index histogram for one month: 

categorized_index <- 
  jan_monthlyindex %>%
  dplyr::mutate(category = case_when(index_bin == 1 ~ "poor",
                                     index_bin == 2 ~ "fair",
                                     index_bin == 3 ~ "good")) %>%
  dplyr::filter(.data$datetime >= parseDatetime(datetime = "20190101", timezone = "America/Los_Angeles")) %>%
  dplyr::filter(.data$datetime < parseDatetime(datetime = "20190201", timezone = "America/Los_Angeles")) #%>%
  #dplyr::arrange(.data$datetime)

test_index <- 
  categorized_index %>%
  #dplyr::mutate(daystamp = strftime(.data$datetime, "%Y%m%d", tz = "America/Los_Angeles")) %>%
  dplyr::group_by(.data$datetime, .data$index_bin) %>%
  dplyr::summarise_at(
    .vars = c("category"),
    .funs = function(x) { length(na.omit(x)) }
  ) %>% #this is awesome, make a new column to store the total for each day, populate with the sum calculated based on when 
  #the date is identical
  dplyr::mutate(totals = case_when(identical(.data$datetime, .data$datetime) ~ sum(.data$category))) %>%
  dplyr::mutate(percent = .data$category/.data$totals*100) %>%
  dplyr::mutate(group = case_when(index_bin == 1 ~ "poor",
                                     index_bin == 2 ~ "fair",
                                     index_bin == 3 ~ "good"))


colors <- c("firebrick", "goldenrod1", "seagreen3")
fillColors <- colors[test_index$percent]
colors <- c("good" = "seagreen3", "fair" = "goldenrod1", "poor" = "firebrick" )

ordered_categories <- c("good", "fair", "poor")
test_index$group <- factor(test_index$group, levels = ordered_categories, order = TRUE)

ggplot(test_index, aes(x = datetime, y = percent, fill = group ) ) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors)

# ggplot(test_index ) +
#   geom_bar(aes(x = datetime, y = (..count..)/sum(..count..), fill = category )) +
#   scale_fill_manual(values = colors) +
#   scale_y_continuous(labels = percent_format())

ggplot(categorized_index ) +
  geom_freqpoly(aes(x = datetime, color = category))+
  scale_color_manual(values = colors)


#### box plot: one metric for all months

soh_metric <- "temperature_pctReporting"

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

apr_metric <-
  apr_sohmeans %>%
  dplyr::select(soh_metric, "datetime") %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

may_metric <-
  may_sohmeans %>%
  dplyr::select(soh_metric, "datetime") %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

jun_metric <-
  jun_sohmeans %>%
  dplyr::select(soh_metric, "datetime") %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

jul_metric <-
  jul_sohmeans %>%
  dplyr::select(soh_metric, "datetime") %>%
  mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))

monthly_metric <-
  jan_metric %>%
  dplyr::bind_rows(feb_metric) %>%
  dplyr::bind_rows(mar_metric) %>%
  dplyr::bind_rows(apr_metric) %>%
  dplyr::bind_rows(may_metric) %>%
  dplyr::bind_rows(jun_metric) %>%
  dplyr::bind_rows(jul_metric)


ggplot(monthly_metric) +
  geom_boxplot(aes(x = datetime, y = temperature_pctReporting)) 



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


