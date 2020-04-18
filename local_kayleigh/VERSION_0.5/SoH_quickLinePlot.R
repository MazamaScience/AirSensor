library(AirSensor)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(purrr)
library(plotly)
library(gridExtra)
library(skimr)

# ----- Whole year from USFS -------------------------------------------------

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()
pat <- pat_load("MV Clean Air Ambassador @ Willowbrook Farm", startdate = 20190101, enddate = 20191101) #summer gap
pat <- pat_load("Spokane South Hill #1", startdate = 20190101, enddate = 20191023) #super clean station

# ------ data from California from Oct 2017 - South Coast -----------

setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
ca <- pas_load() %>% pas_filter(stateCode == "CA")
scaqmd <- ca %>% pas_filter(stringr::str_detect(label, "SC.._"))
pas_leaflet(scaqmd)  
pat <- pat_load("SCPR_05", startdate = 20190401, enddate = 20190723)

# ------ data for all of WA/OR for 2019 ----------------
# Also has recent data for all sensors in California.
setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")

# ----- data/SoH tibbles calculated overnight ----------------------------------
# grab data from the saved SoH. All saved SoH's are from south coast  
path = "/Users/kayleigh/Projects/sensor-data-ingest-v1/output/SoH/2019/"
SoH <- get(load("/Users/kayleigh/Projects/sensor-data-ingest-v1/output/SoH/2019/SoH_SCAH_29_2019.rda"))
pat <- get(load("/Users/kayleigh/Projects/sensor-data-ingest-v1/output/pat/2019/pat_SCAH_29_2019.rda"))


# --------- Flat-lined plot ---------------------------------------------------

# wanted plots: all _pctReporting, A and B _pctDC, (?) all pctValid (?), 
# A/B cor, temp/humd cor, A/B intercept and slope = 10 (plus valid if want)

pat_sub <- pat_sample(pat, keepOutliers = TRUE) #for viewing a subset of the pat data
station_name <- pat$meta$label 

# quick look at the pat
pat_multiplot(pat_sub)

SoH_sub <- dplyr::select(SoH, "datetime", 
                         contains("_pctReporting"),
                         "pm25_A_pctValid",
                         "pm25_B_pctValid",
                         "pm25_A_pctDC", 
                         "pm25_B_pctDC", 
                         "pm25_A_pm25_B_slope",
                         "pm25_A_pm25_B_intercept",
                         "pm25_A_pm25_B_rsquared",
                         "pm25_A_temperature_cor",
                         "pm25_B_temperature_cor") 

datetime <- SoH_sub$datetime

SoH_tidy <-
  SoH_sub %>%
  tidyr::gather(variable, value, -datetime) %>%
  mutate(expectedValue = as.integer(factor(variable))) 

SoH_tidy <- 
  SoH_tidy %>%
  mutate(expectedValue = case_when(
    grepl("_pctReporting", SoH_tidy$variable) ~ 150,
    grepl("_pctValid", SoH_tidy$variable) ~ 100,
    grepl("_pctDC", SoH_tidy$variable) ~ 0,
    grepl("pm25_A_pm25_B_slope", SoH_tidy$variable) ~ 1,
    grepl("pm25_A_pm25_B_intercept", SoH_tidy$variable) ~ 0,
    grepl("pm25_A_pm25_B_rsquared", SoH_tidy$variable) ~ 1,
    grepl("pm25_A_temperature_cor", SoH_tidy$variable) ~ 0,
    grepl("pm25_B_temperature_cor", SoH_tidy$variable) ~ 0)) 


SoH_tidy$variable <- factor(SoH_tidy$variable, levels=c(
  "pm25_A_pctReporting",
  "pm25_B_pctReporting",
  "temperature_pctReporting",
  "humidity_pctReporting",
  "pm25_A_pctValid",
  "pm25_B_pctValid",
  "pm25_A_pctDC",
  "pm25_B_pctDC",
  "pm25_A_pm25_B_slope",
  "pm25_A_pm25_B_intercept",
  "pm25_A_pm25_B_rsquared",
  "pm25_A_temperature_cor",
  "pm25_B_temperature_cor"
))

pm25_A_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
pm25_B_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
temperature_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
humidity_pctReporting <- rep_len(c(0, 150), length.out = length(SoH_sub$datetime))
pm25_A_pctValid <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
pm25_B_pctValid <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
pm25_A_pctDC <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
pm25_B_pctDC <- rep_len(c(0, 100), length.out = length(SoH_sub$datetime))
pm25_A_pm25_B_slope <- rep_len(c(-5, 5), length.out = length(SoH_sub$datetime))
pm25_A_pm25_B_intercept <- rep_len(c(-50, 50), length.out = length(SoH_sub$datetime))
pm25_A_pm25_B_rsquared <- rep_len(c(0, 1), length.out = length(SoH_sub$datetime))
pm25_A_temperature_cor <- rep_len(c(-1, 1), length.out = length(SoH_sub$datetime))
pm25_B_temperature_cor <- rep_len(c(-1, 1), length.out = length(SoH_sub$datetime))

dummy <- data.frame(datetime,
                    pm25_A_pctReporting, 
                    pm25_B_pctReporting,
                    temperature_pctReporting,
                    humidity_pctReporting,
                    pm25_A_pctValid,
                    pm25_B_pctValid,
                    pm25_A_pctDC,
                    pm25_B_pctDC,
                    pm25_A_pm25_B_slope,
                    pm25_A_pm25_B_intercept,
                    pm25_A_pm25_B_rsquared,
                    pm25_A_temperature_cor,
                    pm25_B_temperature_cor)
dummy_tidy <-
  dummy %>%
  tidyr::gather(variable, value, -datetime) %>%
  mutate(expectedValue = as.integer(factor(variable))) 

colors <- c("salmon")

gg <- ggplot(SoH_tidy, aes(datetime, value)) +
  geom_line(aes(x = SoH_tidy$datetime, y = SoH_tidy$expectedValue),  color = colors, alpha = 0.8) +
  geom_line(aes(x = dummy_tidy$datetime, y = dummy_tidy$value, color = "black"), alpha = 0)+
  geom_line() +
  scale_y_continuous(breaks=scales::pretty_breaks(3)) +
  facet_wrap(vars(variable), ncol = 1, strip.position = c("top"), scales = "free_y") +
  labs(title = paste0("State of Health - ", station_name)) +
  theme(legend.position = "none")
gg

# labels <- c("humidity %reporting", "pm25 A %DC", "pm25 A %reporting", "pm25 A/B correlation",
#             "pm25 A/B intercept", "pm25 A/B slope", "pm25 B %DC", "pm25 B %reporting",
#             "temp/humidity correlation", "temp %reporting")

#theme(panel.background = element_rect(fill = "white"))
# theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(color = "lightgray"))
#element_text(hjust = 0.25)


# dplyr::mutate(pm25_A_pctReporting = pm25_A_pctReporting/max(pm25_A_pctReporting, na.rm = TRUE)) %>%
# dplyr::mutate(pm25_B_pctReporting = pm25_B_pctReporting/max(pm25_B_pctReporting, na.rm = TRUE)) %>%
# dplyr::mutate(temperature_pctReporting = temperature_pctReporting/max(temperature_pctReporting, na.rm = TRUE)) %>%
# dplyr::mutate(humidity_pctReporting = humidity_pctReporting/max(humidity_pctReporting, na.rm = TRUE)) %>%
# dplyr::mutate(pm25_A_pm25_B_cor = pm25_A_pm25_B_cor/max(abs(pm25_A_pm25_B_cor), na.rm = TRUE)) %>%
# dplyr::mutate(pm25_A_pm25_B_slope = pm25_A_pm25_B_slope/max(abs(pm25_A_pm25_B_slope), na.rm = TRUE)) %>%
# #dplyr::mutate(pm25_A_pm25_B_intercept = pm25_A_pm25_B_intercept/max(abs(pm25_A_pm25_B_intercept), na.rm = TRUE)) %>%
# dplyr::mutate(temperature_humidity_cor = temperature_humidity_cor/max(abs(temperature_humidity_cor), na.rm = TRUE)) %>%
# #dplyr::mutate(pm25_A_pctDC = 100-pm25_A_pctDC) %>%
# #dplyr::mutate(pm25_B_pctDC = 100-pm25_B_pctDC) %>%
# dplyr::mutate(pm25_A_pctDC = pm25_A_pctDC/max(pm25_A_pctDC, na.rm = TRUE)) %>%
# dplyr::mutate(pm25_B_pctDC = pm25_B_pctDC/max(pm25_B_pctDC, na.rm = TRUE))
