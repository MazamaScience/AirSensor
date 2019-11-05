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
pat <- pat_load("MV Clean Air Ambassador @ Willowbrook Farm", startdate = 20190101, enddate = 20191023) #summer gap
pat <- pat_load("Spokane South Hill #1", startdate = 20190101, enddate = 20191023) #super clean station

# ------ data from California from Oct 2017 - South Coast -----------

setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
ca <- pas_load() %>% pas_filter(stateCode == "CA")
scaqmd <- ca %>% pas_filter(stringr::str_detect(label, "SC.._"))
pas_leaflet(scaqmd)  
pat <- pat_load("SCPR_05", startdate = 20190101, enddate = 20191023)

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
# d <- pat_sub$data 
# quick look at the pat
pat_multiplot(pat_sub)

#SoH <- pat_dailyStateOfHealth(pat) #calculate the daily SoH of a pat

normalize <- function (x, na.rm = TRUE) (x/(max(x, na.rm = TRUE)))

#pull out the indicator SoH columns and normalize them to from [-1, 1]
SoH_sub_normalized <- dplyr::select(SoH, "datetime", contains("_pctReporting"), "pm25_A_pm25_B_rsquared", "pm25_A_pm25_B_slope",
                                    "pm25_A_pm25_B_intercept", "pm25_A_pctDC", "pm25_B_pctDC", "humidity_pctValid", "temperature_pctValid",
                                    "pm25_A_temperature_cor", "pm25_B_temperature_cor") %>%
  dplyr::mutate_if(is.numeric, normalize)

SoH_tidy <-
  SoH_sub_normalized %>%
  tidyr::gather(variable, value, -datetime) %>%
  mutate(id = as.integer(factor(variable))) 

# SoH_tidy <-  ifelse(grepl("_pctReporting", SoH_tidy$variable), 1)
SoH_tidy <- 
  SoH_tidy %>%
  mutate(id = case_when(grepl("_pctReporting", SoH_tidy$variable) ~ 1,
                        grepl("pm25_A_pm25_B_rsquared", SoH_tidy$variable) ~ 1,
                        grepl("pm25_A_pm25_B_slope", SoH_tidy$variable) ~ 1,
                        grepl("pm25_A_pm25_intercept", SoH_tidy$variable) ~ -1,
                        grepl("_pctDC", SoH_tidy$variable) ~ -0,
                        grepl("_pctValid", SoH_tidy$variable) ~ 1,
                        grepl("pm25_A_temperature_cor", SoH_tidy$variable) ~ 0,
                        grepl("pm25_B_temperature_cor", SoH_tidy$variable) ~ 0)) #%>%
#arrange(id, variable, datetime)
SoH_tidy <- reorder(SoH_tidy$id, SoH_tidy$variable)




SoH_tidy$id <- factor(SoH_tidy$id, levels = c("-1", "0", "1"))
colors <- c("deeppink3", "deepskyblue4", "springgreen4")
#color <- scale_color_manual(name="colors", values=c("cyan", "purple", "magenta"), labels=c("-1", "0", "1"))
gg <- ggplot(SoH_tidy, aes(datetime, value)) +
  geom_line(aes(x = .data$datetime, y = .data$id, color = factor(id, levels = c("-1", "0", "1")), alpha = 0.9)) +
  scale_colour_manual(values=colors) +
  geom_line() +
  facet_wrap(vars(variable), nrow = 10, strip.position = c("top")) +
  scale_y_continuous(breaks = seq(-1, 1, by=1), limits=c(-1,1)) +
  labs(title = paste0("State of Health - ", station_name)) +
  labs(color="Expected \nValue")
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
