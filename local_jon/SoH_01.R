library(AirSensor)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(forcats)
# library(stringr)
# library(purrr)
# library(plotly)
# library(gridExtra)
# library(skimr)

# PWFSL 2019 for OR, WA
setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

# ----- Super clean station ----------------------------------------------------

pat_South_Hill <- pat_load("Spokane South Hill #1", startdate = 20190101, enddate = 20191023) #super clean station
pat_multiplot(pat_South_Hill)
pat_South_Hill %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

SoH_South_Hill <- pat_dailyStateOfHealth(pat_South_Hill)

SoH_South_Hill %>% timeseriesTbl_multiplot(parameterPattern = "pctReporting", style = "point", autoRange = FALSE)
SoH_South_Hill %>% timeseriesTbl_multiplot(parameterPattern = "pctValid", style = "point", autoRange = FALSE)
SoH_South_Hill %>% timeseriesTbl_multiplot(parameterPattern = "pctDC", style = "point", autoRange = FALSE)
SoH_South_Hill %>% timeseriesTbl_multiplot(parameterPattern = "_cor", style = "point", autoRange = FALSE)
SoH_South_Hill %>% timeseriesTbl_multiplot(parameterPattern = "pm25_A_pm25_B", style = "point", autoRange = TRUE)


# ----- Summer gap and out-of-spec values --------------------------------------

pat_Willowbrook <- pat_load("MV Clean Air Ambassador @ Willowbrook Farm", startdate = 20190101, enddate = 20191101)
pat_multiplot(pat_Willowbrook)
pat_Willowbrook %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

SoH_Willowbrook <- pat_dailyStateOfHealth(pat_Willowbrook) #calculate the daily SoH of a pat

SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pctReporting", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pctValid", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pctDC", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "_cor", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pm25_A_pm25_B", style = "point", autoRange = TRUE)







pat <- pat_load("Spokane South Hill #1", startdate = 20190101, enddate = 20191023) #super clean station

# # ------ data from California from Oct 2017 - South Coast -----------
# 
# setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
# ca <- pas_load() %>% pas_filter(stateCode == "CA")
# scaqmd <- ca %>% pas_filter(stringr::str_detect(label, "SC.._"))
# pas_leaflet(scaqmd)  
# pat <- pat_load("SCPR_05", startdate = 20190101, enddate = 20191023)
# 
# # ------ data for all of WA/OR for 2019 ----------------
# # Also has recent data for all sensors in California.
# setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
# 
# # --------- Flat-lined plot ---------------------------------------------------
# 
# # wanted plots: all _pctReporting, A and B _pctDC, (?) all pctValid (?), 
# # A/B cor, temp/humd cor, A/B intercept and slope = 10 (plus valid if want)
# 
# pat_sub <- pat_sample(pat, keepOutliers = TRUE) #for viewing a subset of the pat data
# d <- pat_sub$data 
# 
# SoH <- pat_dailyStateOfHealth(pat) #calculate the daily SoH of a pat
# 
# #pull out the indicator SoH columns and normalize them to from [-1, 1]
# SoH_sub_normalized <- dplyr::select(SoH, contains("_pctReporting"), contains("pm25_A_pm25_B"),
#                                     "temperature_humidity_cor", "pm25_A_pctDC", "pm25_B_pctDC",
#                                     "datetime") %>%
#   dplyr::mutate(pm25_A_pctReporting = pm25_A_pctReporting/max(pm25_A_pctReporting, na.rm = TRUE)) %>%
#   dplyr::mutate(pm25_B_pctReporting = pm25_B_pctReporting/max(pm25_B_pctReporting, na.rm = TRUE)) %>%
#   dplyr::mutate(temperature_pctReporting = temperature_pctReporting/max(temperature_pctReporting, na.rm = TRUE)) %>%
#   dplyr::mutate(humidity_pctReporting = humidity_pctReporting/max(humidity_pctReporting, na.rm = TRUE)) %>%
#   dplyr::mutate(pm25_A_pm25_B_cor = pm25_A_pm25_B_cor/max(abs(pm25_A_pm25_B_cor), na.rm = TRUE)) %>%
#   dplyr::mutate(pm25_A_pm25_B_slope = pm25_A_pm25_B_slope/max(abs(pm25_A_pm25_B_slope), na.rm = TRUE)) %>%
#   dplyr::mutate(pm25_A_pm25_B_intercept = pm25_A_pm25_B_intercept/max(abs(pm25_A_pm25_B_intercept), na.rm = TRUE)) %>%
#   dplyr::mutate(temperature_humidity_cor = temperature_humidity_cor/max(abs(temperature_humidity_cor), na.rm = TRUE)) %>%
#   #dplyr::mutate(pm25_A_pctDC = 100-pm25_A_pctDC) %>%
#   #dplyr::mutate(pm25_B_pctDC = 100-pm25_B_pctDC) %>%
#   dplyr::mutate(pm25_A_pctDC = pm25_A_pctDC/max(pm25_A_pctDC, na.rm = TRUE)) %>%
#   dplyr::mutate(pm25_B_pctDC = pm25_B_pctDC/max(pm25_B_pctDC, na.rm = TRUE))
# 
# SoH_tidy <-
#   SoH_sub_normalized %>%
#   tidyr::gather(variable, value, -datetime) %>%
#   transform(id = as.integer(factor(variable)))
# 
# # labels <- c("humidity %reporting", "pm25 A %DC", "pm25 A %reporting", "pm25 A/B correlation",
# #             "pm25 A/B intercept", "pm25 A/B slope", "pm25 B %DC", "pm25 B %reporting",
# #             "temp/humidity correlation", "temp %reporting")
# 
# 
# gg <- ggplot(SoH_tidy, aes(datetime, value)) +
#   geom_line()+
#   facet_wrap(vars(variable), nrow = 10, strip.position = c("top")) +
#   scale_y_continuous(breaks = seq(-1, 1, by=1), limits=c(-1,1)) 
# #theme(panel.background = element_rect(fill = "white"))
# # theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(color = "lightgray"))
# #element_text(hjust = 0.25)
# gg


