# Single state statistics
# Kayleigh Wilson & Jonathan Callahan

# This top portion of this script has several sections that loop through each month to complete 
# various tasks including gathering all pat objects into a monthly list, calculating
# an SoH object for each of those pat objects, calculating SoH Indices, forming lists
# and tibbles. Some of these sections can take quite a while to run so the scripts first
# check to see if the data exist locally and will load/save data accordingly.
# The bottom portion of this script deals with plotting the data

# if ( FALSE ) {
#   
#   library(dplyr)
#   library(reshape2)
#   library(MazamaCoreUtils)
#   library(AirSensor)
#   
#   createLocalArchive(
#     year = 2019,
#     stateCode = "WA",
#     pattern = "^MV Clean Air Ambassador @ B",
#     collection = "MVCAA_B",
#     baseDir = path.expand("~/Data/PA_networks"),
#     verbose = TRUE
#   )  
#   
# }

library(ggplot2)
library(rlang)

#-------------------- Plots -----------------------------------

#### box plot: distribution of single metric from single month

metricMonthBoxplot <- function(
  SoH = NULL,
  parameter = "pm25_A_pctReporting",
  FUN = mean
) {
  
  # NOTE:  Couldn't get this to work with '!!parameter'
  
  ggplot(SoH) +
    aes(x = reorder(SoH[["deviceDeploymentIDs"]], SoH[[parameter]], FUN),
        y = pm25_A_pctReporting) +
    geom_boxplot() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    geom_rug(length = unit(0.01, "npc"))
  
}

# #### SoH index histogram for one month: 
# 
# soh_Indextibble <- nov_sohIndextibble
# startdate <- "20191101"
# enddate <- "20191201"
# 
# categorized_index <- 
#   soh_Indextibble %>%
#   dplyr::mutate(category = dplyr::case_when(index_bin == 1 ~ "poor",
#                                             index_bin == 2 ~ "fair",
#                                             index_bin == 3 ~ "good")) %>%
#   dplyr::filter(.data$datetime >= parseDatetime(datetime = startdate, timezone = "America/Los_Angeles")) %>%
#   dplyr::filter(.data$datetime < parseDatetime(datetime = enddate, timezone = "America/Los_Angeles")) 
# 
# 
# percent_index <- 
#   categorized_index %>%
#   dplyr::group_by(.data$datetime, .data$index_bin) %>%
#   dplyr::summarise_at(
#     .vars = c("category"),
#     .funs = function(x) { length(na.omit(x)) }
#   ) %>% #this is awesome, make a new column to store the total for each day, populate with the sum calculated based on when 
#   #the date is identical
#   dplyr::mutate(totals = dplyr::case_when(identical(.data$datetime, .data$datetime) ~ sum(.data$category))) %>%
#   dplyr::mutate(percent = .data$category/.data$totals*100) %>%
#   dplyr::mutate(group = dplyr::case_when(index_bin == 1 ~ "poor",
#                                          index_bin == 2 ~ "fair",
#                                          index_bin == 3 ~ "good"))
# 
# 
# #colors <- c("firebrick", "goldenrod1", "seagreen3")
# #fillColors <- colors[percent_index$percent]
# colors <- c("good" = "seagreen3", "fair" = "goldenrod1", "poor" = "firebrick" )
# 
# ordered_categories <- c("good", "fair", "poor")
# percent_index$group <- factor(percent_index$group, levels = ordered_categories, order = TRUE)
# 
# ggplot(percent_index, aes(x = datetime, y = percent, fill = group ) ) +
#   geom_bar(stat = "identity") +
#   scale_fill_manual(values = colors)
# 
# 
# 
# 
# 
# 
# #### box plot: one metric for all mons
# 
# soh_metric <- "temperature_pctReporting"
# 
# monthly_metric <-
#   jan_sohMeanstibble %>%
#   dplyr::bind_rows(feb_sohMeanstibble) %>%
#   dplyr::bind_rows(mar_sohMeanstibble) %>%
#   dplyr::bind_rows(apr_sohMeanstibble) %>%
#   dplyr::bind_rows(may_sohMeanstibble) %>%
#   dplyr::bind_rows(jun_sohMeanstibble) %>%
#   dplyr::bind_rows(jul_sohMeanstibble) %>%
#   dplyr::bind_rows(aug_sohMeanstibble) %>%
#   dplyr::bind_rows(sep_sohMeanstibble) %>%
#   dplyr::bind_rows(oct_sohMeanstibble) %>%
#   dplyr::bind_rows(nov_sohMeanstibble) %>%
#   dplyr::select(soh_metric, "datetime") %>%
#   dplyr::mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))
# 
# ggplot(monthly_metric) +
#   geom_boxplot(aes(x = datetime, y = eval(rlang::parse_expr(soh_metric))))
# 
# 
# #### percent reporting all mons ridgeline plot:
# 
# ggplot(monthly_metric, aes(x = eval(rlang::parse_expr(soh_metric)), y = datetime, fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_gradient2(low = "firebrick", high = "seagreen3", midpoint = 50) +
#   labs(title = eval(soh_metric)) +
#   xlab(label = eval(soh_metric)) +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   )
# 
# #### SoH index ridgeline plot
# startdate <- "20190101"
# enddate <- "20191201"
# 
# allmons_sohIndex <-
#   jan_sohIndextibble %>%
#   dplyr::bind_rows(feb_sohIndextibble) %>%
#   dplyr::bind_rows(mar_sohIndextibble) %>%
#   dplyr::bind_rows(apr_sohIndextibble) %>%
#   dplyr::bind_rows(may_sohIndextibble) %>%
#   dplyr::bind_rows(jun_sohIndextibble) %>%
#   dplyr::bind_rows(jul_sohIndextibble) %>%
#   dplyr::bind_rows(aug_sohIndextibble) %>%
#   dplyr::bind_rows(sep_sohIndextibble) %>%
#   dplyr::bind_rows(oct_sohIndextibble) %>%
#   dplyr::bind_rows(nov_sohIndextibble) %>%
#   dplyr::filter(.data$datetime >= parseDatetime(datetime = startdate, timezone = "America/Los_Angeles")) %>%
#   dplyr::filter(.data$datetime < parseDatetime(datetime = enddate, timezone = "America/Los_Angeles")) %>%
#   dplyr::mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC")) 
# 
# breaks = c(0, .2, .8, 1)
# ggplot(allmons_sohIndex, aes(x = index, y = datetime, fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_gradient2(low = "firebrick", mid ="goldenrod1",  high = "seagreen3", midpoint = 0.4, breaks = breaks) +
#   labs(title = "SoH Index") +
#   xlab(label = "SoH Index") +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   )
# 
# 
# ##### box plot: average of each metric for single month
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
# tidySoHMeans <- jan_sohMeansTidy
# tidySoHMeans$variable <- factor(tidySoHMeans$variable, levels = orderedParams, order = TRUE)
# 
# ggplot(tidySoHMeans) +
#   geom_boxplot(aes(x = variable, y = value)) +
#   coord_flip() +
#   labs(title = "January")
# 
# 
# ##### box plot: single month metrics on scale from 0:150 percent
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
#   "humidity_pctDC"
# )
# 
# tidySoHMeans <- jan_sohMeansTidy
# 
# tidySoHMeans <-
#   tidySoHMeans %>%
#   dplyr::filter(variable == "pm25_A_pctReporting"
#                 | variable ==  "pm25_B_pctReporting"
#                 | variable == "temperature_pctReporting"
#                 | variable == "humidity_pctReporting"
#                 | variable == "pm25_A_pctValid"
#                 | variable == "pm25_B_pctValid"
#                 | variable == "temperature_pctValid"
#                 | variable == "humidity_pctValid"
#                 | variable == "pm25_A_pctDC"
#                 | variable == "pm25_B_pctDC"
#                 | variable == "temperature_pctDC"
#                 | variable == "humidity_pctDC")
# 
# tidySoHMeans$variable <- factor(tidySoHMeans$variable, levels = orderedParams, order = TRUE)
# 
# ggplot(tidySoHMeans) +
#   geom_boxplot(aes(x = variable, y = value)) +
#   coord_flip() +
#   labs(title = "January")
# 
# 
# 
# ##### box plot: single month metrics on smaller scale (ie, NOT 0-150%)
# 
# orderedParams <- c(
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
# tidySoHMeans <- jan_sohMeansTidy
# 
# tidySoHMeans <-
#   tidySoHMeans %>%
#   dplyr::filter(variable == "pm25_A_pm25_B_rsquared"
#                 | variable ==  "pm25_A_pm25_B_slope"
#                 | variable == "pm25_A_pm25_B_intercept"
#                 | variable == "pm25_A_pm25_B_p_value"
#                 | variable == "pm25_A_humidity_rsquared"
#                 | variable == "pm25_A_temperature_rsquared"
#                 | variable == "pm25_B_humidity_rsquared"
#                 | variable == "pm25_B_temperature_rsquared")
# 
# tidySoHMeans$variable <- factor(tidySoHMeans$variable, levels = orderedParams, order = TRUE)
# 
# ggplot(tidySoHMeans) +
#   geom_boxplot(aes(x = variable, y = value)) +
#   coord_flip() +
#   labs(title = "January")
# 
# 
# 
# 
# ##### box plot: all metrics, all mons, aka chaos:
# 
# # jan_all_metrics <-
# #   tidyTbl %>%
# #   dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(datetime = "201901", timezone = "UTC")) %>%
# #   mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))
# # feb_all_metrics <-
# #   feb_tidyTbl %>%
# #   dplyr::mutate(datetime = MazamaCoreUtils::parseDatetime(datetime = "201902", timezone = "UTC")) %>%
# #   mutate(datetime = strftime(datetime, format = "%Y/%m", tz = "UTC"))
# # 
# # monthly_all_metrics <-
# #   jan_all_metrics %>%
# #   dplyr::bind_rows(feb_all_metrics)
# # 
# # orderedParams <- c(
# #   "pm25_A_pctReporting",
# #   "pm25_B_pctReporting",
# #   "temperature_pctReporting",
# #   "humidity_pctReporting",
# #   "pm25_A_pctValid",
# #   "pm25_B_pctValid",
# #   "temperature_pctValid",
# #   "humidity_pctValid",
# #   "pm25_A_pctDC",
# #   "pm25_B_pctDC",
# #   "temperature_pctDC",
# #   "humidity_pctDC",
# #   "pm25_A_pm25_B_rsquared",
# #   "pm25_A_pm25_B_slope",
# #   "pm25_A_pm25_B_intercept",
# #   "pm25_A_pm25_B_p_value",
# #   "pm25_A_humidity_rsquared",
# #   "pm25_A_temperature_rsquared",
# #   "pm25_B_humidity_rsquared",
# #   "pm25_B_temperature_rsquared"
# # )
# # 
# # monthly_all_metrics$variable <- factor(monthly_all_metrics$variable, levels = orderedParams, order = TRUE)
# # 
# # ggplot(monthly_all_metrics) +
# #   geom_boxplot(aes(x = variable, y = value)) +
# #   coord_flip()+
# #   facet_grid(cols = vars(monthly_all_metrics$datetime))
# 
# 
