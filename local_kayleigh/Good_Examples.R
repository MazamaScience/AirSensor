
# Good Example of how to use mutate_if:
SoH <- get(load("/Users/kayleigh/Projects/sensor-data-ingest-v1/output/SoH/2019/SoH_SCAH_29_2019.rda"))
normalize <- function (x, na.rm = TRUE) (x/(max(x, na.rm = TRUE)))

SoH_sub_normalized <- dplyr::select(SoH, "datetime", 
                                    contains("_pctReporting"),
                                    "pm25_A_pctValid",
                                    "pm25_B_pctValid",
                                    "pm25_A_pctDC", 
                                    "pm25_B_pctDC", 
                                    "pm25_A_pm25_B_slope",
                                    "pm25_A_pm25_B_intercept",
                                    "pm25_A_pm25_B_rsquared",
                                    "pm25_A_temperature_cor",
                                    "pm25_B_temperature_cor") %>%
  dplyr::mutate_if(is.numeric, normalize)

# How many are NA or not NA?
sum(is.na(pat$data$pm25_A))

sum(!is.na(pat$data$pm25_A))

# good example using case_when and logic:
SoH <- pat_dailySoH(pat)
SoH <-
  SoH %>%
  dplyr::mutate(SoH_index = case_when(
    pm25_A_pctReporting <= 10 & pm25_B_pctReporting <= 10 ~ 0, # case 1 - dead
    pm25_A_pm25_B_rsquared <= 0.05 ~ 0,
    pm25_A_pm25_B_rsquared <= 0.5 & pm25_A_temperature_rsquared >= 0.25 ~ 0.25,
    pm25_A_pctReporting >= 95 & pm25_B_pctReporting >= 95 &
      temperature_pctReporting <= 10 & humidity_pctReporting <= 10 ~ 0.75,
    pm25_A_pctReporting >= 95 & pm25_A_pctReporting >= 95 &
      pm25_A_pm25_B_rsquared >= 0.6 ~ 1, # case 2 - great

    TRUE ~ as.numeric(.5))) #the "else" condition, make average.

index_tbl$SoH_index <- SoH$SoH_index

# good example using colors as factors
index <- pat_dailySoHIndex(pat)
station_name <- pat$meta$label
cols <- brewer.pal(5, "RdYlGn")
colors <- factor(index$SoH_index)
gg <- ggplot(pat$data) +
  geom_point(aes(pat$data$datetime, pat$data$pm25_A), color= "red",pch = 16, cex = 0.5, alpha = 0.5) +
  geom_point(aes(pat$data$datetime, pat$data$pm25_B), color= "blue", pch = 16, cex = 0.5, alpha = 0.5) +
  geom_point(data = index, aes(index$datetime, (index$SoH_index-18), color = colors), pch = 15, cex =5) +
  scale_color_manual(values=c("0" = cols[1], "0.25" = cols[2], "0.5" = cols[3], "0.75" = cols[4], "1" = cols[5])) +
  labs(title = paste0("SoH Index - ", station_name)) 

gg




