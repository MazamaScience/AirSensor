
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




