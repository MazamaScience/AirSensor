library(AirSensor)

# ===== PWFSL 2019 for OR, WA ==================================================

setArchiveBaseUrl("https://airfire-data-exports.s3-us-west-2.amazonaws.com/PurpleAir/v1")
pas <- pas_load()

# ----- Super clean station ----------------------------------------------------

pat_South_Hill <- pat_load("Spokane South Hill #1", startdate = 20190101, enddate = 20191023) #super clean station
pat_multiplot(pat_South_Hill)
pat_South_Hill %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

# Daily State-of-Health metrics
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

# Daily State-of-Health metrics
SoH_Willowbrook <- pat_dailyStateOfHealth(pat_Willowbrook)

SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pctReporting", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pctValid", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pctDC", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "_cor", style = "point", autoRange = FALSE)
SoH_Willowbrook %>% timeseriesTbl_multiplot(parameterPattern = "pm25_A_pm25_B", style = "point", autoRange = TRUE)


# ===== Mazama Science 2019 for SCAQMD =========================================
setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
pas <- pas_load()

# ----- B-channel DC -----------------------------------------------------------

pat_SCAP_46 <- pat_load("SCAP_46", startdate = 20190101, enddate = 20191023)
pat_multiplot(pat_SCAP_46)
pat_SCAP_46 %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

# Daily State-of-Health metrics
SoH_SCAP_46 <- pat_dailyStateOfHealth(pat_SCAP_46)

SoH_SCAP_46 %>% timeseriesTbl_multiplot(parameterPattern = "pctReporting", style = "point", autoRange = FALSE)
SoH_SCAP_46 %>% timeseriesTbl_multiplot(parameterPattern = "pctValid", style = "point", autoRange = FALSE)
SoH_SCAP_46 %>% timeseriesTbl_multiplot(parameterPattern = "pctDC", style = "point", autoRange = FALSE)
SoH_SCAP_46 %>% timeseriesTbl_multiplot(parameterPattern = "_cor", style = "point", autoRange = FALSE)
SoH_SCAP_46 %>% timeseriesTbl_multiplot(parameterPattern = "pm25_A_pm25_B", style = "point", autoRange = TRUE)

# ----- A-channel "magic number" -----------------------------------------------

pat_SCEM_05 <- pat_load("SCEM_05", startdate = 20190101, enddate = 20191023)
pat_multiplot(pat_SCEM_05)
pat_SCEM_05 %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

# Daily State-of-Health metrics
SoH_SCEM_05 <- pat_dailyStateOfHealth(pat_SCEM_05)

SoH_SCEM_05 %>% timeseriesTbl_multiplot(parameterPattern = "pctReporting", style = "point", autoRange = FALSE)
SoH_SCEM_05 %>% timeseriesTbl_multiplot(parameterPattern = "pctValid", style = "point", autoRange = FALSE)
SoH_SCEM_05 %>% timeseriesTbl_multiplot(parameterPattern = "pctDC", style = "point", autoRange = FALSE)
SoH_SCEM_05 %>% timeseriesTbl_multiplot(parameterPattern = "_cor", style = "point", autoRange = FALSE)
SoH_SCEM_05 %>% timeseriesTbl_multiplot(parameterPattern = "pm25_A_pm25_B", style = "point", autoRange = TRUE)

# ----- A-channel moderate errors ----------------------------------------------

pat_SCNP_20 <- pat_load("SCNP_20", startdate = 20190101, enddate = 20191023)
pat_multiplot(pat_SCNP_20)
pat_SCNP_20 %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

# Daily State-of-Health metrics
SoH_SCNP_20 <- pat_dailyStateOfHealth(pat_SCNP_20)

SoH_SCNP_20 %>% timeseriesTbl_multiplot(parameterPattern = "pctReporting", style = "point", autoRange = FALSE)
SoH_SCNP_20 %>% timeseriesTbl_multiplot(parameterPattern = "pctValid", style = "point", autoRange = FALSE)
SoH_SCNP_20 %>% timeseriesTbl_multiplot(parameterPattern = "pctDC", style = "point", autoRange = FALSE)
SoH_SCNP_20 %>% timeseriesTbl_multiplot(parameterPattern = "_cor", style = "point", autoRange = FALSE)
SoH_SCNP_20 %>% timeseriesTbl_multiplot(parameterPattern = "pm25_A_pm25_B", style = "point", autoRange = TRUE)

# ----- A-channel extreme noise ------------------------------------------------

pat_SCAP_14 <- pat_load("SCAP_14", startdate = 20190101, enddate = 20191023)
pat_multiplot(pat_SCAP_14)
pat_SCAP_14 %>% pat_filterDate(20190909, 20190912) %>% pat_multiplot()

# Daily State-of-Health metrics
SoH_SCAP_14 <- pat_dailyStateOfHealth(pat_SCAP_14)

SoH_SCAP_14 %>% timeseriesTbl_multiplot(parameterPattern = "pctReporting", style = "point", autoRange = FALSE)
SoH_SCAP_14 %>% timeseriesTbl_multiplot(parameterPattern = "pctValid", style = "point", autoRange = FALSE)
SoH_SCAP_14 %>% timeseriesTbl_multiplot(parameterPattern = "pctDC", style = "point", autoRange = FALSE)
SoH_SCAP_14 %>% timeseriesTbl_multiplot(parameterPattern = "_cor", style = "point", autoRange = FALSE)
SoH_SCAP_14 %>% timeseriesTbl_multiplot(parameterPattern = "pm25_A_pm25_B", style = "point", autoRange = TRUE)


