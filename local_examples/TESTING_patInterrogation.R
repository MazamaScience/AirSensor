# Interrogating data generated with PROTOTYPE_pat_communityArchive.R

# load some data
scnp_14 <- pat_loadMonth("SCNP_14", 20190401)
scnp_15 <- pat_loadMonth("SCNP_15", 20190401)
scnp_17 <- pat_loadMonth("SCNP_17", 20190401)

# start asking questions
scnp_14 %>% pat_scatterplot()
scnp_14 %>% pat_multiplot()
scnp_14 %>% pat_internalFit()
scnp_14 %>% pat_outliers()

# Compare 14 and 15
scnp_14 %>% pat_filterDate("2019-04-15", "2019-04-21") %>% pat_outliers()
scnp_15 %>% pat_filterDate("2019-04-15", "2019-04-21") %>% pat_outliers()

# outliers
scnp_17 %>% 
  pat_filterDate("2019-04-15", "2019-04-21") %>% 
  pat_outliers()

scnp_17 %>% 
  pat_outliers(replace = TRUE, showPlot = FALSE) %>% 
  pat_filterDate("2019-04-15", "2019-04-21") %>% 
  pat_outliers()

scnp_17 %>% 
  pat_outliers(replace = TRUE, showPlot = FALSE) %>% 
  pat_filterDate("2019-04-15", "2019-04-21") %>% 
  pat_multiplot("pm25_over")

