
pat <- example_pat

compare <- 
  example_pat %>% 
  pat_filterDate(startdate = 20180801, enddate = 20180807) %>% 
  pat_outliers()

test2 <- 
  example_pat %>% 
  pat_filterDate(startdate = 20180801, enddate = 20180807) %>% 
  pat_multiplot(plottype = "pm25", sampleSize = 50)

test3 <- 
  example_pat %>% 
  pat_filterDate(startdate = 20180801, enddate = 20180807) %>% 
  pat_multiplot(plottype = "pm25", sampleSize = 500)

test3 <- 
  example_pat %>% 
  pat_filterDate(startdate = 20180801, enddate = 20180807) %>% 
  pat_multiplot(plottype = "pm25", sampleSize = 5000)
