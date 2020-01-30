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

