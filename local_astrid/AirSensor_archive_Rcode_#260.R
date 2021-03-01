###################################################################
# Author: Astrid Sanna
# Date: 2/23/2021
# Last update: 2/25/2021
# Object: Creating a Data Archive (#260)
###################################################################
devtools::install_github("MazamaScience/AirSensor")
library(MazamaCoreUtils)
library(AirSensor)
library(dplyr)
rm(list = ls())
pas_ex <- AirSensor::example_pas
id <- pas_getDeviceDeploymentIDs(pas_ex, "^Seattle$")

# ----- STEP 1: pas_load MVCCA and save it locally
setArchiveBaseDir(NULL)
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1/")
mvcaa =
  pas_load() %>% # Load the most recent archived 'pas'(PurpleAir Synoptic)
  pas_filter(stringr::str_detect(label, "MV Clean Air Ambassador"))
head(mvcaa)
?str_detect
pas_leaflet(mvcaa)

#test --- can we use this as shortcut if we are not intrested in the metadata?
pas_t = pas_load() %>%
  pas_getDeviceDeploymentIDs("MV Clean Air Ambassador")


# to save objects as files - save(pas, file = "FILE_PATH")
save(mvcaa, file = paste0(my_archive_dir, "/pas/2021/pas_20210225.rda"))

# ----- STEP 2: use setArchiveBaseDir()
setArchiveBaseDir("C:/Users/astri/Mirror/Mazamascience/Data/AirSensor/my_archive/")
getArchiveBaseDir() #OK

# ----- STEP 3: get list of deviceDeploymentIDs for MVCAA sensors
pas = get(load(paste0(my_archive_dir, "/pas/2021/pas_20210225.rda")))
ID =  pas_getDeviceDeploymentIDs(pas = pas)
# id <- pas_getDeviceDeploymentIDs(pas, "^Seattle$")
# output:
# [1] "ab5dca99422f2c0d_13669" "f6c44edd41c941c7_10182" "49215ad49d1a87e3_10188"
# [4] "f736fd3fb21fc4da_13667" "db5d6b3b79f5830e_39237" "4f19d256e1787973_10166"
# [7] "f592adb5067ad9d3_13675" "4a47b9252e16e558_15077" "0cbfeb2ce4c1553c_13661"
# [10] "2e3b5ceea86a885b_10168" "f96deab8c29aa42b_10134" "96b108298883ca47_64441"
# [13] "b56c0ef677852913_81495"

# ----- STEP 4: in a for loop, use pat_createNew() create 'pat' objects for
# each sensor. I think it will get the last week if you don't specify startdate
# and enddate

ID_test = c("ab5dca99422f2c0d_13669","f6c44edd41c941c7_10182")

# try a test of 2 IDs
for (id in ID_test){
  filename = paste0(local_dir, "/", id, "_202009", ".rda")
  pat = pat_createNew(id = id,
                      pas = mvcaa,
                      startdate = 20200901,
                      enddate = 20200930)
  save(pat, file = filename)
}

# ----- STEP 5: create my_archive in "well-known" structure for Fall (Sep-Nov)
# and save pat files there
my_archive_dir = "C:/Users/astri/Mirror/Mazamascience/Data/AirSensor/my_archive"
pat = "pat/2020/09"
pat_10 = "pat/2020/10"
pat_11 = "pat/2020/11"
dir.create(file.path(my_archive_dir, pat), recursive = TRUE)
dir.create(file.path(my_archive_dir, pat_10), recursive = TRUE)
dir.create(file.path(my_archive_dir, pat_11), recursive = TRUE)
dir.create(file.path(my_archive_dir, "/pas/2021"), recursive = TRUE)

# save the new directory as an object
pat_202009 = paste0(my_archive_dir,"/", pat)
pat_202010 = paste0(my_archive_dir,"/", pat_10)
pat_202011 = paste0(my_archive_dir,"/", pat_11)

# try a test of 2 IDs saved in my_archive
setArchiveBaseDir(NULL)
for (id in ID_test){
  filename = paste0(pat_202009, "/", "pat_", id, "_202009", ".rda")
  pat = pat_createNew(id = id,
                      pas = mvcaa,
                      startdate = 20200901,
                      enddate = 20200930)
  save(pat, file = filename)
}

# delete files in my_archive/pat/2020/09 and repeat loop for all sensors

# september
ID =  pas_getDeviceDeploymentIDs(pas = mvcaa)
for (id in ID){
  filename = paste0(pat_202009, "/", "pat_", id, "_202009", ".rda")
  pat = pat_createNew(id = id,
                      pas = mvcaa,
                      startdate = 20200901,
                      enddate = 20200930)
  save(pat, file = filename)
}
# the pat for "b56c0ef677852913_81495" was not created because
# "No data available for the time period you asked for"
# Error in pat_filterDatetime(., startdate = dateRange[1], enddate = dateRange[2],  :
# Parameter 'pat' has no data.

pat_81495 = pat_createNew(id = "b56c0ef677852913_81495",
                          pas = mvcaa,
                          startdate = 20200901,
                          enddate = 20200930)
# Error in pat_filterDatetime(., startdate = dateRange[1], enddate = dateRange[2],  :
# Parameter 'pat' has no data.
# ADD TO ISSUE 261 https://github.com/MazamaScience/AirSensor/issues/261.

# october
ID =  pas_getDeviceDeploymentIDs(pas = mvcaa)
for (id in ID){
  filename = paste0(pat_202010, "/", "pat_", id, "_202010", ".rda")
  pat = pat_createNew(id = id,
                      pas = mvcaa,
                      startdate = 20201001,
                      enddate = 20201031)
  save(pat, file = filename)
}
# november
for (id in ID){
  filename = paste0(pat_202011, "/", "pat_", id, "_202011", ".rda")
  pat = pat_createNew(id = id,
                      pas = mvcaa,
                      startdate = 20201101,
                      enddate = 20201130)
  save(pat, file = filename)
}


# ----- STEP 6: load data (trying different approaches)
setArchiveBaseDir(my_archive_dir)
getArchiveBaseDir()
pas <- pas_load(20210225)
pat <- pat_load(id = "ab5dca99422f2c0d_13669",
         pas = pas,
         startdate = 20201101,
         enddate = 20201130,
         timezone = "America/Los_Angeles")
pat_multiplot(pat)

pat_test <- pat_load(id = "ab5dca99422f2c0d_13669",
                pas = pas) #errors. it needs a date!!!

pat_test2 <- pat_load(id = "ab5dca99422f2c0d_13669",
                startdate = 20200901,
                enddate = 20200930,
                timezone = "America/Los_Angeles")
pat_multiplot(pat)




