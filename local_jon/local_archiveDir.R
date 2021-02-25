# Demonstration of a local archive for the Methow Valley for Sep-Nov, 2020

library(AirSensor)

# ----- Get the PAS and then unset 'archiveBaseUrl' ----------------------------

# Get the latest PAS from the URL archive
setArchiveBaseUrl("http://data.mazamascience.com/PurpleAir/v1")
pas <- pas_load()

# Look at and then remove the package-wide setting of the 'baseUrl'
getArchiveBaseUrl()
removeArchiveBaseUrl()
getArchiveBaseUrl()

# ----- Create the "well known structure" under 'archiveBaseDir' ---------------

# OK. Now we need to create our own `baseDir`
baseDir <- "~/Data/my_archive"
dir.create(baseaDir, recursive = TRUE)
setArchiveBaseDir(baseDir)

# Create the "well known structure" for the files we need
# PAS
dir.create(file.path(baseDir, "/pas/2021/02"), recursive = TRUE) # for latest PAS
# PAT
dir.create(file.path(baseDir, "/pat/2020/09"), recursive = TRUE)
dir.create(file.path(baseDir, "/pat/2020/10"), recursive = TRUE)
dir.create(file.path(baseDir, "/pat/2020/11"), recursive = TRUE)

# ----- Populate the local archive with a PAS object ---------------------------                          

# Methow Valley PAS
pas <- 
  pas %>% 
  pas_filter(stringr::str_detect(label, "MV Clean Air Ambassador"))

# Looks good!
pas_leaflet(pas)

# Use today's datestamp = "20210225"
save(pas, file = file.path(baseDir, "pas/2021/pas_20210225.rda"))

# ----- Populate the local archive with a PAT object ---------------------------                          

id <- "ab5dca99422f2c0d_13669"     # "Balky Hill" sensor

pat <- pat_createNew(
  id = id, 
  pas = pas, 
  startdate = 20200901, 
  enddate = 20201001, 
  timezone = "America/Los_Angeles"
)

# Use appropriate monthstamp = "202009"
fileName <- paste0("pat_", id, "_202009.rda")
filePath <- file.path(baseDir, "/pat/2020/09", fileName)
save(pat, file = filePath)

# ----- Now test to see if we get it -------------------------------------------

# Clean out the environment
rm(list = ls())

# baseDir should be remembered but baseUrl should be NULL
getArchiveBaseDir()
getArchiveBaseUrl()

pas <- pas_load(20210225)

BalkyHill <- pat_load(
  id = "ab5dca99422f2c0d_13669", 
  pas = pas, 
  startdate = 20200901, 
  enddate = 20201001, 
  timezone = "America/Los_Angeles"
)

pat_multiplot(BalkyHill)


