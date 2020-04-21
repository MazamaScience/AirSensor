# Find pas in one directory and update saving to another
library(AirSensor)

# Set the out-of-date directory 
oldDir <- '~/Downloads/old_pas_db'

# Set the upgraded pas directory
newDir <- '~/Downloads/new_pas_db/pas/2019'

# Sequential upgrade
for ( file in list.files(oldDir) ) {
  old_pas <- MazamaCoreUtils::loadDataFile(file, dataDir = oldDir)
  pas <- pas_upgrade(old_pas)
  save(pas, file = file.path(newDir, file))
}

# parallel upgrade
library(foreach)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)

foreach(i = list.files(oldDir)) %dopar% {
  old_pas <- MazamaCoreUtils::loadDataFile(i, dataDir = oldDir)
  pas <- AirSensor::pas_upgrade(old_pas)
  new_fn <- paste0(newDir, '/', stringr::str_extract(i, '^pas_\\d{8}'), '_v2.rda')
  save(pas, file = new_fn)
  paste0(new_fn, ' saved...')
}

parallel::stopCluster(cl)