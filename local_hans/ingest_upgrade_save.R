# Find pas in one directory and update saving to another

# Set the out-of-date directory 
old_pas_dir <- '~/Downloads/old_pas_db'

# Set the upgraded pas directory
new_pas_dir <- '~/Downloads/new_pas_db'

# Sequential upgrade
for ( i in list.files(old_pas_dir) ) {
  old_pas <- MazamaCoreUtils::loadDataFile(i, dataDir = old_pas_dir)
  pas <- pas_upgrade(old_pas)
  new_fn <- paste0(new_pas_dir, '/', stringr::str_extract(i, '^pas_\\d{8}'), '_v2.rda')
  save(pas, file = new_fn)
}

# parallel upgrade
library(foreach)
cl <- parallel::makeCluster(parallel::detectCores() - 1)
doParallel::registerDoParallel(cl)

foreach(i = list.files(old_pas_dir)) %dopar% {
  old_pas <- MazamaCoreUtils::loadDataFile(i, dataDir = old_pas_dir)
  pas <- AirSensor::pas_upgrade(old_pas)
  new_fn <- paste0(new_pas_dir, '/', stringr::str_extract(i, '^pas_\\d{8}'), '_v2.rda')
  save(pas, file = new_fn)
  paste0(new_fn, ' saved...')
}

parallel::stopCluster(cl)