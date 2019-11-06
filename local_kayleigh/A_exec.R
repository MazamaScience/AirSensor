#!/usr/local/bin/Rscript

# This Rscript will download the latest synoptic JSON file from Purple Air. 
#
# Test this script from the command line with:
#
# ./createPAS_exec.R
#
# 

#  --- . --- . MazamaCoreUtils 0.3.5
VERSION = "0.1.6"

# The following packages are attached here so they show up in the sessionInfo
suppressPackageStartupMessages({
  library(futile.logger)
  library(MazamaCoreUtils)
  library(AirSensor)
  
  setArchiveBaseUrl("http://smoke.mazamascience.com/data/PurpleAir")
})

# ----- Get command line arguments ---------------------------------------------

if ( interactive() ) {
  
  # RStudio session
  opt <- list(
    phrase = "cats",
    number = 9, 
    logDir = getwd()
  )  
  
} else {
  
  # Set up OptionParser
  library(optparse)

  option_list <- list(
    make_option(
      c("-p","--phrase"), 
      default="cats", 
      help="The best animal"
    ),
    make_option(
      c("-n", "--number"), 
      default=9, 
      help="how many lives?"
    ),
    make_option(
      c("-l","--logDir"), 
      default=getwd(), 
      help="Output directory for generated .log file [default=\"%default\"]"
    )
  )
  
  # Parse arguments
  opt <- parse_args(OptionParser(option_list=option_list))
  
}

# # Print out version and quit
# if ( opt$version ) {
#   cat(paste0("A_exec.R ", "\n"))
#   quit()
# }

# ----- Set up logging ---------------------------------------------------------

logger.setup(
  traceLog = file.path(opt$logDir, "A_exec_TRACE.log"),
  debugLog = file.path(opt$logDir, "A_exec_DEBUG.log"), 
  infoLog  = file.path(opt$logDir, "A_exec_INFO.log"), 
  errorLog = file.path(opt$logDir, "A_exec_ERROR.log")
)

# For use at the very end
errorLog <- file.path(opt$logDir, "A_exec_ERROR.log")

# Silence other warning messages
options(warn=-1) # -1=ignore, 0=save/print, 1=print, 2=error

# Start logging
logger.info("Running A_exec.R version %s",VERSION)
sessionString <- paste(capture.output(sessionInfo()), collapse="\n")
logger.debug("R session:\n\n%s\n", sessionString)

# ------ Create PAS ------------------------------------------------------------

result <- try({
  
  for (i in 1:opt$number) {
    print(opt$phrase)
  }
  
  logger.info("writing cats")
  #save(list="opt", file=filepath)  
  
}, silent=TRUE)

# Handle errors
if ( "try-error" %in% class(result) ) {
  msg <- paste("Error writing cats: ", geterrmessage())
  logger.fatal(msg)
} else {
  # Guarantee that the errorLog exists
  if ( !file.exists(errorLog) ) dummy <- file.create(errorLog)
  logger.info("The cats win this round!")
}

