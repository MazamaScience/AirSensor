
test_usesArgument_B <- function(
  packageFUNC = NULL,
  functionName = NULL,
  argumentName = NULL
) {

  argumentPattern <- paste0(argumentName,"=|",argumentName,"\\s=")
  
  functionString <- paste0( as.character(rlang::fn_body(packageFUNC)), collapse="\n")
  
  testResult <- stringr::str_detect(findFunctionCalls(functionString, functionName), argumentPattern)
  
  # Could optionally spit out offending calls
  
  return(testResult)
  
}

test_usesArgument <- function(
  file = NULL,
  functionName = NULL,
  argumentName = NULL
) {
  
  argumentPattern <- paste0(argumentName,"=|",argumentName,"\\s=")
  
  functionString <- readr::read_file(file)
  
  testResult <- stringr::str_detect(findFunctionCalls(functionString, functionName), argumentPattern)
  
  # Could optionally spit out offending calls
  
  return(testResult)
  
}

findFunctionCalls <- function(
  functionString = NULL,
  functionName = NULL
) {
  
  functionOpen <- paste0(functionName, "\\(")

  openParens <- stringr::str_locate_all(functionString, "\\(")[[1]][,1]
  closeParens <- stringr::str_locate_all(functionString, "\\)")[[1]][,1]
  
  # TODO:  A better regex above would avoid matching escaped parens so that
  # TODO:  we wouldn't need the check below
  
  if ( length(openParens) != length(closeParens) )
    stop("Unmatched parens (Are you using strings with single escaped parens?)")
  
  functionStarts <- stringr::str_locate_all(functionString, functionOpen)[[1]][,1]
  functionEnds <- stringr::str_locate_all(functionString, functionOpen)[[1]][,2]
  
  indices <- which(openParens %in% (functionEnds))
  
  starts <- functionStarts
  ends <- closeParens[indices]
  
  functionCallStrings <- stringr::str_sub(functionString, starts, ends)
  
  return(functionCallStrings)
  
}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {
 
  file <- "R/pas_load.R"
  functionName <- "strftime"
  
  # Test on source code in the AirSensor package
  stringr::str_detect(findFunctionCalls(readr::read_file(file), "strftime"), "tz=|tz\\s=")
  #[1] TRUE TRUE TRUE TRUE
  all(stringr::str_detect(findFunctionCalls(readr::read_file(file), "strftime"), "tz=|tz\\s="))
  #[1] TRUE  
  
  test_usesArgument("R/pas_load.R", "strftime", "tz")
  #[1] TRUE TRUE TRUE TRUE
  
  test_usesArgument_B(PWFSLSmoke::monitor_dailyBarplot, "strftime", "tz")
  #[1] TRUE TRUE
  
}

if ( FALSE ) {
  
  files <- list.files("R", full.names = TRUE)
  for ( file in files ) {
    
    result <- try({
      test_usesArgument(readr::read_file(file), "strftime", "tz")
    }, silent = TRUE)
    if ( "try-error" %in% class(result) ) {
      cat(file)
      cat("\n")
      cat(geterrmessage())
      cat("\n\n")
    } else if ( length(result) > 0 && !any(result) ) {
      cat(file)
      cat("\n")
      cat(result)
      cat("\n\n")
    }
    
  }
  
}


