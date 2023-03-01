#' @export
#' @docType data
#' @name SCAQMD_SENSOR_INDICES
#' @title Comma separated list of SCAQMD sensor indices. 
#' @format String with comma-separated sensor_index values.
#' @description Character string with PurpleAir \code{sensor_index} values
#' maintained by the South Coast Air Quality Management District (SCAQMD).
#' These can be used with \code{pas_createNew(show_only = ...)} to create a 
#' \emph{pas} object containing only SCAQMD sensors.
#'
SCAQMD_SENSOR_INDICES <-
  paste0(
    "1748,1860,1914,1922,2018,2020,2025,2169,2303,2307,2323,2336,2352,2356,2360,",
    "2372,2380,2452,2496,2504,2693,2713,2725,3487,3515,4666,4670,4708,4714,5218,",
    "6836,9180,9196,9206,9208,9214,9276,9314,9336,9340,9352,9364,9376,9382,9386,",
    "9390,9392,9398,9472,9500,12216,143602,22727,23241,23245,23253,23263,23291,",
    "23429,160633,69773,90253,90263,91095,104894,104900,104916,104926,104930,",
    "104932,104942"
  )