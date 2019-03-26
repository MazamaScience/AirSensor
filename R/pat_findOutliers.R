
#' @export
#' @import graphics
#' @title Detect and Fix Timeseries Outliers
#' @param pat Purple Air Timeseries object from \code{createPATimeseriesObject()}
#' @param n integer window size
#' @param thresholdMin threshold value for outlier detection
#' @param replaceOutliers logical specifying whether replace outlier with the window median value
#' @param showPlot logical specifying whether to generate outlier detection plots
#' @return \code{pat} timeseries object with outliers replaced by median values.
#' @description Outlier detection using a Median Average Deviation "Hampel" filter.
#' 
#' The \code{thresholdMin} level is similar to a sigma value for normally distributed data.
#' Hampel filter values above 6 indicate a data value that is extremely unlikely to
#' be part of a normal distribution (~ 1/500 million) and therefore very likely to
#' be an outlier. By choosing a relatively large value for thresholdMin we make it
#' less likely that we will generate false positives.
#' 
#' Specifying \code{replaceOutliers = TRUE} allows you to perform smoothing by 
#' replacing outliers with the window median value. Using this technique, you can
#' create an highly smoothed, artificial dataset by setting \code{thresholdMin = 1}
#' or lower (but always above zero).
#' 
#' @note Additional documentation on the algorithm is available in 
#' \code{seismicRoll::findOutliers()}.
#' 

pat_findOutliers <- function(x) {

  Cmedian <- Rcpp::cppFunction('
                  double foo(NumericVector x, int k) {
                    int size = x.size();
                    double median = 0;
                    if ( size == 0 ) {
                      median = 0;
                    } else { 
                      std::sort(x.begin(), x.end()); 
                      if ( size % 2 == 0 ) {
                        median = (x[size / 2 - 1] + x[size / 2]) / 2;
                      } else { 
                        median = x[size / 2];                    
                      }
                    }
                    return median;
                  }
                  ')
  poo <- Cmedian(x - Cmedian(x,1), 1)
  
  return( poo )
}


