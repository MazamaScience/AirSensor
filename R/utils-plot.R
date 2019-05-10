#' @export
#' @import graphics
#' 
#' @title Matrix scatter plot variables in a data frame
#' 
#' @description Creates a multi-panel scatterplot comparing all variables in the
#' data frame object. If any variables have not valid data, they are omitted 
#' from the plot.
#' 
#' @param data data frame
#' @param shape symbol to use for points
#' @param size size of points
#' @param color color of points
#' @param alpha opacity of points
#'

scatterPlot <- function(
  data, 
  shape = 18, 
  size = 1.5, 
  color = "black", 
  alpha = 0.5
) {
  scatterPlot <- GGally::ggpairs( 
    data,
    mapping = ggplot2::aes(alpha = 0.15),
    lower = list(
      continuous = GGally::wrap(
        "points", 
        size = size, 
        shape = shape,
        color = color,
        alpha = alpha)),
    diag = list(
      continuous = GGally::wrap(
        "densityDiag")), 
    upper = list(continuous = "cor")
  ) + ggplot2::theme_bw()
  
  return(scatterPlot)
}

#' @export
#' @importFrom rlang .data
#' @import graphics
#' @title Display multiple plots on one page
#' @param ... any number of ggobjects to be plotted
#' @param plotList a list() of any number of ggplot objects to plot on a single pane
#' @param cols Number of columns in the plot layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' 
#' @description # A plotting function that uses ggplot2 to display multiple 
#' ggplot objects in a single pane. 
#' 
#' @note Additional documentation of the multiplot algorithm is available at 
#' cookbook-r.com.

multi_ggplot <- function(..., plotList = NULL, cols = 1, layout = NULL) {
  
  plots <- c(list(...), plotList)
  numPlots <- length(plots)
  
  if ( is.null(layout) ) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if ( numPlots == 1 ) {
    print(plots[[1]])
    
  } else {
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))) )
    
    for ( i in 1:numPlots ) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
  
  # TODO:  Does multi_ggplot() return anything?
  
}