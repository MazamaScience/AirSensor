#' @export
#' @importFrom rlang .data
#' @import graphics
#' @title Display multiple plots on one page
#' @param ... any number of ggobjects to be plotted
#' @param plotlist a list() of any number of ggplot objects to plot on a single pane
#' @param cols Number of columns in the plot layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' 
#' @description # A plotting function that uses ggplot2 to display multiple 
#' ggplot objects in a single pane. 
#' 
#' @note Additional documentation of the multiplot algorithm is available at 
#' cookbook-r.com.

multi_ggplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
  
  plots <- c(list(...), plotlist)
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
}
