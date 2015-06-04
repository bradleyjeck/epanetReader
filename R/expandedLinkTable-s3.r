#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

#  File:  expandedLinkTable-s3.r
#
#  Purpose: define an s3 class for a link table 
#           with coordinates added
#
#  History: 
#      11 Aug 2014  -- adapted from earlier version 



#' Expanded Link Table
#' 
#' Add coordinates of nodes to data frame 
#' of pipes, pumps, or valves from an epanet.inp object 
#'
#' @export
#' @param Links data frame of Pipes, Pumps or Valves of from epanet.inp 
#' @param Coordinates table of epanet.inp
expandedLinkTable <- function( Links, Coordinates ){

  # handle a missing table 
  if( is.null(Links) ) {
    return(NA)
	#ept <- NA
  } else {
    ept <- merge( x = Links, by.x = "Node1", all.x = TRUE,
                  y = Coordinates, by.y = "Node" ) 
    #rename
    names(ept)[ grep("X.coord", names(ept)) ]  <- "x1"
    names(ept)[ grep("Y.coord", names(ept)) ]  <- "y1"
    
    #Node2 coords
    ept <- merge( x = ept, by.x = "Node2", all.x = TRUE,
                  y = Coordinates, by.y = "Node" ) 
    #rename
    names(ept)[ grep("X.coord", names(ept)) ]  <- "x2"
    names(ept)[ grep("Y.coord", names(ept)) ]  <- "y2"
    
    # midpoints for labeling 
    ept$midx <- (ept$x1 + ept$x2) / 2 
    ept$midy <- (ept$y1 + ept$y2) / 2 
    
  }
      
    class(ept) <- c("expandedLinkTable", "data.frame")
    
    return(ept)
}

#' plot an expanded link table 
#' 
#' @export
#' @param x object of type expandedLinkTable
#' @param add logical indicating whether to add this plot to an existing one 
#' @param label logical indicating if the links should be labeled at the mid points
#' @param linewidths passed to lwd argument in segments()
#' @param ... further arguments passed to plot
plot.expandedLinkTable <- function(x, add=FALSE, label=FALSE, linewidths = 3, ...){
  
    if( add == FALSE ){
      # generate a blank plot first 
      plot( range( c(x$x1, x$x2) ),
            range( c(x$y1, x$y2) ),
            type = 'n',
            xlab = "", xaxt = 'n',
            ylab = "", yaxt = 'n',
			...)
      
    } 
    # just put the segments out there 
	
    segments( x0 = x$x1, y0 = x$y1,
              x1 = x$x2, y1 = x$y2,
			  lwd = linewidths  )  
               
	
    if( label == TRUE ){
      text( x$midx, x$midy, x$ID)
    }
}
