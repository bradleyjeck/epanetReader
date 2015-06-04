#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

#  File: rptFuncs.r
#
#  By: bradley.eck@ie.ibm.com
#
#  Purpose: functions to read an rpt file 

.getSectionRange <- function( j, resLines, lengthOfAllLines ) {
  # get the section of the rpt file that corresponds 
  # to the j-th entry in resLines 
   
  startLine <- resLines[j]
  
  numTables <- length( resLines ) 
  
  if( j < numTables ){
    endLine <- resLines[j+1] - 1 
  } else {
    endLine <- lengthOfAllLines - 1 
  }
  
  rl <- list( start = startLine, end = endLine )  
  return( rl ) 
}

.getTimeStamp <- function( tsLine){
  
 tokens <- unlist(  strsplit(tsLine, "\\s") ) 

 stamp <-
   tokens[ grep(":.{2}:", tokens) ]

 if( length(stamp) > 1 ){
   msg <- paste( "more than one timestamp found on line", tsLine)
   stop( msg )
 }
 
 if( length(stamp) == 0 ){
	 # use a time of zero if none is supplied 
	 stamp <- "0:00:00"
 } 
 return( stamp ) 
 
}

.timeStampToSeconds <- function( stamp ){
 
  tokens <- unlist(strsplit(stamp,":"))

  if( length(tokens) != 3 ){
    stop(paste("need to implement converstion for stamp", stamp ))
  }
  
  hours <- tokens[1]
  minutes <- tokens[2]
  seconds <- tokens[3]
  
  totalSeconds <- as.integer(seconds) + 
                  as.integer(minutes) * 60 + 
                  as.integer(hours) * 3600
  
  return( totalSeconds ) 
}

.section2df <- function( sect ){
 # make a section into it's own data frame

  imax <- length(sect)
  
  #figure out what is reported
  measvars <- unlist(strsplit( gsub( "^\\s+", "", sect[3] ), "\\s+"))

  idvarAndUnits <- unlist(strsplit( gsub("^\\s+", "", sect[4]) , "\\s+"))
  
  columnNames <- c( idvarAndUnits[1], measvars, "note" ) 
  
  # make the section a data frame 
  df <- read.table( text = sect[6:imax], col.names=columnNames,
                    strip.white = TRUE, fill = TRUE,
                    header = FALSE, as.is = TRUE )
  
  # now add the time info to the table 
  stamp <- .getTimeStamp( sect[1] )  
  
  # convert that stamp into sections 
  time_secs <-  .timeStampToSeconds(stamp)
  
  df$Timestamp <- stamp
  df$timeInSeconds <- time_secs
  
  return( df ) 
}

#' Bin Breaker    
#' 
#' Generate break points for use with cut() 
#' and range labels based on sample max and min  
#' 
#' @param x vector to find cuts for 
#' @param nbin  number of bins 
#' @return list with elements Breaks and Labels
#' @details 
#' helpful in plot making plots 
#' Labels use the acutal max and min rather than +/- 1% used by cut() 
binBreaker <- function( x, nbin){
	
    xmax <- max(x, na.rm = TRUE )
    xmin <- min(x, na.rm = TRUE )
	
	# use cut() to find break points for the specified number of bins   
	labs <- levels( cut( x, breaks = nbin ))
	# from help(cut) example on getting break points 
	brks <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
				  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))
    
    # get the breaks to use with cut()   	
	brkpts4cut <-  c( brks[1,1], brks[,2])
	
	# force a zero in the legend values  	  
	labs4legend <- levels( cut( x, breaks = c( xmin, brks[1:(nbin-1),2], xmax) ))  
	
	#combine results into list and return 
	return( list( Breaks = brkpts4cut, Labels = labs4legend))
		
} 
