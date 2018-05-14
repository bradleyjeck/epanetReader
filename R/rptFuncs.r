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

 # stamp in token after "at"
 stamp  <- tokens[  grep( "at", tokens) + 1 ]

 if( length(stamp) == 0 ){
	 # use a time of zero if none is supplied 
	 stamp <- "0:00:00"
 } 
 return( stamp ) 
 
}

.timeStampToSeconds <- function( stamp ){

	tokens <- unlist(strsplit(stamp,":"))
	
	if( length(tokens) < 2 ){
		stop(paste("don't have interpretation for stamp", stamp ))
	}
	if( length(tokens)  > 3 ){
		stop(paste("don't have interpretation for stamp", stamp ))
	}
	
	
	hours <- tokens[1]
	minutes <- tokens[2]

	m <- as.integer(minutes)
	if( m > 59){
		stop(paste0("invalid minutes in ", stamp))
	}
	
	
	totalSeconds <- as.integer(hours) * 3600 + 
			        m * 60 
	
	if( length( tokens)  == 3 ){ 
		
		seconds <- tokens[3]
		s <- as.integer(seconds)
		if( s > 59){
			stop(paste0("invalid seconds in ", stamp))
		}
		
		totalSeconds <- totalSeconds +  as.integer(seconds) 
	}
	
	return( as.integer(totalSeconds) ) 
}

.section2df <- function( sect ){
 # make a section into it's own data frame

  imax <- length(sect)
  
   # Take column headings from labels. Sometimes first column is labeled
   # in two rows, sometimes in 1.  See tests. 
   headerRow1 <- unlist(strsplit( gsub("^\\s+", "", sect[3] ), "\\s+"))
   headerRow2 <- unlist(strsplit( gsub("^\\s+", "", sect[4] ), "\\s+"))
   # When the water quality is a trace the header row includes a percent symbol
   # in this case modify headerRow1 one so that the last entry is Pct_from_<row2>
	gpl <- grepl("\\%", headerRow1)
	if( max(gpl) > 0  ){
		 gp <- grep("\\%", headerRow1)
	    cn <- paste("Pct", headerRow1[gp +1], utils::tail(headerRow2,1), sep = "_")	
		headerRow1 <- c( headerRow1[1:(gp-1)], cn)
	}
	
	## figure out the column names 
	 hr1_link <- grepl("link", headerRow1[1], ignore.case=TRUE) 
	 hr1_node <- grepl("node", headerRow1[1], ignore.case=TRUE) 
	 
	 if( hr1_link | hr1_node){
		 columnNames <- c(headerRow1, "note")
	 } else { 
		 # node or link is not in first col of first row
		 columnNames <- c("ID", headerRow1, "note")
	 }
	
   # name the first column "ID" rather than
   # "Node" or "Link"  to be consistent with inp objects
	columnNames[1] <- "ID"

  # a quirk of the gui rpt file 	
  columnNames <- gsub( "VelocityUnit", "Velocity", columnNames)	
   
  # set colClasses, everything is numeric execpt
  # columns: ID, Status, and note  
	lcn <- length( columnNames)
  cc <- rep("numeric", lcn )
  cc[1] <- "character"  # for ID column 
  cc[lcn] <- "character" # for note column 
  #status column 
  cc[grep("Status", columnNames)] <- "factor"
  
   
  # make the section a data frame 
  df <- utils::read.table( text = sect[6:imax], 
		  col.names=columnNames,
		  colClasses = cc, 
		  strip.white = TRUE, fill = TRUE, header = FALSE )
  
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
#' @param nbin number of bins 
#' @return list with elements Breaks and Labels
#' @details 
#' Helpful in making labels use the acutal max and min rather than
#' the +/- 1% cut() uses by default. 
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



checkRptFile <- function( allLines ){
  # check rpt file format 
   

   # look for node results and link results 
   hasNodeResults <- as.logical( max( grepl("Node Results", allLines)))
   hasLinkResults <- as.logical( max( grepl("Link Results", allLines)))
   hasEnergyResults <- as.logical( max( grepl("Energy Usage", allLines)))

   if( hasNodeResults == FALSE ){
  
     msg <- paste(" Node results not found in .rpt file. \n",
                  "Add the line 'Nodes All' to the [REPORT] section of the .inp file.")
     warning(msg)
   }

   if( hasLinkResults == FALSE ){
  
     msg <- paste(" Link results not found in .rpt file. \n",
                  "Add the line 'Links All' to the [REPORT] section of the .inp file.")
     warning(msg)
   }

   


   if( ( hasNodeResults == FALSE )& 
       ( hasLinkResults == FALSE )&
       ( hasEnergyResults==FALSE )  ){
       
       # no results to read, give error 
       stop("No results to read")

   }

}

cleanRptLines <- function( allLines ){
	
	#  lines with page break 
	pb_lines <- grepl("\f", allLines)
	myLines <- allLines[ !pb_lines ]
		
	# delete lines starting with __Page_#
	pg_lines <- grepl("^  Page ", myLines)
	myLines <- myLines[ !pg_lines ]
	
	# delete lines containing "(continued)" along with
	continues_header <- rep(FALSE, length(myLines))
	continues_lines <- grep("(continued)", myLines)
	continues_header_lines <- unlist( lapply(continues_lines, seq, by=1, length.out=5)) 
	continues_header[ continues_header_lines] <- TRUE 
	
	cleanLines <- myLines[ !continues_header ]
	
	return( cleanLines)
	
}

getEnergyUsage <- function( cleanLines ){
 
  tag <- grep("Energy Usage", cleanLines) 
 
  result <- NULL 
   
  if( length(tag) > 1  ){
    warning("The string 'Energy Usage' appeared more than once in the rpt file so the energy usage table was not read")
    
  } else if ( length(tag) == 1 ){
    # Energy Usage table exists
    begin <- tag + 5
    bars <- grep("-----",cleanLines)
    end <- bars[ which( bars > begin)[1] ] - 1 
    
    # get the correct names for the cols  
    varnames <- c("Pump","usageFactor","avgEfficiency", "kWh_per_m3","avg_kW","peak_kW","dailyCost") 
    if( grepl("Mgal",cleanLines[ tag+3])){
      varnames[4] <- "kWh_per_Mgal"
    }
    
    # build a data frame of energy usage     
    egyusg <- utils::read.table( text = cleanLines[begin:end], 
                      stringsAsFactors = FALSE,
                      col.names = varnames) 
    result <- egyusg
    
  } 
  
  return( result) 
  
}
