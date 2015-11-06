#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

# s3 class for report files created by epanetmsx
# need to create data.frame of link results
# and node results
# 




#' Read msx results 
#' 
#' reads an Epanet-msx .rpt file into R
#' 
#' @export 
#' @param file the name of the file to read
#' @return Returns an epanet.rpt S3 object with two data.frame elements. 
#'
#' \item{nodeResults}{data.frame}
#' \item{linkResults}{data.frame}
epanetmsx.rpt <- function( file ) {
# this based heavily on the epanet.rpt function
	
  # read all the lines in the file 
  allLines <- readLines(file)
  #checkRptFile( allLines ) 

  title <- getTitle( allLines) 

  lengthOfAllLines <- length( allLines)
  
  resLines <- grep("<<<.+>>>", allLines)
  numTables <- length(resLines)

  # empty lists of results 
  nodeResList <- list()  
  linkResList <- list()  
  
    # initialize indices for these lists 
  ni <- 0  # node index
  li <- 0  # link index
  
  # go through the tables  
  for( i in 1:numTables ){
    # get the section  
    sectRange <- .getSectionRange( i, resLines, lengthOfAllLines)
    sect <- allLines[ sectRange$start : sectRange$end ]  
    
    # create a data frame from this section
    df <- msxSection2df( sect )  

    # decide if it's for link or node results 
    isNODE <- grepl( "Node", sect[1] )
    isLINK <- grepl( "Link", sect[1] )
    
    #add  data to approriate list of data frames 
    if( isNODE ){
      # increment node indexer 
      ni <- ni + 1
      nodeResList[[ ni ]] <- df
    }
    
    if( isLINK ){
      # increment indexer 
      li <- li + 1
      linkResList[[ li ]] <- df
    }
  }
  
  # combine all these results together 
  nodeResDF <- do.call("rbind", nodeResList )
  linkResDF <- do.call("rbind", linkResList )
 
  # and make a list of the to return 
  allResults <- list( Title = title, 
                      nodeResults = nodeResDF, 
                      linkResults = linkResDF )
  
  class(allResults) <- "epanetmsx.rpt"

  return( allResults ) 
}


#' Summary of Epanet-msx Simulation Results
#'
#' Provides a basic summary of simulation results 
#'
#' @export
#' @param  object of epanetmsx.rpt class
#' @param ... further arguments passed to summary()
summary.epanetmsx.rpt <- function( object, ...){

  # time info  (you have results from tmin to tmax at an interval of delta_t)



  ################
  #  node results
  ################
  
  # unique node IDs 
  uni <- unique(object$nodeResults$ID)

  # node result names
  nrn <- names(object$nodeResults) 

  # node result summary over species  
  jmax <- length(nrn) - 1 
  nrs <- summary( object$nodeResults[,3:jmax])

  # time info for nodes 
  nodeTimeRangeSecs <- range(object$nodeResults$timeInSeconds)
  nodeDeltaT <- mean(diff( object$nodeResults$timeInSeconds) )


  ###############
  # link results
  ###############

  uli <- unique(object$linkResults$ID)
  lrn <- names(object$linkResults) 
  jmax <- length(lrn) - 1 
  lrs <- summary( object$linkResults[,3:jmax])
  linkTimeRangeSecs <- range(object$linkResults$timeInSeconds)
  linkDeltaT <- mean(diff( object$linkResults$timeInSeconds) )
 
  
  
  # collect into an object 
  msxrptSmry <- list( Title = object$Title,
                      #nodes 
                      numNodes = length( uni ), 
                      uniqueNodeIDs = uni,
                      nodeTimeRangeInSeconds = nodeTimeRangeSecs,
                      nodeTimestep = nodeDeltaT,
                      nodeResSmry = nrs,
                      #links
                      numLinks = length( uli ),
                      uniqueLinkIDs = uli,
                      linkTimeRangeInSeconds = linkTimeRangeSecs,
                      linkTimestep = linkDeltaT,
                      linkResSmry = lrs ) 
  
  class(msxrptSmry) <- "summary.epanetmsx.rpt" 

  return( msxrptSmry)                     

}



#' Print msx rpt summary
#' 
#' The function prints a summary of multi-species simulation results
#' contained in the report file 
#'  
#' @export
#' @param x a summary.epanetmsx.rpt object   
#' @param ... further arguments passed to print 
print.summary.epanetmsx.rpt <- function(x,...){

  cat( x$Title)
  cat("\n") 

  # node results 
  cat(" node results\n")
  print( x$nodeResSmry)

  # link results 
  cat(" Link results\n")
  print( x$linkResSmry)

}
