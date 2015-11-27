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
#' @export 
#' @aliases epanetmsx.rpt 
read.msxrpt <-function( file ){
	
	mro <- epanetmsx.rpt(file)
	return( mro)
	
}


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
  if( length(uni) > 0 ){
	  # node result names
	  nrn <- names(object$nodeResults) 
	  
	  # node result summary over species  
	  jmax <- length(nrn) - 1 
	  # species results
	  nsr <- object$nodeResults[,3:jmax]
	  # species names
	  nds <- names(nsr)	
	  nrs <- summary(nsr )
	  
	  # time info for nodes 
	  nodeTimeRangeSecs <- range(object$nodeResults$timeInSeconds)
	  nodeDeltaT <- mean(diff( object$nodeResults$timeInSeconds) )
	  
  } else {
	  # no node results 
	  nodeTimeRangeSecs <- NULL
	  nodeDeltaT <- NULL
	  nrs <- NULL 
	  nds <- NULL 
	  
  }

  ###############
  # link results
  ###############

  uli <- unique(object$linkResults$ID)
  if( length(uli) > 0 ){
	  lrn <- names(object$linkResults) 
	  jmax <- length(lrn) - 1 
	  lsr <- object$linkResults[,3:jmax]
	  lks <- names(lsr)
	  lrs <- summary(lsr )
	  linkTimeRangeSecs <- range(object$linkResults$timeInSeconds)
	  linkDeltaT <- mean(diff( object$linkResults$timeInSeconds) )
  } else {
	  # no links 
	  linkTimeRangeSecs <- NULL
	  linkDeltaT <- NULL 
	  lrs <- NULL 
	  lks <- NULL 
  }
  
  
  # collect into an object 
  msxrptSmry <- list( Title = object$Title,
                      #nodes 
                      numNodes = length( uni ), 
                      uniqueNodeIDs = uni,
					  nodeSpecies = nds,
                      nodeTimeRangeInSeconds = nodeTimeRangeSecs,
                      nodeTimestep = nodeDeltaT,
                      nodeResSmry = nrs,
                      #links
                      numLinks = length( uli ),
                      uniqueLinkIDs = uli,
					  linkSpecies = lks,
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



#' Plot method for epanetmsx.rpt
#' 
#' Create plot matrix of Epanet-msx results 
#' 
#' @export 
#' @param x epanetmsx.rpt object
#' @param Nodes vector of node IDs to plot, 'all' to plot all of them or NULL to plot none 
#' @param Links vector of link IDs to plot, 'all' to plot all of them or NULL to plot none 
#' @details Creates a matrix of plots where each entry
#' plots the timeseries of one species. Each row of the plotted matrix
#' corresponds to a node or link.  The order
#' of species for the columns is taken from the order in the file
plot.epanetmsx.rpt <- function(x, Nodes = "All", Links = NULL, ...){

  Nodes <- 'all'
  Links <- NULL 



  # argument checking - either Nodes OR Links should be NULL 
  nn <- is.null(Nodes)
  ln <- is.null(Links) 
  if( nn & ln ) stop("Nodes = NULL  & Links = NULL . . . nothing to plot")
  if( (nn + ln )== 0 ) stop("one of the arguments Nodes OR Links must be NULL") 

  # compute summary of input
  sx <- summary(x) 

  # plotting for nodes 
#  if( nn == FALSE ){
    # nodes are plotted, confirm we have node results 
    if( is.null(x$nodeResults)){stop("no node results to plot")}
    # plotted num cols 
    if( grepl("all", Nodes[1], ignore.case = TRUE) )  { 
	   
	   IDs <- sx$uniqueNodeIDs
         stp <- sx$nodeSpecies  

    } else {
	   
	   IDs <- Nodes 
         stp <- sx$nodeSpecies  
    }

    # get the data to plot

    results <- x$nodeResults

    # species to plot 
    # number of species 
    nrow <- length(stp)
    ncol <- length(IDs) 

    dtp <- results[ which( results$ID %in% IDs == TRUE ) , ] 

    # create plot region 
    par( mfrow = c( nrow, ncol), oma = c(5,5,5,5), mar = c(0,0,0,0) )
    # widths for outer margin titles 
    omd <- par('omd')
    pltwid <- (omd[2] - omd[1]) / ncol


    # loop through speices  
    for( i in 1:nrow){ 
   
        species <- stp[i]
        print( paste('species =', species)) 
        yrange <- range( subset(dtp, select = species) ) 

        # loop over nodes or links 
        for( j in 1:ncol){ 
            epanetID <- IDs[j]
            dtp2 <- subset( dtp, ID == epanetID, select = c( 'timeInSeconds', species) ) 
              
           plot( dtp2$timeInSeconds / 3600, dtp2[,2], 
                  xlab ='', ylab = '', ylim = yrange,
                  xaxt= 'n', yaxt = 'n' )

            # only add axes in certain cases 
            if( i == 1 ){
               axis( side = 3 )                              
            } 
            if( j == 1){
               axis( side = 2 )  
               title( ylab = species ) 
            }
            if( i == nrow ){
               axis( side = 1) 
               title( xlab = "Hour", 
                      sub = paste("Node", epanetID) ) 
            }
            
            if( j == ncol ) axis( side = 4) 
         }
     }
}
  





 





