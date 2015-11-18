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
plot.epanetmsx.rpt <- function(x, Nodes = "All", Links = "All", ...){
	
	# decide on the size of the matrix of plots
	sx <- summary(x)

	# number of nodes to plot
      if( is.null(Nodes[1])){
        # don't plot any nodes 
        nntp <- 0 

	} else if( grepl("all", Nodes[1], ignore.case = TRUE) )  { 
	   nntp <- sx$numNodes
	   NodeIDs <- sx$uniqueNodeIDs
	} else {
	   nntp <- length(Nodes)
	   NodeIDs <- Nodes 
        }  

	# number of links to plot
      if( is.null(Links[1] )){
        # don't plot any links 
        nltp <- 0 

	} else if( grepl("all", Links[1], ignore.case = TRUE) ) { 
	   nltp <- sx$numLinks
	   LinkIDs <- sx$uniqueLinkIDs
	} else {
	   nltp <- length(Links)
	   LinkIDs <- Links 
        }  
	
	# number of rows in plot matrix  
	pnr <- nntp + nltp 
	
	# number of node species
	nns <- length(sx$nodeSpecies)
	# number of link species 
	nls <- length(sx$linkSpecies) 
	# number of cols in plot matrix 
	pnc <- max( nns, nls)
	
        # create the matrix of plots 
	opar <- par( mfrow = c( pnr, pnc), mar = c(5.1,4.1,4.1,2.1) ) # mar = c(3,3,2,1) ) 
	
	# loop through the nodes
	if( nntp > 0 ){
		
		for( i in 1:nntp ){
			
			#epanetID of the node
			epanetID <- NodeIDs[i]
			
			for( j in 1:nns){
				species <- sx$nodeSpecies[j] 
				
				# data to plot 
				dtp <- subset( x$nodeResults, ID == epanetID, select = c('timeInSeconds',species))
				
				plot( dtp$timeInSeconds / 3600, dtp[,2], xlab ="", ylab="" )
				title( line = 2, xlab = 'Time (hour)', ylab = species ) 
				title( main = paste("Node", epanetID), line = 0.5)
				 
				
				# need to insert some blank plots to fill the end of the row 
				if( (nns < nls) & (j == nns) ){
					nb <- nls - nns 
					for( k in 1:nb){
						plot.new()
					}
				}
			}
		}
	}
	
	# loop through the links 
	if( nltp > 0){
		for( i in 1:nltp){
			
			epanetID <- LinkIDs[i]
			
			# loop over species 
			for( j in 1:nls){
				species <- sx$linkSpecies[j]
				
				#data to plot 
				dtp <- subset( x$linkResults, ID == epanetID, select = c('timeInSeconds', species))
				plot( dtp$timeInSeconds / 3600, dtp[,2], xlab = '', ylab = '' )
				title( line = 2, xlab = 'Time (hour)', ylab = species ) 
				title( main = paste("Link", epanetID), line = 0.5)
				
				if( (nls < nns) & (j == nls) ){
					# need to insert some blank plots to fill the end of the row 
					nb <- nns - nls  
					for( k in 1:nb){
						plot.new()
					}
				}
			}
		}
	}
	
	#return par to usual settings
    par(opar)
}

