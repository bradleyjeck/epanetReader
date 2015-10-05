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




#' Read .rpt file
#' 
#' reads an Epanet .rpt file into R
#' 
#' @export 
#' @param file the name of the file to read
#' @return Returns an epanet.rpt S3 object with two data.frame elements. 
#'
#' \item{nodeResults}{data.frame}
#' \item{linkResults}{data.frame}
epanetmsx.rpt <- function( file ) {
# this based heavily on the epanet.rpt function
	source("../R/rptFuncs.r")
file <- "example.rpt"
	
  # read all the lines in the file 
  allLines <- readLines(file)
  #checkRptFile( allLines ) 

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
  allResults <- list( nodeResults = nodeResDF, 
                      linkResults = linkResDF )
  
  class(allResults) <- "epanetmsx.rpt"

  return( allResults ) 
}

