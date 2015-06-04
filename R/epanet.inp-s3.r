#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

#
#  File:  epanet.inp-s3.r
#
#  Purpose:  S3 implemetation of epanet.inp



#' Read .inp file 
#' 
#' Read an Epanet .inp file into R
#'
#' @aliases epanet.inp
#' @export
#' @param inpFile path to the .inp file to read
#' @details 
#' This function reads a text file in Epanet's .inp format 
#' and returns an S3 object which is a list with entries for 
#' sections of the .inp file.  Sections of the .inp file which are implemented 
#' appear in the Value section.
#' 
#' ID fields are read "as is" and stored as characters not factors.
#'        
#' Sections which are absent from the .inp file are NULL in the list.
#' 
#' Columns of data.frames use the headings exported by 
#' the Epanet GUI. 
#' 
#' The [OPTIONS] section in the .inp file is used to update
#'          a list of Epanet's default options. In this way if an option such as
#'          units is not specified by the .inp file, the units that would be used by
#'          default are provided.
#' 
#' In the [PATTERNS] and [CURVES] sections, integers used as names of list elements are backquoted
#'          according to the default behavior in R.  So if the .inp file has a pattern "1"
#'          this pattern will appear as element `1` in the list that is returned. A warning is issued in this case. 
#' 
#' @return Returns an epanet.inp s3 object which is a list with 
#' the following elements:
#' \item{Title}{character}
#' \item{Junctions}{data.frame}
#' \item{Tanks}{data.frame}
#' 
#' 
#' @references Rossman, L. A. (2000). Epanet 2 users manual. US EPA, Cincinnati, Ohio.
#' 
#' http://nepis.epa.gov/Adobe/PDF/P1007WWU.pdf
#' 
#' @examples
#' # path to Net1.inp example file included with this package
#' inp <- file.path( find.package("epanetReader"), "extdata","Net1.inp") 
#' n1 <- read.inp(inp)
#' summary(n1)
#' names(n1)
#' summary(n1$Junctions)
#' summary(n1$Pipes)
#' plot(n1) 
read.inp <- function (inpFile ){
 
  return(  epanet.inp( inpFile ) )
  
}

epanet.inp <- function( inpFile ){
	
	allLines <- readLines( inpFile )
  
  # read in all the sections  
  titl <- TITLE( allLines )
  junc <- JUNCTIONS( allLines )
  tank <- TANKS (allLines)
  resr <- RESERVOIRS( allLines) 
  pipe <- PIPES(allLines)
  pump <- PUMPS(allLines)
  valv <- VALVES(allLines)
  #dmd
  pats <- PATTERNS(allLines)
  crvs <- CURVES(allLines)
  #ctrl
  engy <- ENERGY(allLines)
  #emit
  #qlty
  #srcs
  #rxns
  #mix
  tims <- TIMES(allLines)
  #rpt
  opts <- OPTIONS(allLines)
  coor <- COORDINATES(allLines)
  #vert
  #labs
  #bdrp
  
  # make a list of all the elements 
  inp <- list( Title = titl,
               Junctions = junc,
               Tanks = tank,
               Reservoirs = resr,
               Pipes = pipe,
               Pumps = pump,
               Valves = valv,
               Patterns = pats,
               Curves = crvs,
               Energy = engy,
               Times = tims,
               Options = opts,
               Coordinates = coor)               
  
  class(inp) <- "epanet.inp"
  return( inp )
}


#' Summary Method for water.networks
#'
#' Summarizes the network by printing the Title of the
#' network and the number of each type of elements.
#'
#' @export
#' @param object of class 'water.network'
#' @param ... futher arguments
summary.epanet.inp <- function( object, ... ){
 
  Number <- unlist(lapply( object, function(object) dim(object)[1]))
  counts <- as.data.frame(Number)
  
   inpSmry <- list( Title = object$Title,
				   entryCounts = counts )
	class(inpSmry) <- "summary.epanet.inp"	   
	
	return(inpSmry)
	
}

print.summary.epanet.inp <- function(object, ...){
	
  cat( object$Title, sep = "\n")
  cat( "\n")
  
  print( object$entryCounts)
}

#' Plot .inp Links
#'
#' Add lines for pipes, pumps and valves
#'  from an epanet.inp object to an existing plot 
#'  
#' @param x epanet.inp object 
#' @export 
plotInpLinks <- function(x){
	
  #############
  #  Pipes  
  ############# 
  if( is.null( x$Pipes) == FALSE ){
    plot( expandedLinkTable( x$Pipes, x$Coordinates ),
          add = TRUE,  
          label = FALSE ) 
  }

    
  #############
  #  Pumps  
  ############# 
  if( is.null( x$Pumps )  == FALSE ){
	  ept <-  expandedLinkTable( x$Pumps, x$Coordinates )
	  plot( ept,
			  add = TRUE,  
			  label = FALSE) 
	  points( ept$midx, ept$midy, pch = 8 ) 
  }

  
  #############
  #  Valves 
  ############# 
  if( is.null( x$Valves )   == FALSE ){
	  evt <- expandedLinkTable(x$Valves, x$Coordinates)
	  plot( evt,
			  add = TRUE,  
			  label = FALSE) 
	  
	  points( evt$midx, evt$midy, pch = 25 ,
			  bg="black", col = "black" )  
  }
}

#' plot node elements 
#'
#' Adds node elements from epanet.inp object to an existing plot
#' 
#' @export 
#' @param x epanet.inp object
#' @param plot.junctions logical indicating whether to plot junctions 
plotInpNodes <- function( x, plot.junctions){
  ############# 
  #  Junctions 
  #############
  if( plot.junctions ){
     
    jpts <- merge( x = x$Junctions, by.x = "ID", all.x = TRUE,
                   y = x$Coordinates, by.y = "Node" )

    points( jpts$X.coord, jpts$Y.coord, pch = "." )

  }

  ###########
  #  Tanks 
  ###########
  if( is.null ( x$Tanks ) == FALSE ){
    tpts <- merge( x = x$Tanks, by.x = "ID", all.x = TRUE,
                   y = x$Coordinates, by.y = "Node" )
    points( tpts$X.coord, tpts$Y.coord, pch = 16, col='black' )
  }

  ##############
  #  Reservoirs
  ##############
  if( is.null( x$Reservoirs) == FALSE ){
    rpts <- merge( x = x$Reservoirs, by.x = "ID", all.x = TRUE,
                   y = x$Coordinates, by.y = "Node" )
    points( rpts$X.coord, rpts$Y.coord, pch = 15, col='black' )
  }
}


#' Plot inp Legend
#' 
#' Add a legend to an existing plot 
#' 
#' @param legend.locn passed to legend() 
#' @export  
plotInpLegend <- function(legend.locn){
	
  legend( legend.locn, bty = 'n',
          c("Tanks", "Reservoirs", "Pumps", "Valves"),
         pch = c(16, 15, 8, 25),
         pt.bg = 'black',
         col = 'black')
}

#' Plot Method for Water network
#' 
#' Make a plot of the network using base graphics
#' 
#' @export
#' @param x object of class 'water.network'
#' @param plot.junctions logical indicating whether plot junctions
#' @param legend.locn character string passed to legend() specifying
#'        the location of the legend on the plot 
#' @param ... other arguments passed to plot()
plot.epanet.inp <- function( x, 
                             plot.junctions  = TRUE,
                             legend.locn = "topright",
                                ... ) {

  
  # check that coordinates actually exist   
  if( is.null(dim(x$Coordinates) )){
    stop("network does not have coordinates" )
  }
  
  ### Do we have vertices ? 
  

  
  # create blank plot 
  plot( range(x$Coordinates$X),
        range(x$Coordinates$Y),
        type = 'n',
        xlab = "", xaxt = 'n',
        ylab = "", yaxt = 'n' )

  plotInpLinks(x)
  
  plotInpNodes(x, plot.junctions)

  plotInpLegend(legend.locn) 
  
}



