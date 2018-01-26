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
#' @param file the name of the file to read 
#' @details 
#' This function reads a text file in Epanet's .inp format 
#' and returns an S3 object with entries for 
#' sections of the .inp file.  Sections of the .inp file that are implemented 
#' appear in the Value section.
#' 
#' Fields for node or link ID are stored as characters not factors or integers.
#' However, some fields are stored as factors
#' to allow more informative summaries. Examples include valve type and pipe status.  
#'        
#' Sections that are absent from the .inp file are NULL in the list.
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
#' @return Returns an epanet.inp S3 object with 
#' elements of the following names and types corresponding to sections
#' of the .inp file. Sections missing from the .inp file have a value of NULL.
#' \item{Title}{character}
#' \item{Junctions}{data.frame}
#' \item{Tanks}{data.frame}
#' \item{Reservoirs}{data.frame}
#' \item{Pipes}{data.frame}
#' \item{Pumps}{data.frame}
#' \item{Valves}{data.frame}
#' \item{Demands}{data.frame}
#' \item{Status}{data.frame}
#' \item{Emitters}{data.frame}
#' \item{Quality}{data.frame}
#' \item{Sources}{data.frame}
#' \item{Reactions}{character}
#' \item{Mixing}{data.frame}
#' \item{Patterns}{list}
#' \item{Curves}{list}
#' \item{Controls}{character}
#' \item{Rules}{character}
#' \item{Energy}{character}
#' \item{Times}{character}
#' \item{Report}{character}
#' \item{Options}{list}
#' \item{Coordinates}{data.frame}
#' \item{Vertices}{data.frame}
#' \item{Labels}{data.frame}
#' \item{Backdrop}{character}
#' \item{Tags}{character}
#' 
#' @references Rossman, L. A. (2000). Epanet 2 users manual. US EPA, Cincinnati, Ohio.
#' 
#' http://nepis.epa.gov/Adobe/PDF/P1007WWU.pdf
#' 
#' @examples
#' # path to Net1.inp example file included with this package
#' inp <- file.path( find.package("epanetReader"), "extdata","Net1.inp") 
#' 
#' #read the network file into R
#' n1 <- read.inp(inp)
#' summary(n1)
#' names(n1)
#' summary(n1$Junctions)
#' summary(n1$Pipes)
#' plot(n1) 
read.inp <- function (file ){
 
  return(  epanet.inp( file ) )
  
}

epanet.inp <- function( file ){
	
	allLines <- read_lines_wrapper( file )
	
	# remove comments  
	allLines <-  gsub( ";.*$", "", allLines)
  
  # read in all the sections  
  titl <- TITLE( allLines )
  junc <- JUNCTIONS( allLines )
  tank <- TANKS (allLines)
  resr <- RESERVOIRS( allLines) 
  pipe <- PIPES(allLines)
  pump <- PUMPS(allLines)
  valv <- VALVES(allLines)
  dmd  <- DEMANDS(allLines) 
  pats <- PATTERNS(allLines)
  crvs <- CURVES(allLines)
  ctrl <- CONTROLS(allLines)
  rul  <- RULES(allLines)
  engy <- ENERGY(allLines)
  stat <- STATUS(allLines)
  emit <- EMITTERS(allLines)
  qlty <- QUALITY(allLines)
  srcs <- SOURCES(allLines)
  rxns <- REACTIONS(allLines)
  mix  <- MIXING(allLines)
  tims <- TIMES(allLines)
  rpt  <- REPORT(allLines)
  opts <- OPTIONS(allLines)
  coor <- COORDINATES(allLines)
  vert <- VERTICES(allLines)
  labs <- LABELS(allLines)
  bdrp <- BACKDROP(allLines)
  tags <- TAGS(allLines)
  
  # make a list of all the elements 
  inp <- list( Title = titl,
               Junctions = junc,
               Tanks = tank,
               Reservoirs = resr,
               Pipes = pipe,
               Pumps = pump,
               Valves = valv,
			   Demands = dmd, 
               Patterns = pats,
               Curves = crvs,
			   Controls = ctrl, 
			   Rules = rul,
               Energy = engy,
			   Status = stat,
			   Emitters = emit,
			   Quality = qlty,
			   Sources = srcs,
			   Reactions = rxns,
			   Mixing = mix,
               Times = tims,
			   Report =rpt,
               Options = opts,
               Coordinates = coor,
			   Vertices = vert,
			   Labels = labs,
			   Backdrop = bdrp,
			   Tags = tags)               
  
  class(inp) <- "epanet.inp"
  return( inp )
}


#' Summary Method for epanet.inp
#'
#' Summarizes the network by printing the Title of the
#' network and the number of each type of elements.
#'
#' @export
#' @param object of class epanet.inp
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
#' @export 
#' @param x epanet.inp object 
#' @param lwd width of lines  
#' @param col color of lines
#' @details 
#' Helper function for building up a plot of the network by
#' adding links to an existing plot.  
#' @examples 
#' ## make a new blank plot 
#' plot( range(Net1$Coordinates$X), range(Net1$Coordinates$Y), type = 'n') 
#' ## add the links
#' plotInpLinks(Net1) 
plotInpLinks <- function(x, lwd=3, col='black'){
	
  #############
  #  Pipes  
  ############# 
  if( is.null( x$Pipes) == FALSE ){
    graphics::plot( expandedLinkTable( x$Pipes, x$Coordinates),         
	                add = TRUE,  
                    label = FALSE, 
		            linewidth=lwd, 
		            color=col )
 
  }

  #############
  #  Pumps  
  ############# 
  if( is.null( x$Pumps )  == FALSE ){
	  ept <-  expandedLinkTable( x$Pumps, x$Coordinates )
	  graphics::plot( ept,
		        	  add = TRUE,  
                    label = FALSE, 
		            linewidth=lwd, 
		            color=col )
	  graphics::points( ept$midx, ept$midy, pch = 8 ) 
  }

  
  #############
  #  Valves 
  ############# 
  if( is.null( x$Valves )   == FALSE ){
	  evt <- expandedLinkTable(x$Valves, x$Coordinates)
	  graphics::plot( evt,
		        	  add = TRUE,  
                    label = FALSE, 
		            linewidth=lwd, 
		            color=col )
	  
	  graphics::points( evt$midx, evt$midy, pch = 25 ,
			  bg="black", col = "black" )  
  }
}

#' Plot Node Elements 
#'
#' Adds node elements from epanet.inp object to an existing plot
#' 
#' @export 
#' @param x epanet.inp object
#' @param plot.junctions logical indicating whether to plot junctions 
#' @details  Helper function for building up a network plot. Tanks and
#' Reservoirs are shown using plot characters (pch) '           16 and 15.
#' Junctions, if plotted, appear as pch ="."
#' @examples 
#' ## make a new blank plot 
#' plot( range(Net1$Coordinates$X), range(Net1$Coordinates$Y), type = 'n') 
#' ## add the nodes, including junctions 
#' plotInpNodes(Net1, TRUE ) 
plotInpNodes <- function( x, plot.junctions){
  ############# 
  #  Junctions 
  #############
  if( plot.junctions ){
     
    jpts <- merge( x = x$Junctions, by.x = "ID", all.x = TRUE,
                   y = x$Coordinates, by.y = "Node" )

    graphics::points( jpts$X.coord, jpts$Y.coord, pch = "." )

  }

  ###########
  #  Tanks 
  ###########
  if( is.null ( x$Tanks ) == FALSE ){
    tpts <- merge( x = x$Tanks, by.x = "ID", all.x = TRUE,
                   y = x$Coordinates, by.y = "Node" )
    graphics::points( tpts$X.coord, tpts$Y.coord, pch = 16, col='black' )
  }

  ##############
  #  Reservoirs
  ##############
  if( is.null( x$Reservoirs) == FALSE ){
    rpts <- merge( x = x$Reservoirs, by.x = "ID", all.x = TRUE,
                   y = x$Coordinates, by.y = "Node" )
    graphics::points( rpts$X.coord, rpts$Y.coord, pch = 15, col='black' )
  }
}


#' Plot Legend of Network Elements
#' 
#' Add legend of network elements to the active plot 
#' 
#' @export  
#' @param legend.locn keyword for location of legend. See details of legend()
#'        function.
#' @details
#' Helper function for adding a legend to the active plot.  
#' Uses plot characters 16, 15, 8 and 25 for Tanks, Reservoirs, Pumps and Valves 
#' for compatibility with plotInpNodes() 
#' @examples
#' ## make a new blank plot 
#' plot( c(0,1), c(0,1), type = 'n') 
#' ## add the nodes, including junctions 
#' plotElementsLegend('topright') 
plotElementsLegend <- function(legend.locn) {
	
  graphics::legend( legend.locn, bty = 'n',
          c("Tanks", "Reservoirs", "Pumps", "Valves"),
         pch = c(16, 15, 8, 25),
         pt.bg = 'black',
         col = 'black')
}

#' Plot Method for epanet.inp 
#' 
#' Make a plot of the network using base graphics
#' 
#' @export
#' @param x object of class epanet.inp 
#' @param plot.junctions logical indicating whether to plot junctions
#' @param legend.locn character string passed to legend() specifying
#'        the location of the legend on the plot 
#' @param plot.labels logical indicating whether to plot the labels using text()
#' @param link.lwd value of lwd passed to segments()
#' @param link.col value of col passed to segments() 
#' @param ... other arguments passed to plot()
#' @details
#' Implements the generic plot function for S3 objects of class epanet.inp.
#' The plot is built from base graphics by creating a blank plot and then calling 
#' the helper functions plotInpLinks(), plotInpNodes(), plotElementsLegend().  
#' @examples
#' plot(Net1) 
#' plot(Net1, plot.labels=TRUE)
plot.epanet.inp <- function( x, 
                             plot.junctions  = TRUE,
                             legend.locn = "topright",
							 plot.labels = FALSE,
							 link.lwd = 3,
							 link.col = 'black',
                                ... ) {

  
  # check that coordinates actually exist   
  if( is.null(dim(x$Coordinates) )){
    stop("network does not have coordinates" )
  }
  
  ### Do we have vertices ? 
  

  
  # create blank plot 
  graphics::plot( range(x$Coordinates$X),
        range(x$Coordinates$Y),
        type = 'n', asp = 1, 
        xlab = "", xaxt = 'n',
        ylab = "", yaxt = 'n', ... )

  plotInpLinks(x, lwd=link.lwd, col=link.col)
  
  plotInpNodes(x, plot.junctions)

  plotElementsLegend(legend.locn) 
  
  if( plot.labels){
  	plotInpLabels(x)
  }
  
}

plotInpLabels <- function(x){
	
	xx <- x$Labels$X.coord
	yy <- x$Labels$Y.coord
	lab <- x$Labels$Label
	graphics::text(xx,yy,lab)
}

#' Check if an object as class 'epanet.inp' 
#'
#' @param x an R object 
#' @export
is.epanet.inp <- function(x){
  inherits(x,"epanet.inp")

}
