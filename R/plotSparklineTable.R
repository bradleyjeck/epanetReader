###############################################################################
# (c) Copyright IBM Corp. 2015 
# 
# Author: Bradley J. Eck 
###############################################################################


#' Plot Sparkline Table
#' 
#' Generate a table of sparkline plots 
#' 
#' @export 
#' @param df data.frame of values to plot.   
#' @param row.var variable for rows of the table 
#' @param col.vars variables for columns of the table 
#' @param xvar optional name of variable for horizontal axis of sparkline plots
#' @param xrange.labels optional vector of length 2 with labels for the first
#'        and last quantities plotted on x-axis, often a date and/or time
#' 
#' @details Generates a table of 'sparkline' plots of data in df. rows the table correspond to 
#'         different values of row.var. The table's first column gives the value of row.var. The 
#'         remaining columns contain sparkline plots for the values of col.vars.  When xvar is not
#'         provided values are plotted against their index in the extracted vector. The starting
#'         and ending values are labeled. 
#'         Uses layout() function to arrange plots.
#' 
#' @seealso 
#' yaletoolkit and sparkTable packages 
#' @references
#' E. Tufte, Beautiful Evidence, Graphics Press, 2006.
#'  
#' @example
#' plotSparklineTable( Orange, row.var = 'Tree', col.vars = c('age','circumference'))
#' plotSparklineTable( Loblolly, row.var = 'Seed', col.vars = 'height')
#' ## specify the x variable if you have it, especially if it differs 
#' plotSparklineTable(Theoph, row.var = 'Subject', col.vars = 'conc')


plotSparklineTable <- function( df, row.var, col.vars, xvar = NULL, xrange.labels = NULL ){
    
	#argument checking
	if( is.data.frame(df) == FALSE) stop("df must be a data.frame")
	if( length( row.var) != 1 ) stop("row.var must have length 1")
    if( (row.var %in% names(df))  == FALSE ) stop(paste("row.var", row.var, "is not a column in data.frame df"))
    if( min( col.vars %in% names(df)) < 1 ) stop(paste("at least one value in col.vars is not a column in data.frame df")) 
	if( is.null(xvar) == FALSE ){ 
      if( (xvar %in% names(df))  == FALSE ) {
		  stop(paste("xvar", xvar, "is not a column in data.frame df"))
	  }
    }
    # x range label  
	xrl <- c("","")
	if( is.null(xrange.labels) == FALSE ){
		if( length(xrange.labels) != 2 ) stop("xrange.labels must have length 2")
		xrl <- as.character(xrange.labels)
	}
	
	
	urv <- unique( df[ , row.var])
	Nrv <- length(urv ) 	
	Ncv <- length(col.vars)
	M <- getLayoutMatrix( num.row.var = Nrv,
			              num.col.vars = Ncv  )
				  
	# create the plot grid 
	par( mar = c(0,0,0,0), oma = rep(1,4))
	layout( mat = M, respect = FALSE  )
	
	# plot the header 
	plotWord(row.var, font = 2 )
	for( j in 1:Ncv){
		
		plotWord( xrl[1], font=2)
		plotWord( col.vars[j], font = 2 , cex = 1)
		plotWord( xrl[2], font=2)
	}
	
	
	# Loop over the rows 
	for( i in 1:Nrv){
		
		plotWord( urv[i]) 
		
		# Loop thru the params 
		for(j in 1:Ncv){
			# first param, start value 
		    xy <- sparklineData( df, row.var, urv[i], col.vars[j], xvar)
			N <- dim(xy)[1]
			plotWord( xy[1,2] ) 
			plotSparkline( xy )
			plotWord( xy[N,2] ) 	
		}
	}
	
	
}

# create matrix for a single sparkline
sparklineData <- function( df, row.var, this.row.var, col.var, xvar){
	
	D <- df[ which( df[ , row.var] == this.row.var), c( xvar, col.var )   ] 
    D <- as.matrix(D)	
	# if D has only one column we assume it's already in the right order
	# and add a vector of indices to plot against 
	# if D has two columns we sort it into xvar order

	Dncol <- dim(D)[2]
	Dnrow <- dim(D)[1]
   
	if( Dncol == 1 ){ 
	
		D <- cbind( 1:Dnrow, D)
	
	} else if( Dncol == 2) { 
		
		xord <- order( D[ , 1])
		D <- D[xord, ]
	
	} else { 
		stop("D should only have two cols, something is wrong")
	}
	
	return(D)
}


getLayoutMatrix <- function( num.row.var, num.col.vars ){
	
	
	# determine the matrix size for the layout 
	nr <-  1 + num.row.var  
	nc <- 1 + 3 * num.col.vars
	
    # col labels spread over three
	#header <- c(1, unlist( lapply ( 2:(num.col.vars+1), FUN = rep, times = 3 ) ) ) # param name takes up 3 columns 
	#ki2j1 <- tail( header, 1 ) + 1  
	#vals <- c(header, seq( from = ki2j1, by = 1, length.out = (nr-1) * nc ) )  
	
    vals <- 1 : (nr * nc)	
	
	M <- matrix( data =vals, nrow = nr, ncol = nc, byrow = TRUE) 
	
	return( M )
}

plotWord <- function(w, ...){
	
	if( is.numeric(w) ) {
		
		w <- format( w, digits = 3)
	} 
	
	plot(c(0,1),c(0,1), type = 'n', 
			xaxt='n', yaxt='n', xlab = '', ylab = '', 
			frame.plot = FALSE )
	text( .5, .5, w, ...) 
	
}

#'@param matrix or data.frame with two columns, x and y values  
plotSparkline <- function( xy ){	

	dimxy <- dim(xy)
	N <- dimxy[1]

	if( dimxy[2] != 2 ) stop(" input matrix xy must have two columns")

	# give a 10% buffer in the vertical 
	yr <- range(xy[,2])
	ybuff <- ( yr[2] - yr[1] ) * 0.10 
	ylimits <- c( yr[1] - ybuff, yr[2] + ybuff )

	# give a 1% buffer in the horizontal 
	xr <- range(xy[,1])
	xbuff <- (xr[2] - xr[1]) * 0.01
	xlimits <- c(xr[1] - xbuff, xr[2] + xbuff ) 

	plot(xy, type = 'l', col = 'gray',  
			xaxt='n', yaxt='n', xlab = '', ylab = '', 
			ylim =  ylimits, xlim = xlimits,
			frame.plot = FALSE )
	points( xy[1,1], xy[1,2], pch = 16, cex = .9 )
	points( xy[N,1], xy[N,2], pch = 16, cex = .9 )  
}



