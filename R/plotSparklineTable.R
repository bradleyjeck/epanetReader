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
#' @param df data.frame with the values  
#' @param row.var variable for rows of the table 
#' @param col.vars variables for columns of the table 
#' @param order.var variable governing the order of the observations
#' @param show.legend logical indicating whether to show a legend
#'        explaining the plot  
#' 
#' @details order.var could be a time stamp or other index, if absent the 
#'          order existing in df is used.  Uses Layout  

plotSparklineTable <- function( df, row.var, col.vars, 
		                        order.var = NULL,
								show.legend = FALSE){
    
	#argument checking
	if( is.data.frame(df) == FALSE) stop("df must be a data.frame")
	if( length( row.var) != 1 ) stop("row.var must have length 1")
    if( (row.var %in% names(df))  == FALSE ) stop(paste("row.var", row.var, "is not a column in data.frame df"))
    if( min( col.vars %in% names(df)) < 1 ) stop(paste("at least one value in col.vars is not a column in data.frame df")) 
	if( is.null(order.var) == FALSE ){ 
      if( (order.var %in% names(df))  == FALSE ) {
		  stop(paste("order.var", order.var, "is not a column in data.frame df"))
	  }
    }
	
	
	urv <- unique( df[ , row.var])
	Nrv <- length(urv ) 	
	Ncv <- length(col.vars)
	M <- getLayoutMatrix( num.row.var = Nrv,
			              num.col.vars = Ncv,  
						  show.legend )
				  
	# create the plot grid 
	par( mar = c(0,0,0,0), oma = rep(1,4))
	layout( mat = M, respect = FALSE  )
	# plot the header 
	plotWord(row.var, cex = 1.5  )
	for( j in 1:Ncv){
		plotWord( col.vars[j], cex =1.5  )
	}
	
	
	# Loop over the rows 
	for( i in 1:Nrv){
		
		plotWord( urv[i]) 
		
		# Loop thru the params 
		for(j in 1:Ncv){
			# first param, start value 
			paramTimeSeries <- df[ which( df[ , row.var] == urv[i]), col.vars[j]  ] 
			# TODO deal with order 
			# make sure we're in the right order 
			xx <- paramTimeSeries
			plotWord( head(xx,1) ) 
			plotSpark( xx )
			plotWord( tail(xx,1) ) 	
		}
	}
	
	
}


getLayoutMatrix <- function( num.row.var, num.col.vars, show.legend ){
	
	if( show.legend == TRUE) stop("implement show.legend")
	
	# determine the matrix size for the layout 
	nr <-  1 + num.row.var  
	nc <- 1 + 3 * num.col.vars
	
	header <- c(1, unlist( lapply ( 2:(num.col.vars+1), FUN = rep, times = 3 ) ) ) # param name takes up 3 columns 
	ki2j1 <- tail( header, 1 ) + 1  
	vals <- c(header, seq( from = ki2j1, by = 1, length.out = (nr-1) * nc ) )  
	
	M <- matrix( data =vals, nrow = nr, ncol = nc, byrow = TRUE) 
	
	return( M )
}

#funcs that get re-used 
plotWord <- function(w, ...){
	
	if( is.numeric(w) ) {
		
		w <- format( w, digits = 2)
	} 
	
	plot(c(0,1),c(0,1), type = 'n', 
			xaxt='n', yaxt='n', xlab = '', ylab = '', 
			frame.plot = FALSE )
	text( .5, .5, w, ...) 
	
}

plotSpark <- function( x  ){
	N <- length(x)
	plot(x, type = 'l', col = 'gray',  
			xaxt='n', yaxt='n', xlab = '', ylab = '', 
			frame.plot = FALSE )
	points( 1, x[1], pch = 16 )
	points( N, x[N], pch = 16 )  
}



