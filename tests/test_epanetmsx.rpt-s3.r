
#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

source("../R/msxFuncs.r")
source("../R/rptFuncs.r")
source("../R/epanetmsx.rpt-s3.r")

context("epanetmsx.rpt object")
test_that( "epanetmsx.rpt-s3 reads",{
			
			mr <- epanetmsx.rpt( "example.rpt")
			expect_equal("epanetmsx.rpt", class(mr))

            # check some specifics around results for link 5
            expect_equal("5", unique(mr$linkResults$ID)) 
            expect_equal(0, min(mr$linkResults$timeInSeconds)) 
            expect_equal(172800, max(mr$linkResults$timeInSeconds)) 

})

test_that( "timeInSeconds ",{
			
			mr <- epanetmsx.rpt( "example.rpt")
			expect_equal("integer", class(mr$nodeResults$timeInSeconds))
})

test_that("no links",{
			mr <- epanetmsx.rpt( "example-noLinks.rpt")
			mrs <- summary(mr) 
			
			expect_equal( 0, mrs$numLinks)
			expect_true( is.null( mrs$uniqueLinkIDs))
			expect_true( is.null( mrs$linkTimeRangeInSeconds))
			expect_true( is.null( mrs$linkTimestep))
			expect_true( is.null( mrs$linkResSmry))
})


test_that("no nodes",{
			mr <- epanetmsx.rpt( "example-noNodes.rpt")
			mrs <- summary(mr) 
			
			expect_equal( 0, mrs$numNodes)
			expect_true( is.null( mrs$uniqueNodeIDs))
			expect_true( is.null( mrs$nodeTimeRangeInSeconds))
			expect_true( is.null( mrs$nodeTimestep))
			expect_true( is.null( mrs$nodeResSmry))
})

test_that(" no title works ",{


	    	mr <- epanetmsx.rpt( "example-noTitle.rpt")
            mrs <- summary(mr) 
			expect_true( is.null( mrs$Title))
})

test_that(" plot works" ,{
			
			x <- epanetmsx.rpt( "example.rpt")
			plot(x)
                 plot(x,Nodes = 'C')
                 plot(x,Nodes = 'C', Links = NULL )  
                 plot(x,Nodes = NULL, Links = '5' )  		
})

#test_that("plot works for another case",{
     x <- read.msxrpt("5deg.msxrpt")

pairs(x$nodeResults[,3:8])
x$nodeResults

	 plot(x)
#})

test_that("sparklines plot of results"),{
	
     x <- read.msxrpt("5deg.msxrpt")
	 sx <- summary(x)
 
 nr <- 12
 nc <- 16
 
 nspec <- 5

header <- c(1, unlist( lapply ( 2:(nspec+1), FUN = rep, times = 3 ) ) ) # param name takes up 3 columns 
ki2j1 <- tail( header, 1 ) + 1  
vals <- c(header, seq( from = ki2j1, by = 1, length.out = (nr-1) * nc ) )  
 
 M <- matrix( data =vals, nrow = nr, ncol = nc, byrow = TRUE)
 

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
	
	plotSpark <- function( x ){
		N <- length(x)
		plot(x, type = 'l', col = 'gray',  
				xaxt='n', yaxt='n', xlab = '', ylab = '', 
				frame.plot = FALSE )
		points( 1, x[1], pch = 16 )
		points( N, x[N], pch = 16 )  
	}

   # create the plot grid 
	par( mar = c(0,0,0,0), oma = rep(1,4))
	layout( mat = M, respect = FALSE  )
	# plot the header 
	plotWord("Node", cex = 1.5  )
	for( j in 1:nspec){
		plotWord(sx$nodeSpecies[j], cex =1.5  )
	}
	
	# put IDs in a decent order
    IDs <- sort(as.integer(sx$uniqueNodeIDs))
	
	nIDs <- length(IDs)
	
	for( j in 1:nIDs){
		# node id 
		plotWord(IDs[j])	
		
		for( k in 1:nspec){
			# first param, start value 
			paramTimeSeries <- subset( x$nodeResults, ID == IDs[j], select = c( sx$nodeSpecies[ k ], 'timeInSeconds') ) 
			# make sure we're in the right order 
			xx <- paramTimeSeries[ order(paramTimeSeries$timeInSeconds) ,1]
			plotWord( head(xx,1) ) 
			plotSpark( xx )
			plotWord( tail(xx,1) ) 
		}
	}

 
 
 
 
 
 
 
 
 }