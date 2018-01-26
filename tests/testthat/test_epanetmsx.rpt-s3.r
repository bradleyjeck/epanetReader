#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

context("epanetmsx.rpt object")
test_that( "epanetmsx.rpt-s3 reads",{
			
			mr <- epanetmsx.rpt( "example.rpt")
			expect_equal("epanetmsx.rpt", class(mr))

            # check some specifics around results for link 5
            expect_equal("5", unique(mr$linkResults$ID)) 
            expect_equal(0, min(mr$linkResults$timeInSeconds)) 
            expect_equal(172800, max(mr$linkResults$timeInSeconds)) 

	    expect_true( is.epanetmsx.rpt( mr))
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
	                expect_true( is.epanetmsx.rpt( x) )
			plot(x)
})

test_that("plot works for another case",{
   x <- read.msxrpt("5deg.msxrpt")
   expect_true( is.epanetmsx.rpt( x) )
   plot(x)
})
