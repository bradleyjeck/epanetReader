
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
          fail("what if no links")
})


test_that("no nodes",{
          fail("what if no nodes")
})

test_that(" summary works",{


	    	mr <- epanetmsx.rpt( "example.rpt")
            summary(mr) 
          fail(" what if no links")
          fail(" what if no nodes")
})

test_that(" plot works" ,{
          fail()
})


