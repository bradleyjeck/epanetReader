
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
})

test_that( "timeInSeconds ",{
			
			mr <- epanetmsx.rpt( "example.rpt")
			expect_equal("integer", class(mr$nodeResults$timeInSeconds))
})