
#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************


source("../R/epanetmsx.rpt-s3.r")

test_that( "epanetmsx.rpt-s3 reads",{
			
			
			mr <- epanetmsx.rpt( "example.rpt")
			
			expect_true("epanetmsx.rpt", class(mr))
			
			
			
			
		})