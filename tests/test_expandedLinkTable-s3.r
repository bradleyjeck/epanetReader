#******************************************
#
#  (C) Copyright IBM Corp. 2014
#
#  Author: Bradley J Eck 
#
#******************************************


#  File: test_expandedLinkTable-s3.r
#  By  : bradley.eck@ie.ibm.com

source("../R/epanet.inp-s3.r")
source("../R/inpFuncs.r")
source("../R/expandedLinkTable-s3.r")

context("expandedlinkTable s3 object")
test_that("expanedLinkTable works for Net1",
		{
			Net1 <- suppressWarnings(read.inp( "Net1.inp"))
			
			# pipes 
			ept <- expandedLinkTable( Net1$Pipes, Net1$Coordinates)
			expect_that(ept$y2[5], equals(40))
			
			# pumps 
			ept <- expandedLinkTable( Net1$Pumps, Net1$Coordinates)
			expect_that(ept$x1[1], equals(10))

			# valves 
			evt <- expandedLinkTable( Net1$Valvesves, Net1$Coordinates)
		    expect_that(evt, equals(NA))	
			
		}
)
