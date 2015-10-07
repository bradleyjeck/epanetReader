#******************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck 
#
#******************************************


source("../R/msxFuncs.r")

context("msx funcs")
test_that( "getID func works",{
			
		marker1 <- "<<< Node 1 >>>"
		expect_equal("1", getID(marker1))
		
		marker2<- "<<< Node D >>>"
		expect_equal("D", getID(marker2))
				
})


test_that("msxSection2df",{
			
			
			sect <-c(	
					" <<< Node D >>>                              ", 
					"                                             ", 
					" Time            AS5       AStot       NH2CL ", 
					" hr:min         UG/L        UG/L        MG/L ", 
					" -------  ----------  ----------  ---------- ", 
					"    0:00        0.00        0.00        0.00 ", 
					"    2:00        0.00        0.00        0.00 ", 
					"    4:00        0.00        0.00        0.00 ", 
					"    6:00        0.00        0.00        0.00 ", 
					"    8:00        0.00        0.00        0.00 ", 
					"   10:00        0.00        0.00        0.00 " )
		
			
		    df <- msxSection2df( sect )	
		 	
			expect_equal("data.frame", class(df) )
			
			expect_equal("integer", class(df$timeInSeconds))
			
			
		})
			
			
	