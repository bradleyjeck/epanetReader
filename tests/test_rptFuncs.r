#************************************
#
#  (C) Copyright IBM Corp. 2014
#
#  Author: Bradley J Eck
#
#************************************

# File:  test_rptFuncs.r
#



source("../R/rptFuncs.r")

context("test helper functions used with rpt ")

test_that("testing binBreaker",{
		  vals1 <- c(-1,0,1,2,3,4,5)
		  v1bb <- binBreaker(vals1, 4)
		  expect_true( v1bb$Breaks[1] < min(vals1), "first break point is less than min value")
		  
		  expect_equal( (substr(v1bb$Labels[1], 2,3)),  as.character(min(vals1)), "lower range of first bin is sample min")
		  
		  expect_equal( ( substr( v1bb$Labels[4], 6,6)) ,
		                as.character(max(vals1)), 
						"upper end of last bin is sample max")
		
		  vals2 <- c(0,1.1, 2.2, 3.3, NA, 2, 3, 4, 5, 6.6)
		 expect_true ( class(binBreaker(vals2, 3)) == "list", "function returns even with NA inpus ") 
		  
		})
