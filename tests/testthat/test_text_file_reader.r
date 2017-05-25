#************************************
#
#  (C) Copyright IBM Corp. 2016
#
#  Author: Bradley J Eck
#
#************************************

context("text file reader")

test_that("same result as readLines",{
			
		x <- "Net1.inp"	
		rl	<- readLines(x)
		rlw <- read_lines_wrapper(x)
		pass <- identical( rl, rlw)	
		expect_true(pass)
		})

test_that(" Net2.rpt same result as readLines",{
			
		x <- "Net2.rpt"	
		rl	<- readLines( x)
		rlw <- read_lines_wrapper(x)
		pass <- identical( rl, rlw)	
		expect_true(pass)
		})
