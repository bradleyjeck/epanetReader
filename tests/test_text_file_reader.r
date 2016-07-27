#************************************
#
#  (C) Copyright IBM Corp. 2016
#
#  Author: Bradley J Eck
#
#************************************

source("../R/text_file_reader.r")
context("text file reader")

test_that("same result as readLines",{
			
		x <- "Net1.inp"	
		rl	<- readLines( x)
		rcl <- read_char_lines(x)
		pass <- identical( rl, rcl)	
		expect_true(pass)
		})

test_that(" Net2.rpt same result as readLines",{
			
		x <- "Net2.rpt"	
		rl	<- readLines( x)
		rcl <- read_char_lines(x)
		pass <- identical( rl, rcl)	
		expect_true(pass)
		})

# profile 