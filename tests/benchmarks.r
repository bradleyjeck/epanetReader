
# document some benchmarking work
# this file is written to call with testthat
# but is not part of the default test suite 
# as the file name does not begin with test_

library(microbenchmark)
context("benchmark file reading   ")

source("../R/text_file_reader.r")

test_that("benchmark reading Net2.rpt",{
			
		x <- "Net2.rpt"
			
		mb <- microbenchmark(
				baseReadLines = readLines(x),
				viaReadChar = read_char_lines(x), 
				
				times = 10
		) 
		
		print(mb)	
		})

test_that("benchmark etmnt.rpt",{
			
		x <- file.path(R.home("doc"), "COPYING")
		
		mb <- microbenchmark(
				baseReadLines = readLines(x),
		        Kmisc_readlines = readlines(x)
		) 
		
		print(mb)	
			
		})


