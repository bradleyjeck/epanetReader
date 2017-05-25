
# document some benchmarking work
# this file is written to call with testthat
# but is not part of the default test suite 
# as the file name does not begin with test_

library(microbenchmark)
library(testthat)
context("benchmark file reading   ")

## bje implementation for test purposes
read_char_lines <- function( file ){
	
	size <- file.info(file)$size
	val <- readChar( file, size, TRUE)
	
	has_windows_ending <- grepl("\r\n", val)
	has_unix_ending <- grepl("\n", val)
	if( has_windows_ending ) { 
		sp <-   strsplit(val, "\r\n")
		cv <- sp[[1]]
	} else if( has_unix_ending ){
		# assume unix ending
		sp <-  strsplit(val, "\n" ) 
		cv <- sp[[1]]
	} else {
		# didn't work for some reason so default to readLines
		cv <- readLines( file )
	}
	return (cv)
}

test_that("benchmark file reading",{
		library(Kmisc)
		library(readr)
			
		x <- file.path(R.home("doc"), "COPYING")
		
		
		
		mb <- microbenchmark(
				baseReadLines = readLines(x),
				bje_read_char_lines = read_char_lines(x),
		        Kmisc_readlines = Kmisc::readlines(x),
		        readr_read_lines = readr::read_lines(x),
				
				times = 50
		) 
		
		print(mb)	
			
		})


