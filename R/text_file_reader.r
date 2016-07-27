#************************************
#
#  (C) Copyright IBM Corp. 2016
#
#  Author: Bradley J Eck
#
#************************************
#' Read char lines  
#' 
#' Read lines of characters from a file via 
#' the readChar function. 
#' 
#' @export
#' @param file the name of the file to read 
#' @return character vector where each entry corresponds to 
#' a line in the file.  #'
#' @details
#' Checks for windows and unix line endings. 
#' Testing showed this took about half the time of base::readLines()  

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