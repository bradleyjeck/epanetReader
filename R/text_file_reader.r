#************************************
#
#  (C) Copyright IBM Corp. 2016
#
#  Author: Bradley J Eck
#
#************************************
#' Read lines wrapper   
#' 
#' Wrapper function for different implementations of
#' readlines functions  
#'
#' @export  
#' @param file the name of the file to read 
#' @return character vector where each entry corresponds to 
#' a line in the file.  
#' @details
#' calls Kmisc::readlines if available and base::readLines otherwise 
read_lines_wrapper <- function( file ){
	
	sz <- base::file.info(file)$size
	size_MB <- sz / 1e6
	
	if( requireNamespace("data.table", quietly = TRUE)){
		
		allLines <- data.table::fread(file, sep=NULL,colClasses = "character", strip.white=F,
		                              header=F,fill=T,data.table=F)[,1]

		
	} else {
		
		if( size_MB > 100){
			warning("Consider installing package data.table to speed up file reading")
		}
		
		allLines <- readLines(file)
		
	}
	
	return (allLines)
}
