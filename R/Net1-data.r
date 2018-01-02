#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

#' Epanet's Net1 Example
#'
#' A dataset created by reading the Net1.inp file
#' distributed with Epanet using this package's
#' read.inp() function.
#' 
#' @name Net1
#' @docType data
#' 
#' @usage Net1
#' 
#' @format An object of class \code{epanet.inp} created by \link{read.inp}.
#' @source http://www.epa.gov/sites/production/files/2014-06/en2setup_0.exe
#' @examples
#' #confirm built-in dataset matches output of read.inp
#' inp <- file.path( find.package("epanetReader"), "extdata","Net1.inp") 
#' n1 <- suppressWarnings( read.inp(inp) )
#' ok <- isTRUE( all.equal(Net1, n1))
#' if( ok==FALSE) stop("built-in Net1 doesn't match read.inp")
NULL
