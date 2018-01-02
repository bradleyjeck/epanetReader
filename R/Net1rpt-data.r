#************************************
#
#  (C) Copyright IBM Corp. 2015, 2018
#
#  Author: Bradley J Eck
#
#************************************

#' Epanet's Net1 Example
#'
#' A dataset created by reading the Net1.rpt file
#' distributed with Epanet using this package's
#' read.rpt() function.
#' 
#' @name Net1rpt 
#' @docType data
#' @usage Net1rpt
#' @format An object of class \code{epanet.rpt} created by \link{read.rpt}.
#' @examples
#' #confirm built-in dataset matches output of read.rpt
#' rpt <- file.path( find.package("epanetReader"), "extdata","Net1.rpt") 
#' n1r <- read.rpt(rpt)
#' ok <- isTRUE( all.equal(Net1rpt, n1r))
#' if( ok==FALSE) stop("built-in Net1rpt doesn't match read.rpt")
NULL
