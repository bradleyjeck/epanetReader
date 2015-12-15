#******************************************
#
#  (C) Copyright IBM Corp. 2014
#
#  Author: Bradley J Eck 
#
#******************************************
#  File: rptFuncs_tests.r
#
#  By: bradley.eck@ie.ibm.com
#
# Purpose: tests for reading .rpt files 
#

# package used for testing 
library(testthat)

# assume working dir is epanetReader/tests
source("../R/rptFuncs.r")
source("../R/epanet.rpt-s3.r")
source("../R/epanet.inp-s3.r")
source("../R/inpFuncs.r")



context("epanet.rpt s3 object")
test_that("net1.rpt reads correctly",
{
Net1res <- read.rpt("Net1.rpt")
            
            expect_that( Net1res, is_a("epanet.rpt"))
            
            expect_that( class(Net1res$nodeResults), equals("data.frame"))
		
			# get the value of P in node 22 for timestep 1:00
			p <- subset(Net1res$nodeResults, Timestamp == "1:00:00" & ID == "22", select = Pressure)
			expect_that( as.numeric(p), equals(120.07)  )
			
			# flow in pump 9 at 24 hrs 
			q <- subset( Net1res$linkResults, Timestamp =="24:00:00" & ID == "9", select = Flow)
			expect_that(as.numeric(q), equals(1892.24))
})


test_that("Net2.rpt reads correctly",{
            Net2res <- read.rpt("Net2.rpt")
		})


test_that("Net3.rpt reads",{
			expect_warning(read.rpt("Net3.rpt"), "Node results not found")
		})


context("read.rpt error checking")

test_that("page breaks give an error",{

  expect_error( read.rpt("Net1-pagebrks.rpt"), "Page breaks not allowed") 	

})

test_that("missing both node and link results gives error",{

  expect_error( suppressWarnings( read.rpt("Net1-noResult.rpt") ) )

})

test_that("missing node results gives warning",{

  expect_warning(  read.rpt("Net1-noNodes.rpt") ,  "Node results not found" )

})

test_that("missing link results gives warning",{

  expect_warning( read.rpt("Net1-noLinks.rpt"), "Link results not found") 

})


#test_that("SB25-det-max-dmd.rpt reads",{
#			sb25res <- read.rpt("SB25-det-max-dmd.rpt")
#		})

context("summary.epanet.rpt s3 object")
test_that("net1.rpt summary is ok",
		{
            n1res <- read.rpt( "net1.rpt")
			n1rs <- summary(n1res)
		   expect_output( print(n1rs), "25 time steps")	
		   expect_output( print(n1rs), "Median :\\s+113.08" )	
		   expect_output( print(n1rs), "Max.\\s+:3.210" )	
		})

test_that("Net2.rpt summary is ok",{
			
			Net2res <- read.rpt("Net2.rpt")
			sn2r <- summary(Net2res)
			expect_output(print(sn2r), "Fluoride")
		    expect_output(print(sn2r), "Mean\\s+:0.2767")	
			
			
		})

#test_that("ctown.rpt summary is ok",{
#			ctownres <- read.rpt("ctown.rpt")
#			scr <- summary(ctownres)
#			expect_output(print(scr), "1244.18")
#		})

test_that("Net3.rpt summary is ok",{
			Net3res <- suppressWarnings( read.rpt("Net3.rpt") )
			sn3 <- summary(Net3res)
			expect_output(print(sn3), "0 time steps")
			expect_output(print(sn3), "25 time steps")
			expect_output(print(sn3), "13205.64")
		})


context("plotting simulation results")

test_that("plot args for Net1.rpt",{
			
			Net1res <- read.rpt("Net1.rpt")
			inp <- suppressWarnings( read.inp("Net1.inp"))
			
			expect_error(plot(Net1res,inp,juncQty="junk"), "juncQty not present in nodeResults")
			expect_error(plot(Net1res,inp,linkQty="junk"), "linkQty not present in linkResults")
			expect_error(plot(Net1res,inp,juncQty=NA), "use NULL")
			expect_error(plot(Net1res,inp,linkQty=NA), "use NULL")
			
			plot(Net1res,inp)
			
		})

test_that("plot Net2.rpt",{
          n2 <- suppressWarnings( read.inp("Net2.inp") ) 
          n2r <- read.rpt("Net2.rpt")
          plot(n2r,n2)

        })

test_that("rpt plot w valves",{
			
			v <- read.inp("oneprv.inp")
			vr <- read.rpt("oneprv.rpt")
			plot(vr,v)
		})


context("IDs are characters in rpt")

test_that("node IDs are characters",{
		
			# Net 1 
			rpt <- read.rpt("Net1.rpt")
			expect_true( class(rpt$nodeResults$ID) == "character")
		
			# Net 2
			rpt <- read.rpt("Net2.rpt")
			expect_true( class(rpt$nodeResults$ID) == "character")
		   
			
		})

test_that("link IDs are characters ",{
			# Net 3
			rpt <- suppressWarnings(read.rpt("Net3.rpt"))
			expect_true( class(rpt$linkResults$ID) == "character")
			
		}) 
