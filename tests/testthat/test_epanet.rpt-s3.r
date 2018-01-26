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

   expect_true( is.epanet.rpt( Net1res))
})

test_that("net1-gui.rpt reads correctly",
{
Net1res <- read.rpt("Net1-gui.rpt")
            
            expect_that( Net1res, is_a("epanet.rpt"))
            
            expect_that( class(Net1res$nodeResults), equals("data.frame"))
		
			# get the value of P in node 22 for timestep 1:00
			p <- subset(Net1res$nodeResults, Timestamp == "1:00" & ID == "22", select = Pressure)
			expect_that( as.numeric(p), equals(120.07)  )
			
			# flow in pump 9 at 24 hrs 
			q <- subset( Net1res$linkResults, Timestamp =="24:00" & ID == "9", select = Flow)
			expect_that(as.numeric(q), equals(1892.24))
})

test_that("another version of net1  with page breaks reads ",{

			Net1res <-  read.rpt("Net1-pagebrks.rpt") 	
            expect_that( Net1res, is_a("epanet.rpt"))
            
            expect_that( class(Net1res$nodeResults), equals("data.frame"))
		
			# get the value of P in node 22 for timestep 1:00
			p <- subset(Net1res$nodeResults, Timestamp == "1:00:00" & ID == "22", select = Pressure)
			expect_that( as.numeric(p), equals(120.07)  )
			
			# flow in pump 9 at 24 hrs 
			q <- subset( Net1res$linkResults, Timestamp =="24:00:00" & ID == "9", select = Flow)
			expect_that(as.numeric(q), equals(1892.24))

})
test_that("Net1.rpt and Net1-gui.rpt are equivalent",{
			
            n1r <- read.rpt("Net1.rpt")
			n1rg <- read.rpt("Net1-gui.rpt")
			actual <- all.equal(n1r,n1rg)
			expected <- c( "Component \"nodeResults\": Names: 1 string mismatch", 
						   "Component \"nodeResults\": Component \"Timestamp\": 275 string mismatches", 
						  "Component \"linkResults\": Names: 4 string mismatches", 
						 "Component \"linkResults\": Length mismatch: comparison on first 8 components",           
						 "Component \"linkResults\": Component 5: Modes: character, numeric",          
						 "Component \"linkResults\": Component 5: Attributes: < target is NULL, current is list >",
						 "Component \"linkResults\": Component 5: target is character, current is factor",
						 "Component \"linkResults\": Component 6: 325 string mismatches",        
						 "Component \"linkResults\": Component 7: Modes: numeric, character",                      
						 "Component \"linkResults\": Component 7: target is numeric, current is character",        
						 "Component \"linkResults\": Component 8: 'current' is not a factor")
					
					
					
					expect_equal(length(actual), length(expected))
		})



test_that("Net2.rpt reads correctly",{
            net2res <- read.rpt("Net2.rpt")
		})

test_that("Net2-gui.rpt reads",{
			
			Net2res <- read.rpt("Net2-gui.rpt")
		})

test_that("Net2.rpt and Net2-gui.rpt are equivalent",{
			
            n2r <- read.rpt("Net2.rpt")
			n2rg <- read.rpt("Net2-gui.rpt")
		
		    s <- summary(n2r)	
			
			sg <- summary(n2rg)
			
			actual <- all.equal(s, sg)
			
			expected <-c("Component \"juncSummary\": Attributes: < Component \"dimnames\": Component 2: 1 string mismatch >",
			             "Component \"tankSummary\": Attributes: < Component \"dimnames\": Component 2: 1 string mismatch >" )
			
			expect_equal(length(actual), length(expected))
			
		})

test_that("Net3.rpt reads",{
			expect_warning(read.rpt("Net3.rpt"), "Node results not found")
		})


test_that("Net3-nodes.rpt has correct col names",{
			
			n3nr <- read.rpt("Net3-nodes.rpt")
			node_result_names <- names(n3nr$nodeResults)
			expect_equal(node_result_names[1], "ID" )
			expect_equal(node_result_names[2], "Demand" )
			expect_equal(node_result_names[3], "Head" )
			expect_equal(node_result_names[4], "Pressure" )
			expect_equal(node_result_names[5], "Pct_from_Lake" )
			
		})
test_that("Net3-gui.rpt reads",{
			
			n3r <- read.rpt("Net3-gui.rpt")
			n3r <- read.rpt("Net3-nodes.rpt")
			node_result_names <- names(n3r$nodeResults)
			expect_equal(node_result_names[1], "ID" )
			expect_equal(node_result_names[2], "Demand" )
			expect_equal(node_result_names[3], "Head" )
			expect_equal(node_result_names[4], "Pressure" )
			expect_equal(node_result_names[5], "Pct_from_Lake" )
		})

test_that("Net3.rpt and Net3-gui.rpt are equivalent",{
			
            n3r <- read.rpt("Net3-nodes.rpt")
			n3rg <- read.rpt("Net3-gui.rpt")
			actual <- 
					all.equal(n3r$nodeResults,n3rg$nodeResults)
			expected <- c( "Names: 1 string mismatch",                       
							 "Component \"Timestamp\": 2425 string mismatches")
		    expect_equal(length(actual),length( expected))
		})
context("read.rpt error checking")


test_that("missing both node and link results gives error",{

  expect_error( suppressWarnings( read.rpt("Net1-noResult.rpt") ) )

})

test_that("missing node results gives warning",{

  expect_warning(  read.rpt("Net1-noNodes.rpt") ,  "Node results not found" )

})

test_that("missing link results gives warning",{

  expect_warning( read.rpt("Net1-noLinks.rpt"), "Link results not found") 

})



context("summary.epanet.rpt s3 object")
test_that("net1.rpt summary is ok",
		{
            n1res <- read.rpt( "Net1.rpt")
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
