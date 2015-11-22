#******************************************
#
#  (C) Copyright IBM Corp. 2014
#
#  Author: Bradley J Eck 
#
#******************************************



source("../R/epanet.inp-s3.r")
source("../R/inpFuncs.r")
source("../R/expandedLinkTable-s3.r")


   
   
  context("epanet.inp s3 object") 
test_that("net1.inp reads correctly",
{
   Net1 <- suppressWarnings( read.inp( "../inst/extdata/net1.inp") )
               expect_that( class(Net1), equals("epanet.inp"))
               expect_that( Net1$Curves$`1`$Y , equals(250))
    
})

test_that("Net2.inp reads correctly", {
		
   Net2 <- suppressWarnings( read.inp("Net2.inp") )

			expect_that( class(Net2), equals("epanet.inp"))
            expect_that( Net2$Junctions$Demand[1] , equals(-694.4))
		
		})


		
test_that("read Net3.inp",{
			
			Net3 <- suppressWarnings( read.inp("Net3.inp"))
			
			expect_false( is.null(Net3$Status))
			expect_equal(Net3$Status$Status[1], 'Closed')
		})

context("summary.epanet.inp s3 object") 
test_that(" summary works for Net1 ",
		{
			
   Net1 <- suppressWarnings( read.inp( "net1.inp"))
			sn1 <- summary(Net1)
			expect_that(sn1$entryCounts[1,1], equals(9))
		})

test_that(" summary prints correctly for Net 1",{
			
   Net1 <- suppressWarnings( read.inp( "net1.inp"))
			sn1 <- summary(Net1)
			expect_output(print(sn1), "EPANET Example Network 1")
			expect_output(print(sn1), "Junctions \\s+ 9")
			expect_output(print(sn1), "Coordinates \\s+ 11")
		})

test_that("summary works for Net2",{
			
   Net2 <- suppressWarnings( read.inp("Net2.inp")   )
			sn2 <- summary(Net2)
			expect_output(print(sn2), "Pipes\\s+40")
		})

#test_that("ctown summary",{
#			ctown <- read.inp("ctown.inp")
#			cts <- summary(ctown)
#			expect_output(print(cts), "Coordinates\\s+396")
#		})

test_that("Net3 summary",{
			Net3 <- suppressWarnings( read.inp("Net3.inp"))
			n3s <- summary(Net3)
			expect_output(print(n3s),"Junctions\\s+92")
			expect_output(print(n3s),"Pumps\\s+2")
		})




