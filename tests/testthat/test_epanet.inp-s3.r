#******************************************
#
#  (C) Copyright IBM Corp. 2014
#
#  Author: Bradley J Eck 
#
#******************************************

context("epanet.inp s3 object") 
test_that("net1.inp reads correctly",
{
   Net1 <- suppressWarnings( read.inp( "Net1.inp") )
               expect_that( class(Net1), equals("epanet.inp"))
               expect_that( Net1$Curves$`1`$Y , equals(250))
			   expect_that( length(Net1$Controls), equals(2))
			   expect_that( dim(Net1$Quality)[1], equals(11))

   expect_true( is.epanet.inp( Net1) )
    
})

test_that("Net1-gui.inp reads with warning",{
			
			expect_warning( n1 <- read.inp("Net1-gui.inp"))
			
		})

test_that("Net2.inp reads correctly", {
		
   Net2 <- suppressWarnings( read.inp("Net2.inp") )

			expect_that( class(Net2), equals("epanet.inp"))
            expect_that( Net2$Junctions$Demand[1] , equals(-694.4))
			expect_that( Net2$Sources$Type[1], equals("CONCEN"))
			expect_false( is.null(Net2$Report))
		
		})

test_that("read Net3.inp",{
			
			Net3 <- suppressWarnings( read.inp("Net3.inp"))
			
			expect_false( is.null(Net3$Status))
			expect_true(Net3$Status$Status[1] == 'Closed')
		})

test_that("icdm.inp",{


  net <- suppressWarnings( read.inp("icdm13.inp") ) 

  numPipes <- dim( net$Pipes)[1] 
  expect_equal( numPipes, 21 ) 

})



context("summary.epanet.inp s3 object") 
test_that(" summary works for Net1 ",
		{
			
   Net1 <- suppressWarnings( read.inp( "Net1.inp"))
			sn1 <- summary(Net1)
			expect_that(sn1$entryCounts[1,1], equals(9))
		})

test_that(" summary prints correctly for Net 1",{
			
   Net1 <- suppressWarnings( read.inp( "Net1.inp"))
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

test_that("Net3 summary",{
			Net3 <- suppressWarnings( read.inp("Net3.inp"))
			n3s <- summary(Net3)
			expect_output(print(n3s),"Junctions\\s+92")
			expect_output(print(n3s),"Pumps\\s+2")
		})

context("plot.epanet.inp works")
test_that("Plot Net 1 labels",{
			x  <- suppressWarnings(read.inp("Net1.inp"))
			plot(x , plot.labels=T)
		})
test_that("Plot Net 2 labels",{
			x  <- suppressWarnings(read.inp("Net2.inp"))
			plot(x , plot.labels=T)
		})
test_that("Plot Net 3 labels",{
			x  <- suppressWarnings(read.inp("Net3.inp"))
			plot(x , plot.labels=T, link.lwd=1, link.col='red')
		})
