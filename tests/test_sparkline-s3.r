#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************
source("../R/sparkline-s3.r")

context("sparkline-s3")

test_that("sparkline construction",{
			
			df = Theoph
			id.var = 'Subject'
            ID = '1' 
			yvar = 'conc'
            xvar = NULL 
			
			M <- sparkline( Theoph, 'Subject', 1, 'conc', xvar = NULL)
			expect_equal(M[1,1], 1, check.names = FALSE)
			expect_equal(class(M), 'sparkline')
			
			M <- sparkline( Theoph, 'Subject', 1, 'conc', xvar = 'Time')
			expect_equal(M[1,1], 0)
			expect_equal(class(M), 'sparkline')
			
		})

test_that("sparkline plotting",{
			
			
			
			xy <- sparkline( Theoph, 'Subject', 1, 'conc', xvar = 'Time')
		    plot(xy )	
			
		})