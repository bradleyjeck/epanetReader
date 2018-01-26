#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

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
			expect_true(is.sparkline(M))
			
			M <- sparkline( Theoph, 'Subject', 1, 'conc', xvar = 'Time')
			expect_equal(M[1,1], 0)
			expect_equal(class(M), 'sparkline')
			expect_true(is.sparkline(M))
			
		})

test_that("sparkline plotting",{
			
			
			
			xy <- sparkline( Theoph, 'Subject', 1, 'conc', xvar = 'Time')
			expect_true(is.sparkline(xy))
		    plot(xy )	
			
		})
