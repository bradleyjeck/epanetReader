#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************


context("sparklineTable-s3")

test_that("argument checking works",{
			
			expect_error( 	sparklineTable( 1:3, "id", c("p1", "p2") ), "df must be a data.frame" ) 
			
			df <- data.frame( ID = c(1,1,1,2,2,2), Param1 = c(1,2,3,4,5,6), Param2 = c(6,5,4,3,2,1))
			row.var = 'ID'
			col.vars = c("Param1","Param2")
			
			expect_error( sparklineTable(df, "node", c("Param1", "Param2")), "node is not a column" )
			expect_error( sparklineTable(df, "ID", c("Param1", "Param3")), "is not a column" )
			
			
			slt <- sparklineTable( df, 'ID', c('Param1', 'Param2')) 
			expect_true( is.sparklineTable( slt))
		})



test_that("xvar warning",{
			
			m1 <- matrix( rnorm(4), 2,2,)
			m2 <- matrix( rnorm(4), 2,2,)
			
			slt <- list( m1, m2)
			
			expect_warning( sparklineDataCheck( slt )) 
			
		})


test_that('plotting works',{
			
			
			slt <- sparklineTable( Loblolly, row.var = 'Seed', col.vars = 'height', xvar = 'age')
			expect_true( is.sparklineTable( slt) ) 	
			plot(slt)
						
					
			
		})



