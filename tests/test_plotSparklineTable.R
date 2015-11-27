# TODO: Add comment
# 
# Author: Brad
###############################################################################


source("../R/plotSparklineTable.r")

context("Plot Sparkline Table")

test_that("argument checking works",{
			
		expect_error( 	plotSparklineTable( 1:3, "id", c("p1", "p2") ), "df must be a data.frame" ) 
	
		df <- data.frame( ID = c(1,1,1,2,2,2), Param1 = c(1,2,3,4,5,6), Param2 = c(6,5,4,3,2,1))
		row.var = 'ID'
        col.vars = c("Param1","Param2")
		
		expect_error( plotSparklineTable(df, "node", c("Param1", "Param2")), "node is not a column" )
		expect_error( plotSparklineTable(df, "ID", c("Param1", "Param3")), "is not a column" )
		
		
		plotSparklineTable( df, 'ID', c('Param1', 'Param2')) 
		})


test_that("layout matrix works",{
			
			A <- getLayoutMatrix( 11, 5, show.legend = F)
			expect_equal( A[12,16], 182 )
			
		})
