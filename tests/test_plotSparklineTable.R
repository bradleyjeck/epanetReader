# TODO: Add comment
# 
# Author: Brad
###############################################################################


source("../R/plotSparklineTable.r")
source("../R/epanetmsx.rpt-s3.r")
source("../R/msxFuncs.r")
source("../R/rptFuncs.r")

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


test_that("sparkline data matrix",{
			
#			df = Theoph
#			row.var = 'Subject'
#            this.row.var = '1' 
#			col.var = 'conc'
#            xvar = NULL 
			
			M <- sparklineData( Theoph, 'Subject', 1, 'conc', xvar = NULL)
			expect_equal(M[1,1], 1)
			expect_equal(class(M), 'matrix')
			
			M <- sparklineData( Theoph, 'Subject', 1, 'conc', xvar = 'Time')
			expect_equal(M[1,1], 0)
			expect_equal(class(M), 'matrix')
		})

test_that("msx example is ok",{
			
		mr <- read.msxrpt("example.rpt")	
	#windows()	
		plotSparklineTable( mr$nodeResults, row.var = 'ID', col.vars = c("AS5", "AStot", "NH2CL"))
			
		})

test_that("5deg example is ok",{
			mr <- read.msxrpt("5deg.msxrpt")
		plotSparklineTable( mr$nodeResults, row.var = 'ID', col.vars = names(mr$nodeResults)[3:7])
		title("5deg", outer = T)
			
		})

test_that("datasets::Orange",{
			plotSparklineTable( Orange, row.var = 'Tree', col.vars = c('age','circumference'))
			plotSparklineTable( Orange, row.var = 'Tree', col.vars = 'circumference', xvar = 'age' )
		})

test_that("datasets::CO2",{
			
			plotSparklineTable( CO2, row.var = 'Plant', col.vars = c('conc', 'uptake'))
		})
test_that("datasets::Loglolly",{
			
			plotSparklineTable( Loblolly, row.var = 'Seed', col.vars = 'height', xvar = 'age')
			
		})

test_that("datasets::Theoph",{
		
			plotSparklineTable( Theoph, 'Subject', 'conc')
			plotSparklineTable( Theoph, 'Subject', 'conc', xvar = 'Time')
		})

test_that("xvar warning",{
			fail("issue warning if data have different ranges of xvar")
		})

