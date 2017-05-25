#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************


context("Plot Sparkline Table")

test_that("msx example is ok",{
			
		mr <- read.msxrpt("example.rpt")	
	#windows()	
		plotSparklineTable( mr$nodeResults, row.var = 'ID', col.vars = c("AS5", "AStot", "NH2CL"))
			
		})

test_that("5deg example is ok",{
			mr <- read.msxrpt("5deg.msxrpt")
			
			
		plotSparklineTable( mr$nodeResults, row.var = 'ID', col.vars = names(mr$nodeResults)[3:7], )
			
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
			
			expect_warning( plotSparklineTable( Theoph, 'Subject', 'conc', xvar = 'Time') ) 
		})



