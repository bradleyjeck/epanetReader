#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

# File:  test_rptFuncs.r
#

context("test helper functions used with rpt ")

test_that("testing binBreaker",{
		  vals1 <- c(-1,0,1,2,3,4,5)
		  v1bb <- binBreaker(vals1, 4)
		  expect_true( v1bb$Breaks[1] < min(vals1), "first break point is less than min value")
		  
		  expect_equal( (substr(v1bb$Labels[1], 2,3)),  as.character(min(vals1)), "lower range of first bin is sample min")
		  
		  expect_equal( ( substr( v1bb$Labels[4], 6,6)) ,
		                as.character(max(vals1)), 
						"upper end of last bin is sample max")
		
		  vals2 <- c(0,1.1, 2.2, 3.3, NA, 2, 3, 4, 5, 6.6)
		 expect_true ( class(binBreaker(vals2, 3)) == "list", "function returns even with NA inpus ") 
		  
		})


test_that("two line heading for ID works",{
          
          #some sample input
aFewLines <- c(          
"Node Results at 0:00 Hrs: ",
"  ---------------------------------------------------------------------- ",
"  Node                Demand      Head  Pressure   Quality               ",
"  ID                     GPM        ft       psi      mg/L               ",
"  ---------------------------------------------------------------------- ",
"  10                    0.00   1004.35    127.54      0.50               ",
"  11                  150.00    985.23    119.26      0.50               ",
"  12                  150.00    970.07    117.02      0.50               ",
"  13                  100.00    968.87    118.67      0.50               ",
"  21                  150.00    971.55    117.66      0.50               ",
"  22                  200.00    969.08    118.76      0.50               " )

df <- .section2df(aFewLines)

expect_equal( names(df)[1], expected = "ID") 
expect_equal( names(df)[5], expected = "Quality") 

        })


test_that("one line heading for ID works",{

          #some sample input
aFewLines <- c(          
"Node Results at 0:00:00 hrs:  ",
"  -------------------------------------------------------- ",
"                     Demand      Head  Pressure  Chlorine  ",
"  Node                  gpm        ft       psi      mg/L  ",
"  -------------------------------------------------------- ",
"  10                    0.00   1004.35    127.54      0.50               ",
"  11                  150.00    985.23    119.26      0.50               ",
"  12                  150.00    970.07    117.02      0.50               ",
"  13                  100.00    968.87    118.67      0.50               ",
"  21                  150.00    971.55    117.66      0.50               ",
"  22                  200.00    969.08    118.76      0.50               " )

df <- .section2df(aFewLines)

expect_equal( names(df)[1], expected = "ID") 
expect_equal( names(df)[5], expected = "Chlorine") 

        })


test_that(" pct sign in heading",{
          #some sample input
aFewLines <- c(          
"  Node Results at 0:00:00 hrs: ",
"  --------------------------------------------------------",
"                     Demand      Head  Pressure    % from ",
"  Node                  gpm        ft       psi      Lake ",
"  --------------------------------------------------------",
"  10                   0.00    145.52     -0.64      0.00 ",
"  15                 620.00    125.81     40.65      0.00 ",
"  20                   0.00    158.00     12.57      0.00 ",
"  35                1637.00    145.74     57.73      0.00 ")

df <- .section2df(aFewLines)

expect_equal( names(df)[1] , "ID")
expect_equal( names(df)[5] , "Pct_from_Lake")

})



test_that("IDs are characters",{
		
			## Manually input due to complexity of reading and breaking into sections
			aFewLines <- c(          
					"Node Results at 0:00:00 hrs:  ",
					"  -------------------------------------------------------- ",
					"                     Demand      Head  Pressure  Chlorine  ",
					"  Node                  gpm        ft       psi      mg/L  ",
					"  -------------------------------------------------------- ",
					"  10                    0.00   1004.35    127.54      0.50               ",
					"  11                  150.00    985.23    119.26      0.50               ",
					"  12                  150.00    970.07    117.02      0.50               ",
					"  13                  100.00    968.87    118.67      0.50               ",
					"  21                  150.00    971.55    117.66      0.50               ",
					"  22                  200.00    969.08    118.76      0.50               " )
			
			df <- .section2df(aFewLines)
			
			expect_true(class(df$ID) == "character")
		})

test_that(" gui rpt file cleaner",{
			
			
			allLines <- readLines("Net1-gui.rpt")
			cleanLines <- cleanRptLines(allLines)
			expect_equal(class(cleanLines), "character")
			x <- length(allLines) - 19 - 20 - 35
			expect_equal(length(cleanLines), x)
		})

test_that(" gui rpt file cleaner on a clean file",{
			
			
			allLines <- readLines("Net1.rpt")
			cleanLines <- cleanRptLines(allLines)
			expect_equal(class(cleanLines), "character")
			x <- length(allLines) -1 
			expect_equal(length(cleanLines), x)
		})


context("converting timestamps to seconds")

test_that(" two colons parse to HH:MM:SS",{
			
			ts <- " 2:30:44"
            x <- 2*3600+30*60+44
			a<- .timeStampToSeconds(ts)
			
			expect_equal( x, a)
		})


test_that(" 0:00 parses as HH:MM",{
			
			ts <- " 2:30"
            x <- 2*3600+30*60
			a <- .timeStampToSeconds(ts)
			
			expect_equal( x, a)
		})

test_that( "  Node Results at 2:00 Hrs:",{
			
			tsl <- "  Node Results at 2:00 Hrs:"
			a <- .getTimeStamp(tsl)
			expect_equal( "2:00", a)
			
		})

test_that(" 00:00:00:00 gives error",{
			
			ts <- " 2:30:11:22"
			expect_error( .timeStampToSeconds(ts) )
		})

test_that(" 00 gives error",{
			
			ts <- " 22"
			expect_error( .timeStampToSeconds(ts) )
		})

test_that(" returns integer",{
			
			ts <- " 2:33"
			x <- .timeStampToSeconds(ts)
			expect_equal('integer', class(x))
			
		})
