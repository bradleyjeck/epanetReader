context("write the data that ships with the package") 
test_that("Net1",{
  
  myInp <-  file.path( "../../inst/extdata/Net1.inp") 
  
  Net1 <- suppressWarnings( read.inp(myInp))
  
  expect_true( is.epanet.inp(Net1)) 
  save(Net1, file="Net1.rdata") 
  
}) 

test_that("Net1rpt",{
  
  myRpt <-  file.path( "../../inst/extdata/Net1.rpt") 
  
  Net1rpt <- suppressWarnings( read.rpt(myRpt))
  
  expect_true( is.epanet.rpt(Net1rpt)) 
  save(Net1rpt, file="Net1rpt.rdata") 
  
}) 