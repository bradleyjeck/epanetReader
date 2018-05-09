context("write the data that ships with the package") 
test_that("Net1",{
  
  Net1 <- suppressWarnings( read.inp("Net1.inp"))
  
  expect_true( is.epanet.inp(Net1)) 
  save(Net1, file="Net1.rdata") 
  
}) 

test_that("Net1rpt",{
  
  Net1rpt <- suppressWarnings( read.rpt("Net1.rpt"))
  
  expect_true( is.epanet.rpt(Net1rpt)) 
  save(Net1rpt, file="Net1rpt.rdata") 
  
}) 