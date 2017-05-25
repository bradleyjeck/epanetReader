#************************************
#
#  (C) Copyright IBM Corp. 2016
#
#  Author: Bradley J Eck
#
#************************************


context("write.inp")

test_that("class check works",{
 expect_error( write.inp(mtcars), file = "mtcars" ) 

})


test_that("Net1 writes",{

 NET1 <- suppressWarnings(read.inp("Net1.inp" ))

  write.inp(NET1, file = "writeNet1.inp") 
  n1 <- suppressWarnings( read.inp("writeNet1.inp") )

  expect_true(  identical( NET1$Junctions, n1$Junctions)   )
  expect_true( all.equal(NET1, n1) )

})

test_that("Net 2", {

  NET2 <- suppressWarnings( read.inp("Net2.inp"))
  write.inp(NET2, "writeNet2.inp")
  n2 <- suppressWarnings( read.inp( "writeNet2.inp"))
  expect_true(all.equal(NET2, n2) )
})

test_that("Net 3", {

  NET3 <- suppressWarnings( read.inp("Net3.inp"))
  write.inp(NET3, "writeNet3.inp")
  n3 <- suppressWarnings( read.inp( "writeNet3.inp") ) 
  expect_true(all.equal(NET3, n3) )
})


test_that("oneprv",{
  op <- read.inp("oneprv.inp")
  write.inp(op, "writeOneprv.inp") 
  xx <- read.inp("writeOneprv.inp")
  expect_true( all.equal(op, xx) ) 
})
