#************************************
#
#  (C) Copyright IBM Corp. 2015
#
#  Author: Bradley J Eck
#
#************************************

library(epanetReader)

Net1 <- readInpFile( "Net1.inp")

summary(Net1)

plot( Net1 ) 

Net1$Junctions

summary(Net1$Junctions)


#look at the results 

n1res <- readRptFile( "Net1.rpt") 

summary( n1res ) 


plot( n1res ) 


