
allLines <- readLines("oneprv.inp")

VERTICES <- function( allLines){
tag <- "\\[VERTICES\\]"
df <- .inpSection2df(tag,allLines) 

names(df) <- c("ID", "X.coord", "Y.coord") 

return(df)

}


vert <- VERTICES(allLines)

op <- read.inp("oneprv.inp") 

ept <- expandedLinkTable( op$Pipes, op$Coordinates)


vert1 <- subset(vert, ID == 1 ) 

v1l <- as.list( vert1[,2:3] )

ept$vert <-NA

ept$vert[1] <- I(v1l) 

as.list(vert)
unlist(vert)

unique(vert$ID)

as.list(vert)

vertlist

which( vert[1] 


sx <- split( vert[,'X.coord'], vert$ID)

sxd <- data.frame( ID = names(sx), 
                   X.vertices = I(sx) )  

sy <- split( vert[,'Y.coord'], vert$ID)
syd <- data.frame( ID = names(sy), Y.vertices = I(sy)) 

vd <- merge( sxd, syd)


merge( ept, vd )

