#************************************
#
#  (C) Copyright IBM Corp. 2016
#
#  Author: Bradley J Eck
#
#************************************


#' Write .inp file 
#' 
#' Write an epanet.inp object to a file 
#'
#' @export
#' @param x epanet.inp object to write 
#' @param file the name of the file where object is written  
#' @details 
#' Writes an epanet.inp object to a file suitable for simulation
#' with EPANET. 
#' @return nothing
#' @examples
#' write.inp(Net1, "Net1-fromR.inp")
#' n1 <- read.inp("Net1-fromR.inp")
#' all.equal(Net1, n1)
#' 

write.inp <- function(x, file){

   cx <- class(x)
   if( cx != "epanet.inp" ) stop("x is not of class epanet.inp") 

   file.create(file) 

  nx = names(x) 
  N <- length(x) 
  for( i in 1:N){ 
    section_name = toupper(nx[i])
    tag <- paste0("[", section_name, "]") 
    write( tag, file, append = TRUE) 
    writeData( x[[i]], file, tag ) 
    write("", file = file, append =TRUE)     

  }
  write("[END]", file =file, append =TRUE)     
}

writeData <- function(y, file, tag){

  cy <- class(y) 

 if( cy == 'character' ){
   # write each entry as a line 
   write(y, file, append = TRUE, ncolumns = 1 ) 
 } else if( cy == 'data.frame'){  
   utils::write.table(y, file, append = TRUE, na = "",
             quote = F, col.names=F, row.names = F, sep = "\t")
 } else if( tag == "[OPTIONS]") {
     ny <- names(y) 
     nyf <- gsub("_", " ", ny)
     s <- paste(nyf,y)
     # don't write options stored as NA 
     s <- s[ which( is.na(y) == FALSE ) ]
     write(s, file, append=TRUE,file=file, ncolumns = 1 )
 } else if( tag == "[PATTERNS]"){
   np <- length(y)
   for( i in 1:np){
      s <- paste( names(y)[i], y[[i]])
      write(s,file,append=TRUE,file=file,ncolumns=1)
   } 
 } else if( tag == "[CURVES]") {
    
      ynames <- names(y) 
      NC <- length(ynames) 
     
      for( i in 1:NC){
         curv <- y[[i]] 
         cname <- ynames[i]
         cx <- as.numeric( curv[[1]] ) 
         cy <- as.numeric( curv[[2]] )  
         N <- length(cx)
         s <-  cbind( rep(cname,N), cx, cy)   
         utils::write.table(s, file, append=TRUE, 
                     quote = F, row.names=F, col.names = F )        
      } 


}

}
