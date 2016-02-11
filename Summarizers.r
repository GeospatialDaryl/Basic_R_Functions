colMeaner <- function(inputCol){
  inputL <-  ! is.na(inputCol)
  mean(inputCol[inputL]) 
}



standardizeTrimmedTable<- function(use){
        medians <- apply(use,2,median,na.rm=TRUE)
        mads <- apply(use,2,mad, na.rm = TRUE)
        use <- scale(use, center=medians, scale = mads)
        use
}

namez<- function(vNames, strMatch = "average"){
        
        vectNamez <- vector(mode="integer")
        k = 1
        vOut <- vector(mode="character")
        for (i in vNames){
                #print (c(i,strMatch))
                pos <- regexpr(strMatch,i)[1]
                vectNamez[k] <- pos
                #print (pos)
                if (pos > 0){
                         print(i)
                         
                         vOut[k]<- i
                         k = k + 1
                }
                
        }
        #vLog <- vectNamez[(vectNamez > 0)]
        #vOut <<- vector(mode = "character")
        #k = 0
        #for (i in vLog){
        #        k = k + 1
        #        
        #        if (i){
        #               rbind(vOut,vNames[k]) 
         #       }
                
        #}
        vOut
}


extractStat <- function(inDF,strMatch){

        vNames <- names(inDF)
        vMatches <- namez(vNames)
        outDF <- subset(inDF, select = vMatches)
                
        outDF
        
}
