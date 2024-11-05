### A function that reads the LANDIS-II 'base-harvest.txt' input file
### and returns the harvest implementation table as a data.frame

fetchBDAParam <- function(x) { #function(x, prescripLvls) {
    
    bdaParam <- list()
    tmp <- readLines(x, skipNul = T)
    tmp <- tmp[!substr(tmp, 1,2)==">>"] ## remove comments
    
    
    indexNull <- which(nchar(tmp)==0) 
    index <- grep("BDASpeciesParameters", tmp)+1
    indexEnd <- indexNull[head(which(indexNull > index), 1)]
    index <- c(index : (indexEnd-1))
    tmp <- tmp[index]
    
    for (i in seq_along(tmp)) {
        y <- strsplit(tmp[[i]], "\\t| ")[[1]]
        y <- y[nchar(y)!=0]
        
        df <- data.frame(species = y[1],
                          minorHostAge = as.numeric(y[2]),
                          minorHostSRDProb = as.numeric(y[3]),
                          secondHostAge = as.numeric(y[4]),
                          secondHostSRDProb = as.numeric(y[5]),
                          majorHostAge = as.numeric(y[6]),
                          majorHostSRDProb = as.numeric(y[7]),
                          class3Age = as.numeric(y[8]),
                          class3VulnProb = as.numeric(y[9]),
                          class2Age = as.numeric(y[10]),
                          class2VulnProb = as.numeric(y[11]),
                          class1Age = as.numeric(y[12]),
                          class1VulnProb = as.numeric(y[13]))
        #beginTime = x[4])
        if(i == 1) {
            bdaParam[["BDASpeciesParameters"]]  <- df
        } else {
            bdaParam[["BDASpeciesParameters"]]  <- rbind(bdaParam[["BDASpeciesParameters"]] , df)
        }
    }
   
    return(bdaParam)
}

