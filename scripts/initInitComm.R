#############
rm(list = ls())
home <- path.expand("~")
home <- ifelse(Sys.info()["user"] == "dcyr-z840", gsub("/Documents", "", home), home)
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/frqnt_2022-25", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(tidyverse)


initComm_qcR <- raster("../inputsLandis/offline/provincialInputs/initial-communities_quebec.tif")
initComm_qc <- read.csv("../inputsLandis/offline/provincialInputs/initial-communities_quebec_encoded.csv")

qcCodes <- read.csv("../inputsLandis/qcCodes.csv")
vegCodes <- read.csv("../inputsLandis/vegCodes.csv")

sppCodes <- qcCodes %>%
    rename(Code = code) %>%
    merge(vegCodes) %>%
    dplyr::select(qcCode, LandisCode, commonName, scientificShort)


#for (a in "mixedwood-042-51") {
for (a in c("temperate-2a-3b", "mixedwood-042-51", "boreal-085-51")) {
    
    landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
    
    lt <- landtypes!=0
    
    r <- crop(initComm_qcR, lt)
    r[lt==0] <- 0
    r[is.na(r)] <- 0
    
    writeRaster(r, file = paste0("initial-communities_", a, ".tif"),
                datatype='INT4S', overwrite=TRUE)
    
    
    mapCodes <- unique(values(r))
    mapCodes <- mapCodes[!is.na(mapCodes)]
    mapCodes <- mapCodes[order(mapCodes)]
    
    initComm <- initComm_qc[which(initComm_qc$site %in% mapCodes),]
    
    #mapCodes[which(mapCodes  %in% initComm_qc$site)]
    
    if(length(mapCodes) != nrow(initComm)+1) {
        stop("not all mapCodes matched with a unique init comm")
    }

    ####  fetching spp codes
    spp <- colnames(initComm[, -c(1, ncol(initComm))])
    spp <- data.frame(qcCode = spp) %>%
        merge(sppCodes)

      
    ##############################################################
    ##############################################################
    ##### Creating a list of initial communities
  
    
    initCommTable <- list()
    initCommTable["MapCode 0"] <- ""
    
   
    
    
    for(i in 1:nrow(initComm)) {
        mc <-initComm[i,"site"]
        ic <- initComm[i,-c(1, ncol(initComm))]
        
        ic  <- as.character(ic)
        index <- which(grepl("666", ic)==F)
        
        sppCode <- spp[index, "LandisCode"]
        ic <- ic[index]
        
        ic <- strsplit(ic, "_")
        ic <- lapply(ic, function(x) which(grepl("1", x)))
        cohorts <- lapply(ic, function(x) x*10)
        names(cohorts) <- sppCode
        
        initCommTable[[paste("MapCode", mc)]] <- cohorts
    }
    #########
    save(initCommTable, file = paste0("initial-communities_", a, ".RData"))
    
    ##############################################################
    ##############################################################
    ##### writing to file, LANDIS format
    
    
    mapCodes <- names(initCommTable)

    
    
    sink(paste0("initial-communities_", a, ".txt"))
    cat('LandisData "Initial Communities"')
    cat("\n")
    cat("\n")
    
    
    for(i in seq_along(initCommTable)) {
        cat(paste0(mapCodes[i], "\n"))
        ic <- initCommTable[[i]]
        spp <- names(ic)
        for(j in seq_along(spp)) {
            cat(paste0(spp[j], " ", paste(ic[[j]], collapse = " "), "\n"))
        }
        cat("\n")
    }
    
    sink()
    
}
    



