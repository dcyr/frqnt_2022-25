x <- list.files(inputPathLandis)
x <- x[grep("initial-com", x)]
x <- x[grep(".tif", x)]

for (i in seq_along(x)) {
    r <- raster(paste(inputPathLandis, x[i], sep = "/"))
    writeRaster(r, file = x[i], NAflag = 0, datatype="INT4S", overwrite=TRUE)
}




### creating generic stand maps and mgmt maps
inputPathLandis
areas <- "boreal-085-51"
for (a in areas) {
    r <- raster(paste0(inputPathLandis, "/landtypes_", a, ".tif"))
    r[r==0] <- NA
    rVals <- values(r)
    rVals <- rVals[!is.na(rVals)]
    valLength <- length(rVals)
    
    ## stand map
    standMap <- r
    standMap[!is.na(r)] <- 1:valLength
    writeRaster(standMap, file = paste0("stand-map_", a, ".tif"),
                NAflag = 0, datatype="INT4S", overwrite=TRUE)  
    
    ## mgmt maps
    mgmtAreas <- r
    mgmtAreas[!is.na(r)] <- 1
    writeRaster(mgmtAreas, file = paste0("mgmt-areas_", a, ".tif"),
            NAflag = 0, datatype="INT4S", overwrite=TRUE)
    
}

