################################################################################
################################################################################
### Preparation of Forcs simulation file packages
### Dominic Cyr
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
inputDir <- "../inputsLandis/"

a <- "mixedwood-042-51"
x <- list.files(inputDir)



rasterA <- x[grepl(".tif", x)]
rasterA <- rasterA[grepl(a, rasterA)]

rCropped <- raster(paste0("../inputsLandis/landtypes_mixedwood-042-51.tif"))
rasterA <- rasterA[!grepl("studyArea|cropped|landtypes", rasterA)]



for(i in seq_along(rasterA)) {
  r <- raster(paste0(inputDir, "/", rasterA[i]))
  r <- crop(r, rCropped)
  r[is.na(rCropped)] <- NA
  writeRaster(r, file = rasterA[i],
              datatype='INT4S', overwrite=TRUE, NAflag = 0)
}
writeRaster(rCropped, file = paste0("landtypes_", a, ".tif"),
            datatype='INT4S', overwrite=TRUE, NAflag = 0)

plot(rCropped)
plot(r)


