################################################################################
################################################################################
### Preparation of cropped study area raster (based on a shapefile)
### Dominic Cyr
#############
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/frqnt_2022-25", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)
require(sf)
require(raster)

gisDir <- "../gis/data"
areas <- "temperate-2a-3b"
lNames <- c("studyArea_temperate_2a-3b_Buffered_-77k_Convex.shp",
            "studyArea_boreal_5a_Buffered_-95k_Convex.shp",
            "studyArea_mixedwood_042-51_Dissolved_Buffered_-52k_Convex.shp")

a <- areas

for (a in areas) {
  landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
  x <- list.files(gisDir)
  ## making string matching a little less fussy
  aStr <- gsub("_|-", " ", a)
  x <- lNames[grep(aStr, gsub("_|-", " ", lNames))]
  # st_layers(dsn = "D:/SIFORT/TES_Geometrie.gdb")
  p <- read_sf(dsn = paste(gisDir, x, sep = "/"))
  p <- st_transform(p, st_crs(landtypes))
  
  r <- mask(landtypes, p)
  r <- trim(r)
  r[!is.na(r)] <- 1
  plot(r)
  writeRaster(r, file = paste0("studyArea_", a, "_cropped.tif"),
                              NAflag = 0, datatype="INT4S", overwrite=TRUE)
}


