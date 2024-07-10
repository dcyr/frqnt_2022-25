################################################################################
################################################################################
### Generating snags init communities for ForCS 
### test
### Dominic Cyr and Jane Adim
#############
rm(list = ls())
home <- path.expand("~")
#home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/frqnt_2022-25", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(tidyverse)
require(raster)
#require(rgeos)

a <- "temperate-2a-3b"#c("Hereford", "Maskinonge", "ForMont")

