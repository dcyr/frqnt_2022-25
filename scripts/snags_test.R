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

### Jesus' files (updated)
lt_qc <- raster("../data/offline/snags/ltVP_RE.tif") ### landtype vs régions écologiques
sites_qc <- raster("../data/offline/snags/allSites24_250m.tif") ### mapcodes landis


#knn % couverture inv. spp bonifié dendro polygone + classe d'âge + région écologique (<7m? probabiliste)
snags <-  read.csv("../data/offline/snags/snags.csv")
initComm <- read.csv("../data/offline/snags/AllcommunitiesFINAL24_250m.csv")

model.predict <- get(load("../data/offline/snags/rfTestPar.RData"))



### current landtypes
landtypes <- raster(paste0("../inputsLandis/landtypes_",a,  ".tif"))

### cropping new files 
lt <- crop(lt_qc, landtypes)
sites <- crop(sites_qc, landtypes)
### dropping cells outside the study area
lt[is.na(landtypes)] <- sites[is.na(landtypes)] <- NA

###
plot(lt)
plot(sites)
plot(landtypes)

### extracting snags based on sites in the study area
head(snags)
head(initComm)

table(values(sites))
table(values(lt))







# Comment rouler le modèle
# Comment faire la correspondance entre les landtypes (ltVP_RE.tif) et site (allSites)? Raster?
# Confirmer l'encodage des cohortes
# What's "IDE"
# What's "site"
tab

head(foo)
head(snags)


summary(snags)
unique(snags$suivi)



# plot(landtypes)
# plot(lt_jesus, add =T)
