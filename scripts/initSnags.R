rm(list = ls())
################################################################################
home <- path.expand("~")
home <- ifelse(Sys.info()["user"] == "CyrDo",
               home,
               gsub("/Documents", "", home)) # for my Windows machine
wwd <- paste(home, "Sync/Travail/ECCC/Landis-II/frqnt_2022-25", Sys.Date() ,sep ="/")
dir.create(wwd)
setwd(wwd)

require(tidyverse)
require(raster)
#require(rgeos)
initYear <- 2024

areas <- c("temperate-2a-3b", "mixedwood-042-51")


### snagData setup
snagAgeCls <- c(0, 60, 120, 180)
snagTSD <- c(0, 18, 36, 54)
snagAgeClsLbl <- (head(snagAgeCls, -1) + tail(snagAgeCls, -1))/2
snagTSDLbl <- (head(snagTSD, -1) + tail(snagTSD, -1))/2
snagSpp <- list("temperate-2a-3b" = c("sw" = "PICE.MAR",
                                      "hw" = "BETU.ALL"),
                "mixedwood-042-51" = c("sw" = "PICE.MAR",
                                       "hw" = "BETU.ALL"))
###  mid-cls sampling years for each inventory cycle (as an approx for now)
smplYr <- c(1977,
            1988,
            1997,
            2010,
            2018,
            2020)

#### PEP and PET reference files (from Jesus)
initCommLookupTbl <- read.csv("../data/offline/snags/codeLinks.csv")

# lt_qc <- raster("../data/offline/snags/ltVP_RE.tif") ### landtype vs r?gions ?cologiques
# sites_qc <- raster("../data/offline/snags/allSites24_250m.tif") ### mapcodes landis


###########
vegCodes <- read.csv("../inputsLandis/vegCodes.csv")
qcCodes <- read.csv("../inputsLandis/qcCodes.csv")

snags <-  read.csv("../data/offline/snags/snags.csv")
snagCodes <- unique(snags$ETAT)
allTrees <- read.csv("../data/offline/snags/allTrees.csv") 

####
snagDataTbl <- read.table(paste0("../data/offline/snags/initial-snags_cls.txt"),
                          comment.char = ">", skip = 2)

sw_hw <- c("ABIE.BAL" = "sw",
           "ACER.RUB" = "hw",
           "ACER.SAH" = "hw",
           "BETU.ALL" = "hw",
           "BETU.PAP" = "hw",
           "FAGU.GRA" = "hw",
           "LARI.LAR" = "sw",
           "PICE.GLA" = "sw",
           "PICE.MAR" = "sw",
           "PICE.RUB" = "sw",
           "PINU.BAN" = "sw",
           "PINU.RES" = "sw",
           "PINU.STR" = "sw",
           "POPU.TRE" = "hw",
           "QUER.RUB" = "hw",
           "THUJ.SPP.ALL" = "sw",
           "TSUG.CAN" = "sw")


for (a in areas) {

  ### initComm 
  initComm <- get(load(paste0("../inputsLandis/initial-communities_", a, ".RData")))
  species <- read.table(paste0("../inputsLandis/species_", a, ".txt"),
                        comment.char = ">", skip = 2)
  initCommVals <- unique(values(raster(paste0("../inputsLandis/initial-communities_", a, ".tif"))))
  
  
  
  ###
  sppLookupTbl <- species %>%
    left_join(vegCodes,  by = c("V1" = "LandisCode")) %>%
    left_join(qcCodes, by = c("Code" = "code")) %>%
    mutate(LandisCode = V1,
           nfiCode = Code,
           sw_hw = sw_hw[LandisCode]) %>%
    dplyr::select(LandisCode, sw_hw, qcCode, nfiCode, commonName, scientificShort) %>%
    filter(complete.cases(.))
  
  
  
  #######################################################
  sppLookupTbl[, "snagSp"] <-  snagSpp[[a]][][sppLookupTbl$sw_hw]
  
  allSnags <- allTrees %>%
    filter(ETAT %in% snagCodes) %>%
    left_join(sppLookupTbl, by = c("ESSE" = "qcCode"))
  
  
  
  initSnags <- list()
  initSnags["MapCode 0"] <- ""
  for (i in seq_along(names(initComm))) {
    mapCode <- as.numeric(gsub("[^0-9]", "", names(initComm)[i]))
    if(!mapCode %in% initCommVals) {
      next
    }
    if(mapCode == 0) {
    initSnags[paste("MapCode", mapCode)] <- ""
    next
    }
    site <- filter(initCommLookupTbl, site == mapCode) %>%
      arrange(suivi)
    if(nrow(site) >1 ) {
      site <- site[1,]
    }
    
    x <-  allSnags %>%
      filter(ID_PE %in% site$ID_PE &
             suivi  %in% site$suivi)
    
    isPEP <- site$suivi == "PEP"
  
    if(isPEP) {
      # removing measurement that are posterior to reference stand composition
      # plus keeping the more ancient tree measurement 
      x <- x %>%
        filter(NO_MES<=site$NO_MES &
                 suivi == "PEP") %>%
        group_by(NO_ARBRE) %>%
        slice_max(order_by = NO_MES, n = 1, with_ties = FALSE) %>%
        as.data.frame() %>%
        mutate(annee = ifelse(suivi == "PEP", smplYr[NO_MES],
                              ifelse(suivi == "PET4", smplYr[4],
                                     ifelse(suivi == "PET5", smplYr[5], NA))),
               ageCls = cut(nAGEc,
                            breaks = c(snagAgeCls[-length(snagAgeCls)], Inf),
                            labels = snagAgeClsLbl, 
                            right = F),
               TSD = cut(initYear - annee,
                         breaks = c(snagTSD[-length(snagTSD)], Inf),
                         labels = snagTSDLbl,
                         right = F),
               ageCls =  as.numeric(as.character(ageCls)),
               TSD = as.numeric(as.character(TSD)),
               ageCls = ifelse(TSD == snagTSDLbl[3], ageCls+2,
                                     ifelse(TSD == snagTSDLbl[2], ageCls+1,
                                            ageCls))) %>%
        dplyr::select(snagSp, ageCls) %>%
        arrange(snagSp, ageCls) %>%
        distinct()
    }
    
    
  
    if(nrow(x)>0) {
      for (sp in unique(x$snagSp)) {
        mcString <- paste("MapCode", mapCode)
        initSnags[[mcString]] <- list()
        #sp <- x[j, "snagSp"]
        index <- which(x$snagSp ==sp)
        initSnags[[mcString]][[sp]] <- x[index, "ageCls"]
      }
    }
  }
  save(initSnags, file = paste0("initSnags_", a, ".RData"))
  
  #merge with initial communities (commented to keep the option)
  
  sink(paste0("initial-communities_", a, ".txt"))
  cat('LandisData "Initial Communities"\n')
  
  mapCodes <- names(initComm)
  for(i in seq_along(mapCodes)) {
    mc <- mapCodes[i] 
    cat("\n")
    cat(paste0(mapCodes[i]), "\n")
    ic <- initComm[[mc]]
    spp <- names(ic)
    for(j in seq_along(spp)) {
      cat(paste0(spp[j], " ", paste(ic[[j]], collapse = " ")))
      cat("\n")
    }
    sn <- initSnags[[mc]]
    if(is.null(sn)) {
      next
    }
    spp <- names(sn)
    for(j in seq_along(spp)) {
      cat(paste0(">>", spp[j], ".snag ", paste(sn[[j]], collapse = " ")))
    }
    cat("\n")
  }
    
    sink()
    

  
  ####
  ##create snagData table
  
  snagFileName <- paste0("initial-snags_", a, ".txt")
  
  initSnagTable <- data.frame(species = c(rep(snagSpp[[a]][1], 9),rep(snagSpp[[a]][2], 9)),
             AgeAtDeath = rep(rep(snagAgeClsLbl,3), 2) +
               c(rep(-1,length(snagAgeClsLbl)),rep(0,length(snagAgeClsLbl)), rep(1,length(snagAgeClsLbl))),
             TimeSinceDeath = c(rep(snagTSDLbl[1], 3),rep(snagTSDLbl[2], 3),rep(snagTSDLbl[3], 3)),
             cause = "other") %>%
    arrange(species, AgeAtDeath, TimeSinceDeath)
    #arrange(species, AgeAtDeath, TimeSinceDeath)
  
  sink(snagFileName)
  cat('LandisData "ForC Succession"')
  cat("\n")
  cat("SnagData")
  cat("\n")
  cat(">>	species	AgeAtDeath	TimeSinceDeath	Cause")
  cat("\n")
  sink()
  
  write.table(
    initSnagTable,
    file = snagFileName,
    sep = "\t",
    append = T,
    row.names = F,
    col.names = F,
    quote = F
  )
}


###########################################
###########################################
#######################################


data.frame(icTbl) %>%
  mutate(icVals = as.numeric(as.character(icVals))) %>%
  mutate(snagPresent = icVals %in% as.numeric(gsub("MapCode ", "", names(initSnags)))) %>%
  left_join(initCommLookupTbl, by = c("icVals" = "site")) %>%
  mutate(TYPE_PE =ifelse(is.na(TYPE_PE), suivi, TYPE_PE),
         snagPresent = Freq*snagPresent) %>%
  group_by(TYPE_PE) %>%
  summarise(n = sum(Freq),
            snagPresentProp = sum(snagPresent)/sum(Freq))


