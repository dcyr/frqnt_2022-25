################################################################################
################################################################################
### Some code to produce a area extent within which we fetch NFI plots for 
### study area calibration
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

require(raster)
require(rgeos)
require(tidyverse)
a <- "temperate-2a-3b"#"temperate-2a-3b" #"mixedwood-042-51"#c("Hereford", "Maskinonge", "ForMont")

df <- read.csv("../data/SOC-database.csv")#/log_Pools.csv")
lookuptbl <- read.csv("../data/lookupTable_vegPot-landtypes.csv", na.strings = "")
landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
landtypeRAT <- read.table(paste0("../inputsLandis/landtypes_", a, ".txt"),
                          comment.char = ">",
                          skip = 1)
colN <- c("active", "MapCode", "Name", "Description")
colnames(landtypeRAT) <- colN



ltRAT <- landtypeRAT %>%
  #filter(MapCode != 0) %>%
  left_join(lookuptbl, by = c("MapCode" = "codInd"))


################################################################################
################################################################################
#### uncomment to replace description in landtypes RAT file
# out <- ltRAT %>%
#   mutate(Description = ifelse(MapCode == 0, Description, ind)) %>%
#   select(active, MapCode, Name, Description)
# 
# fName <- paste0("landtypes_", a, ".txt")
# # Write everything to the file
# writeLines(c("LandisData Ecoregions",
#            "",
#            c(paste(">", paste(colN, collapse = " ")))),
#              fName)
# # Append the data frame (without column names, to avoid redundancy)
# write.table(out, fName, append = TRUE, row.names = FALSE,
#             col.names = FALSE, quote = FALSE)



################################################################################
################################################################################
#### assign substitute VEG_POT for those without obs.
#### and SOC database by VEG_POT
socSummary <- df %>%
  group_by(VEG_POT) %>%
  summarise(n= n(),
            SOC_mineral = mean(SOC_mineral),
            SOC_organic = mean(SOC_organic))

df <- left_join(ltRAT, socSummary) %>%
  filter(MapCode != 0) %>%
  mutate(VEG_POT_socRef = ifelse(is.na(substVEG_POT), VEG_POT, substVEG_POT)) %>%
  select(MapCode, Name, Description, VEG_POT_socRef) %>%
  left_join(socSummary, by = c("VEG_POT_socRef" = "VEG_POT")) %>%
  pivot_longer(cols = "SOC_mineral" | "SOC_organic",
               names_to = "orgMin",
               values_to = "SOCTotal_tonnesPerHa") %>%
  mutate(orgMin = gsub("SOC_", "", orgMin))
           
           #### some landtypes may not have observations available, in which case a subsitute is designated

write.csv(df, file = "DOM-initPools_obs.csv", row.names = F)



################################################################################
#
  

#### fetch dom decay parameters
domDecayParams <- read.csv("../data/DOM-decayParams.csv")

foo <- domDecayParams %>%
  #mutate(poolID = as.factor(poolID)) %>%
  group_by(poolID) %>%
  summarise(decayRate = mean(decayRate_Hararuk),
            q10 = mean(q10_Hararuk))

  
  head()




foo <- initDOMprop %>%
  group_by(landtype, orgMin) %>%
  summarise(SOCTotalSpinup = sum(amountAtT0)/100,
            SOCTotalObs = sum(amountAtT0Updated)/100) %>%
  filter(!is.na(orgMin)) %>%
  mutate(ratio = SOCTotalSpinup / SOCTotalObs) %>%
  


summary(foo)

foo2 <- initDOMprop %>%
  group_by(landtype, poolID) %>%
  summarise(SOCTotalSpinup = sum(amountAtT0)/100) %>%
  group_by(poolID) %>%
  summarize(SOCTotalSpinup = mean(SOCTotalSpinup))



foo2 %>% summarise()


df %>%
  group_by(orgMin) %>%
  summarise(SOCTotal = mean(SOCTotal_tonnesPerHa)) %>%
  summary()



head(df)




as.data.frame(initDOMprop)

foo <- filter(initDOMprop, landtype == 305)
foo <- foo[complete.cases(foo),]
  

sum(foo$propDOM)

head(initDOMprop)

summary(initDOMprop)

%>%
  mutate(pool =  names(domPools)[match(poolID), domPools])
  group_by(landtype, spp, )



 

           
################################################################################
################################################################################
#### Assing DOM values based on ForCS spinup

domPools <- c(VF_A = 1,
             VF_B = 2,
             Fast_A = 3,
             Fast_B = 4,
             MED = 5,
             Slow_A = 6,
             Slow_B = 7,
             Sng_Stem = 8,
             Sng_Oth = 9,
             Extra = 10)

x <- df %>%
 filter(Time == 0) %>%
 group_by(ecoregion, species) %>%
 summarise(VF_A = mean(VF_A),
           VF_B = mean(VF_B),
           Fast_A = mean(Fast_A),
           Fast_B = mean(Fast_B),
           MED = mean(MED),
           Slow_A = mean(Slow_A),
           Slow_B = mean(Slow_B),
           Sng_Stem = mean(Sng_Stem),
           Sng_Oth = mean(Sng_Oth),
           Extra = mean(Extra)) %>%
 gather(key = "pool", value = "amountAtT0", -ecoregion, -species) %>%
 mutate(landtype = ecoregion,
        spp = species,
        poolID = domPools[match(pool, names(domPools))],
        amountAtT0 = round(amountAtT0)) %>%
 ungroup() %>%
 select(landtype, spp, poolID, amountAtT0) %>%
 arrange(spp, landtype, poolID)

write.csv(x, file = paste0("DOM-initPools_", a, ".csv"), row.names = F)
           
#### some landtypes may not have observations available, in which case a subsitute is designated



x <- table(values(landtypes))
x[order(x)]
foo <- lookuptbl[grepl("NDI", lookuptbl$VEG_POT),]
foo$codInd

sum(values(landtypes) %in% foo$codInd, na.rm = T)





l


foo <- 


# 1 "Very Fast Aboveground"
# 2 "Very Fast Belowground"
# 3 "Fast Aboveground"
# 4 "Fast Belowground"
# 5 "Medium"
# 6 "Slow Aboveground"
# 7 "Slow Belowground"
# 8 "Stem Snag"
# 9 "Other Snag"
# 10 "Extra pool"


################################################################################
################################################################################
#### Assing DOM values based on ForCS spinup

domPools <- c(VF_A = 1,
              VF_B = 2,
              Fast_A = 3,
              Fast_B = 4,
              MED = 5,
              Slow_A = 6,
              Slow_B = 7,
              Sng_Stem = 8,
              Sng_Oth = 9,
              Extra = 10)

x <- df %>%
  filter(Time == 0) %>%
  group_by(ecoregion, species) %>%
  summarise(VF_A = mean(VF_A),
            VF_B = mean(VF_B),
            Fast_A = mean(Fast_A),
            Fast_B = mean(Fast_B),
            MED = mean(MED),
            Slow_A = mean(Slow_A),
            Slow_B = mean(Slow_B),
            Sng_Stem = mean(Sng_Stem),
            Sng_Oth = mean(Sng_Oth),
            Extra = mean(Extra)) %>%
  gather(key = "pool", value = "amountAtT0", -ecoregion, -species) %>%
  mutate(landtype = ecoregion,
         spp = species,
         poolID = domPools[match(pool, names(domPools))],
         amountAtT0 = round(amountAtT0)) %>%
  ungroup() %>%
  select(landtype, spp, poolID, amountAtT0) %>%
  arrange(spp, landtype, poolID)
  
write.csv(x, file = paste0("DOM-initPools_", a, ".csv"), row.names = F)

