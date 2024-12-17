

dhpToAge_fnc <- function(sp,
                         landtypes,
                         dataPath = "../scripts/data") {
    
    
    require(tidyverse)
    require(sars)
    require(minpack.lm)
    
    #require(sars) 
    
    df <- read.csv(paste(dataPath, "DHP_AGE_brut_qc.csv", sep = "/")) %>%
        filter(!is.na(AGE),
               AGE <= 350,
               DHP <= 800)
    
    vegCodes <- read.csv(paste(dataPath, "vegCodes.csv", sep = "/"))
    
    
    df <- df %>%
        merge(vegCodes, by.x = "ESSENCE", by.y = "qcCode", all.x = T)
    
    
    ### where does that come from...? Can' remember... spp to check are commented
    ageDeltaAt1m <- c(ABIE.BAL = 12,
                      ACER.RUB = 4,
                      ACER.SAH = 6,
                      ACER.SAC = 4,###
                      BETU.ALL = 4,
                      BETU.PAP = 5,
                      BETU.POP = 4,
                      FAGU.GRA = 16,
                      FRAX.AME = 9,###
                      FRAX.NIG = 6,###
                      FRAX.PEN = 6,###
                      LARI.LAR = 8,
                      OSTR.VIR = 9,###
                      PICE.GLA = 18,
                      PICE.MAR = 12,
                      PICE.RUB = 18,
                      PINU.BAN = 7,
                      PINU.RES = 8,
                      PINU.STR = 10,
                      POPU.SPP = 4,
                      POPU.TRE = 4,###
                      POPU.GRA = 4,###
                      POPU.BAL = 4,
                      QUER.RUB = 9,
                      THUJ.SPP.ALL = 26,
                      TSUG.CAN = 25)
    
    
    sppGroup <- c(ABIE.BAL = "borFast",
                  ACER.RUB = "temp",
                  ACER.SAH = "temp",
                  ACER.SAC = "temp",
                  BETU.ALL = "temp",
                  BETU.PAP = "borFast",
                  BETU.POP = "temp",
                  FAGU.GRA = "temp",
                  FRAX.AME = "temp",
                  FRAX.NIG = "temp",
                  FRAX.PEN = "temp",
                  LARI.LAR = "borFast",
                  OSTR.VIR = "temp",
                  PICE.GLA = "borSlow",
                  PICE.MAR = "borSlow",
                  PICE.RUB = "borSlow",
                  PINU.BAN = "borFast",
                  PINU.RES = "temp",
                  PINU.STR = "temp",
                  POPU.TRE = "borFast",
                  POPU.GRA = "temp",
                  POPU.BAL = "temp",
                  QUER.RUB = "temp",
                  THUJ.SPP.ALL = "temp",
                  TSUG.CAN = "temp")
    
    
    df[,"ageDelta"] <- ageDeltaAt1m[df$LandisCode]
    df[,"ageCorr"] <-  df$AGE + df[,"ageDelta"]
    df[,"group"] <-sppGroup[df$LandisCode]
    
    # subsetting within geographical area (buffer 100 km)
    crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    r <- landtypes
    
    ### creating coarse raster (faster processing)
    rCoarse <- r
    rCoarse[] <- NA
    res(rCoarse) <- 5000
    
    rCoarse <- resample(r, rCoarse)
    rCoarse[!is.na(rCoarse)] <- 1
    
    ### extending raster
    rExt <- raster(extend(extent(r), c(250000, 250000,
                                       250000, 250000)),
                   resolution = res(rCoarse),
                   crs = crs(r))
    
    rCoarse <- resample(rCoarse, rExt)
    rCoarse[] <- 1
    # rBuffer <- buffer(rCoarse, width = 100000,
    #                   doEdge = T,
    #                   dissolved = T)
    rBuffer <- projectRaster(rCoarse, crs = CRS(crs), method = "ngb")
    
    
    
    p <- as(rBuffer, 'SpatialPolygons')  
    
    
    
    cNames <- c("LONGITUDE", "LATITUDE")
    
    
    
    xy <- SpatialPointsDataFrame(coords = df[,cNames],
                                 data = df[,which(!colnames(df) %in% cNames)],
                                 proj4string = CRS(crs))
    xy <- over(p, xy)[,c("AGE", "ageCorr", "ESSENCE", "DHP", "group")]
    
    ### Chapman-Richards model fit
    AGEtoDBH_model <- list()
    for (g in unique(sppGroup)) {
        sppLandis <- names(sppGroup[which(sppGroup == g)])
        
        df <- xy[complete.cases(xy),] %>%
            filter(group == g,
                   AGE <= 300
            ) %>%
            arrange(AGE)
        
        
        
        if(nrow(df)<10) {
            if(nrow(df)==0) {
                stop(paste0("no observations for ", g, ". Consider increasing the size of area"))
            }
            warning(paste("number of observation < 10 for", g))
            
        }
        AGE <- df$AGE
        DHP <- df$DHP
        fitLin <- lm(DHP ~ 0 + AGE)
        print(paste("fitting linear model for", g, "species group"))
        print(paste("fitting 4 parameter Chapman-Richard model for age-dbh relationship for ", g, "species group"))
        fitCR <- nlsLM(
            DHP ~ A * (1 - exp(-k * (AGE - t0)))^m,
            data = df,
            start = list(A = quantile(df$DHP, .9), k = 0.05, m = 1, t0 = 0),#
            lower = c(0, 0, 0, -Inf), # Lower bounds
            upper = c(Inf, Inf, Inf, Inf),#, # Upper bounds
            control = nls.lm.control(maxiter = 300, ftol = 1e-8, ptol = 1e-8),
            weights = 1/AGE
            
        )
        
        
        AGEtoDBH_model[[g]] <- list()
        AGEtoDBH_model[[g]][["obs"]] <- df
        AGEtoDBH_model[[g]][["model"]] <- fitCR
        AGEtoDBH_model[[g]][["modelType"]] <- "Chapman-Richards"
        AGEtoDBH_model[[g]][["sppLandis"]] <- sppLandis
        AGEtoDBH_model[[g]][["ageDelta"]] <- ageDeltaAt1m[sppLandis]
    }
    
    
    #############################################################################
    ### plotting fit for verification purposes
    
    for(i in seq_along(AGEtoDBH_model)) {
        g <- names(AGEtoDBH_model)[i]
        fit <- AGEtoDBH_model[[i]][["model"]]
        
        grDF <- AGEtoDBH_model[[i]][["obs"]] %>%
            mutate(source = "obs")
        
        new <- data.frame(AGE = 0:300, ageCorr = NA,
                          ESSENCE = NA)
        new[, "DHP"] <- predict(fit, new)
        new[, "group"] <- g
        new[, "source"] <- "new"
        
        if(i == 1) {
            df <- rbind(grDF, new)
        } else {
            df <- rbind(df, grDF, new)
        }
        
    }
    df[, "landisSPP"] <- vegCodes[match(df$ESSENCE, vegCodes$qcCode), "LandisCode"]
    
    
    
    p <- ggplot(df, aes(x = AGE, y = DHP, colour = landisSPP)) +
        facet_wrap(vars(group)) +
        theme_bw() +
        geom_point( size = 0.15, alpha = 0.5) +
        theme(plot.caption = element_text(size = rel(.6), hjust = 1),
              axis.text.x = element_text(angle = 45, hjust = 1),
              #legend.title=element_blank()
        ) +
        labs(title = "Age to DBH relationship",
             subtitle = paste0("Study area: ", a),
             #subtitle = paste(areaName, simName),
             x = "Cohort age", y = "DBH (mm)",
             colour = NULL
             #y = expression(paste("tonnes C"," ha"^"-1","\n")),
        )
    
    
    png(filename= paste0("allometry_AGEtoDBH_", a, ".png"),
        width = 12, height = 6, units = "in", res = 600, pointsize=10)
    
    print(p)
    
    dev.off()
    #############################################################################
    return(AGEtoDBH_model)
}



# #### test et visualisation
# sp <- c("Deciduous", "Conifers", "All", "Balsam fir", "American beech", "Black spruce", "Sugar maple", "Trembling aspen")
# dbh <- 1:50
# params <- paramTreeDBH
# x <- biomass_tree_fcn(sp, dbh,
#                       paramTreeDBH = params)
# 
# x <- x %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg),
#            biomassTotal_kg = sum(biomass_kg))
# 
# 
# 
# 
# #### Total aboveground biomass
# require(ggplot2)
# ggplot(data = x, aes(x = dbh, y = biomassTotal_kg, group = Species_en, colour = Species_en)) +# Biomass_kg = a * D_cm^b * H_m^c
#     geom_line() +
#     labs(title = "Tree-level aboveground biomass as a function of DBH",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "total aboveground biomass\n(kg)")
# 
# 
# #### Foliage proportion
# ##################
# df <- x %>%
#     filter(Component_en == "Foliage")
# 
# ggplot(data = df, aes(x = dbh, y = ratio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Foliage proportion of total aboveground biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion") +
#     geom_hline(yintercept = 0.1, linetype = "dotted", colour = "grey25") +
#     geom_text(x = max(dbh),
#               y = 0.1,
#               vjust = -0.5, hjust = 1,
#               label = "ForCS hard-coded setting",
#               colour = "grey25")
# 
# 
# ### Woody biomass total
# df <- x %>%
#     filter(Component_en != "Foliage") %>%
#     group_by(Species_en, dbh) %>%
#     mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
#     filter(Component_en == "Wood") %>%
#     mutate(merchProp = volM_to_volTot(dbh = dbh,
#                                       dTop = 8,
#                                       stumpHeight = .3,
#                                       height = 20))
# 
# ggplot(data = df, aes(x = dbh, y = woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Stem proportion of aboveground woody biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion")
# 
# 
# 
# 
# png(filename = paste0("propStem_dhp.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=10)
# ggplot(data = df, aes(x = dbh, y = merchProp*woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +# Biomass_kg = a * D_cm^b * H_m^c
#     labs(title = "Merchantable proportion of woody biomass",
#          subtitle = paste0("Sources:\nM.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018\n",
#                           "Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada."),
#          x = "dbh (cm)",
#          y = "proportion") +
#     theme(plot.subtitle=element_text(size=rel(0.5)))
# dev.off()
# 
# 
# ###
# df <- x %>%
#     filter(Component_en != "Foliage") %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg)) %>%
#     as.data.frame() %>%
#     select(Species_en, Component_en, dbh, biomass_kg, ratio)
# 
# ggplot(data = df, aes(x = dbh, y = ratio, group = Species_en, colour = Species_en)) +
#     facet_wrap(~Component_en) +
#     geom_line() +
#     labs(title = "Stem proportion of aboveground woody biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion") +
#     geom_hline(yintercept = 0.1, linetype = "dotted", colour = "grey25") +
#     geom_text(x = max(dbh),
#               y = 0.1,
#               vjust = -0.5, hjust = 1,
#               label = "ForCS setting",
#               colour = "grey25")

################################################################################
##### Tree-level from DBH only
# Reference: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018. 
# Reference: Ung, C.-H., Bernier, P., Guo, X.-J. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.
# 
# Parameters for biomass equation for estimating biomass (Kg, Biomass_kg) at the tree scale from DBH (cm, D_cm) and height (m, H_m)
# Biomass_kg = a * D_cm^b * H_m^c





# rm(list = ls())
# ################################################################################
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # for my Windows machine
# wwd <- paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/", Sys.Date() ,sep ="/")
# dir.create(wwd)
# setwd(wwd)
# #################

################################################################################
##### Tree-level from DBH only
# Reference: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018. 
# Reference: Ung, C.-H.; Bernier, P.; Guo, X.-J. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.
# 
# Parameters for estimating biomass (Kg, Biomass_kg) at the tree scale from DBH (cm, D_cm)
# Biomass_kg = a * (D_cm^b)
biomass_tree_fcn <- function(sp,
                             dbh,
                             component = "all",
                             paramPath = "../scripts/data") {
    
    require(tidyverse)
    
    params <- read.csv(paste(paramPath,
                                   "Tree-level allometric equations dbh.csv", sep = "/"),
                             fileEncoding = "Windows-1252", skip = 6, header = T)
    # paramTreeDbhHeight <- read.csv(paste(paramPath,
    #                                      "Tree-level allometric equations dbh-height.csv", sep = "/"),
    #                                fileEncoding = "Windows-1252", skip = 5, header = T)
    # paramPlot  <- read.csv(paste(paramPath,
    #                              "Plot-level allometric equations.csv", sep = "/"),
    #                        fileEncoding = "Windows-1252", skip = 9, header = T)
    
    
    x <- params %>%
        filter(Species_en %in% sp) %>%
        merge(data.frame(dbh = dbh)) %>%
        mutate(biomass_kg = a * dbh^b)
        
    return(x)
}

################################################################################
##### Ratio of merchantable vol to total volume
##### Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada.
### Vm/V = R1 + R2*X3 + R3*X3*
### X3 = (T*T/(D*D*((1-0.04365*B2)**2)))*(1+S/H)
# R1 <- 0.9604
# R2 <- -0.1660
# R3 <- -0.7868
### T = Top diameter (inside, cm)
### D = dbh (outside, cm)
### B2 = 0.154 ###
### S = Stump height (m)
### H = total height (m)


volM_to_volTot <- function(dbh, dTop, stumpHeight, height) {
    D <- dbh
    t <- dTop
    S <- stumpHeight
    H <- height
    
    B2 = 0.154 ###
    
    X3 <- (t*t/(D*D*((1-0.4365*B2)^2))) * (1 + S/H)
    
    R1 <- 0.9604
    R2 <- -0.1660
    R3 <- -0.7868
    
    
    ratio = R1 + R2*X3 + R3*X3^2
    ratio[D<t+2] <- 0
    return(ratio)
}


#
# #### test et visualisation
# #### Sourcing scripts
# require(raster)
# require(RCurl)
# source("../scripts/dhpToAge_fnc.R")
# source("../scripts/CBMtoLANDIS_fnc.R", encoding = "Windows-1252")
# a <- "Maskinonge"
# s <- "baseline"
# 
# forCSInput <- paste0("../inputsLandis/forCS-input_", a, "_", s, ".txt")
# landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
# forCS <- landisInputFetch(forCSInput, type = "ForCS")
# vegCodes <- read.csv("../scripts/data/vegCodes.csv")
# spp <- forCS$SpeciesParameters$table[,1]
# sppAllo <- vegCodes[match(spp, vegCodes$LandisCode), "allometryCode"]
# aParams <- forCS$SpeciesParameters$table[,5]
# bParams <- forCS$SpeciesParameters$table[,6]
# 
# dhpToAge <- dhpToAge_fnc(sp = sppAllo, 
#                          landtypes = landtypes)
# 
# sp <- c("Deciduous", "Conifers", "All", "Balsam fir", "American beech", "Black spruce", "Sugar maple", "Trembling aspen")
# dbh <- 1:50
# 
# x <- biomass_tree_fcn(sp = sppAllo, dbh = dbh)
# 
# x <- x %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg),
#            biomassTotal_kg = sum(biomass_kg))
# 
# 
# #### Total aboveground biomass
# require(ggplot2)
# ggplot(data = x, aes(x = dbh, y = biomassTotal_kg, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Tree-level aboveground biomass as a function of DBH",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "total aboveground biomass\n(kg)")
# 
# 
# #### Foliage proportion
# ##################
# df <- x %>%
#     filter(Component_en == "Foliage")
# 
# ggplot(data = df, aes(x = dbh, y = ratio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Foliage proportion of total aboveground biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion") +
#     geom_hline(yintercept = 0.1, linetype = "dotted", colour = "grey25") +
#     geom_text(x = max(dbh),
#               y = 0.1,
#               vjust = -0.5, hjust = 1,
#               label = "ForCS hard-coded setting",
#               colour = "grey25")
# 
# 
# ### Woody biomass total
# df <- x %>%
#     filter(Component_en != "Foliage") %>%
#     group_by(Species_en, dbh) %>%
#     mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
#     filter(Component_en == "Wood") %>%
#     mutate(merchProp = volM_to_volTot(dbh = dbh,
#                                       dTop = 8,
#                                       stumpHeight = .3,
#                                       height = 20))
# 
# ggplot(data = df, aes(x = dbh, y = woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Stem proportion of aboveground woody biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion")
# 
# #
# 
# 
# png(filename = paste0("propStem_dhp.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=10)
# ggplot(data = df, aes(x = dbh, y = merchProp*woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Merchantable proportion of woody biomass",
#          subtitle = paste0("Sources:\nM.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018\n",
#                           "Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada."),
#          x = "dbh (cm)",
#          y = "proportion") +
#     theme(plot.subtitle=element_text(size=rel(0.5)))
# dev.off()
# 
# 
# #### Sourcing scripts
# require(raster)
# require(RCurl)
# source("../scripts/dhpToAge_fnc.R")
# source("../scripts/CBMtoLANDIS_fnc.R", encoding = "Windows-1252")
# a <- "Maskinonge"
# s <- "baseline"
# 
# forCSInput <- paste0("../inputsLandis/forCS-input_", a, "_", s, ".txt")
# landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
# forCS <- landisInputFetch(forCSInput, type = "ForCS")
# vegCodes <- read.csv("../scripts/data/vegCodes.csv")
# spp <- forCS$SpeciesParameters$table[,1]
# aParams <- forCS$SpeciesParameters$table[,5]
# bParams <- forCS$SpeciesParameters$table[,6]
# minAge <- forCS$SpeciesParameters$table[,4]
# 
# dhp <- df$dbh
# dhpToAge <- dhpToAge_fnc(sp = spp, 
#                          landtypes = landtypes)
# 
# for (i in seq_along(dhpToAge)) {
#     fit <-  dhpToAge[[i]][["model"]]
#     spp <- dhpToAge[[i]]$sppLandis
#     sppAllo <- vegCodes[match(spp, vegCodes$LandisCode), "allometryCode"]
#     x <- df %>%
#         filter(Species_en %in% sppAllo)
#     dhpToAge <- dhpToAge_fnc(sp = sppAllo, 
#                              landtypes = landtypes)
#     x[,"age"] <- ((x$dbh*10)^2)/fit$coefficients
#     for (j in seq_along(spp)) {
#         sp <- spp[j]
#         #
#         index <- which(forCS$SpeciesParameters$table[,1]==sp)
#         aPar <- aParams[index]
#         bPar <- bParams[index]
#         minA <- minAge[index]
#         
#         spAllo <- sppAllo[j] 
#         y <- x %>%
#             filter(Species_en == spAllo)
#         ageCorr <- y$age + dhpToAge[[i]]$ageDelta[[sp]]
#         ageCorr <- data.frame(dhp = y$dbh, ageCorr)
#         
#         tmp <- biomass_tree_fcn(sp = spAllo,
#                                          dbh = y$dbh)
#         tmp <- tmp %>%
#             group_by(Species_en, dbh) %>%
#             mutate(ratio = biomass_kg/sum(biomass_kg),
#                    biomassTotal_kg = sum(biomass_kg))
#         
#         
#         ### Woody biomass total
#         tmp <- tmp %>%
#             filter(Component_en != "Foliage") %>%
#             group_by(Species_en, dbh) %>%
#             mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
#             filter(Component_en == "Wood") %>%
#             mutate(merchProp = volM_to_volTot(dbh = dbh,
#                                               dTop = 7,
#                                               stumpHeight = .15,
#                                               height = 15),
#                    minAge = minA) %>%
#             mutate(propStem = woodyBiomassRatio*merchProp)  %>%
#             merge(ageCorr, by.x = "dbh", by.y = "dhp") %>%
#             filter(ageCorr <= 250) %>%
#             mutate(aParam = aPar,
#                    bParam = bPar,
#                    spLandis = sp)
#         
#         if(i == 1 &
#            j == 1) {
#             biomass_tree <- tmp
#         } else {
#             biomass_tree <- rbind(biomass_tree, tmp)
#         }
#     }
# }
#  
# 
# df <- biomass_tree %>%
#     mutate(propStemLandis = aParam*(1-bParam^ageCorr)) %>%
#     mutate(propStemLandis = ifelse(ageCorr < minAge, 0, propStemLandis))
# png(filename = paste0("propStem_age_landis.png"),
#     width = 10, height = 8, units = "in", res = 600, pointsize=10)
#     
#     ggplot(df, aes(x = ageCorr)) +
#         facet_wrap(~spLandis) +
#         geom_line(aes(y = propStemLandis)) +
#         geom_line(aes(y = propStem), linetype = "dotted") +
#         labs(x = "âge",
#              y = "Proportion marchande",
#              title = "Proportion marchande de la biomasse aérienne ligneuse en fonction de l'âge",
#              subtitle = "Proportion simulée par ForCS (en noir), proportion estimées à partir de relations allométrique et de défilement (pointillée) et paramétrage Landis précédent (en bleu).",
#              caption = "Notez les proportions décroissantes passé un certain âge chez certaines espèces (ex. Quercus). Cela s'explique par la forte biomesse présente dans les branches, \nqui ne sont pas considérées par les modèles de défilement (d'où le choix de simuler un plateau).") +
#         geom_line(aes(y = 0.7546*(1-0.983^ageCorr)), colour = "blue") +
#         theme(plot.subtitle=element_text(size= rel(0.7)),
#               plot.caption = element_text(size= rel(0.6)))
#         
# dev.off()
