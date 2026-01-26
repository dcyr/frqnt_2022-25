
###################### a wrapper that uses several of the functions above to create a formatted input file
#### for Forest Carbon Succession from Biomass Succession input files and CBM Archive Index Database (AIDB)
initForCS <- function(forCSInput, ### a formatted Forest Carbon Succession input file
                      bsMainInput,  ### Biomass succession main inputs
                      bsDynInput, ### Biomass succession dynamic inputs
                      landtypes,
                      landtypes_AT,
                      climate = F,
                      spinup = F,
                      domOBS = F,
                      version = "3.1",
                      scenario,
                      t0,
                      allometry,
                      inputOffset = 0,
                      interpolate = F,
                      includeSnags = F,
                      alignT0withBaseline = F,
                      valuesSingleAll = c("Timestep", "SeedingAlgorithm", "ForCSClimateFile",
                                          "InitialCommunities", "InitialCommunitiesMap",
                                          "SnagFile"),
                      tablesAll = c("ForCSOutput", "SoilSpinUp", "AvailableLightBiomass",
                                    "LightEstablishmentTable", "SpeciesParameters",
                                    "DOMPools", "EcoSppDOMParameters", "ForCSProportions",
                                    "DisturbFireTransferDOM", "DisturbOtherTransferDOM",
                                    "DisturbFireTransferBiomass", "DisturbOtherTransferBiomass",
                                    "ANPPTimeSeries", "MaxBiomassTimeSeries",
                                    "EstablishProbabilities", "RootDynamics"), ...) {### other arguments may be required for some functions
    
    print("Fetching Landis inputs and templates...")
    ### fetching source formatted Landis Biomass Succession inputs
    bsMain <- landisInputFetch(input = bsMainInput, type = "BSMain")
    bsDyn <- landisInputFetch(bsDynInput, type = "BSDynamics")
    forCS <- landisInputFetch(forCSInput, type = "ForCS")
    # # fetching Forest Carbon succession template file
    # x <- readLines(forCSInput)
    print("Done!")
    
    ### preparing species' list and spatial units
    print("Preparing species' list and spatial units...")
    spp <- sppConvert(bsMain$SpeciesParameters$table[,1],
                      inputCode = "LANDIS", aidbURL = aidbURL)
    spu <- spuFetch(landtypes, landtypes_AT,
                    aidbURL = aidbURL,
                    spuURL = spuURL)
    print("Done!")
    
    ############################################################################
    ### updating AvailableLightBiomass (identical format)
    print("Preparing / updating 'AvailableLightBiomass'...")
    forCS$AvailableLightBiomass$table <- bsMain$MinRelativeBiomass$table
    # storing landtype names for further use
    lt <- colnames(forCS$AvailableLightBiomass$table)
    print("Done!")
    
        ############################################################################
    ### updating file names (if necessary)
    print("Preparing / updating 'ForCSClimateFile'...")
    forCS$ForCSClimateFile <-  "forCS-climate.txt"
    print("Done!")
    
    
    ### updating file names (if necessary)
    print("Preparing / updating 'SnagData'")
    if(includeSnags) {

      forCS$SnagFile <-  "init-snags.txt"
      snags <- read.csv(paste0(inputPathLandis, "/initial-snags_", a,".csv"))
      snags[,"TimeSinceDeath"] <- t0-snags$YearOfDeath
      snags<- snags[, c("species", "AgeAtDeath", "TimeSinceDeath", "Cause")]
      if(as.numeric(version) >= 3.1) {
        ### create file
        snags_init_fnc(snags, file = paste0("initial-snags_", a, ".txt"))
        print("Done!") 
        ### remove snag section from main ForCS input file
        forCS <- forCS[-which(names(forCS)=="SnagData")]
      } else {
        forCS$SnagData$SnagData$table <- snags
      }
      
    } else {
      ######### placeholder
      forCS <- forCS[-which(names(forCS)=="SnagFile")]
    }
    print("Done!") 
    

    
    ############################################################################
    ### updating output timestep (if necessary)
    print("Preparing / updating 'ForCSClimateFile'...")
    forCS$ForCSOutput$table <- data.frame(bioPools = 1,
                                          domPools = 1,
                                          flux = 1, ## must be 1
                                          summary = 1) ## must be 1
    print("Done!")
    
    

    
    ############################################################################
    ### updating LightEstablishmentTable (identical format)
    print("Preparing / updating 'LightEstablishmentTable'")
    forCS$LightEstablishmentTable$table <- bsMain$SufficientLight$table
    print("Done!")
    
    ############################################################################
    ### updating SpeciesParameters (different parameters)
    print("Preparing / updating 'SpeciesParameters'")
    forCS$SpeciesParameters$table <- SpeciesParameterFetch(bsMain,
                                                           aidbURL = aidbURL)
    ### rounding shape parameter values 
    forCS$SpeciesParameters$table[,3] <- round(forCS$SpeciesParameters$table[,3], 2)
    forCS$SpeciesParameters$table[,8] <- round(forCS$SpeciesParameters$table[,8], 3)
    
    
    ## minimum age for merchantable stems
    # (should be revised
    
    # merchMinAge <- c("ABIE.BAL" = 20,
    #                  "ACER.RUB" = 20,
    #                  "ACER.SAH" = 20,
    #                  "BETU.ALL" = 20,
    #                  "BETU.PAP" = 20,
    #                  "FAGU.GRA" = 20,
    #                  "LARI.LAR" = 20,
    #                  "PICE.GLA" = 20,
    #                  "PICE.MAR" = 20,
    #                  "PICE.RUB" = 20,
    #                  "PINU.BAN" = 20,
    #                  "PINU.RES" = 20,
    #                  "PINU.STR" = 20,
    #                  "POPU.TRE" = 20,
    #                  "QUER.RUB" = 20,
    #                  "THUJ.SPP.ALL" = 20,
    #                  "TSUG.CAN" = 20)
    # 
    #forCS$SpeciesParameters$table[,4] <- merchMinAge[forCS$SpeciesParameters$table[,1]]
    
    if (allometry) {
      source("../scripts/allometry_fnc.R")
      dhpToAge <- dhpToAge_fnc(sp = spp, landtypes = landtypes)
      vegCodes <- read.csv("../scripts/data/vegCodes.csv")
      sppLandis <- forCS$SpeciesParameters$table[,1]
      
      # ### Chapman-Richard function (reverse, dbh to age)
      # crInverse <- function(y, d, z, c) {
      #   -1 / z * log(1 - (y / d)^(1 / c))
      # }
      # 
      
      # ### Chapman-Richard function (reverse, dbh to age)
      # crInverse <- function(y, d, z, c) {
      #   -1 / z * log(1 - (y / d)^(1 / c))
      # }
      # 
      # Define the inverse Chapman-Richards function
      inverse_chapman_richards <- function(y, A, k, m, t0) {
        #if (any(y >= A | y <= 0)) stop("y must be between 0 and A.")
        t0 - (1 / k) * log(1 - (y / A)^(1 / m))
      }
      
      for(i in seq_along(dhpToAge)) {
        dhp <- 1:100 # cm
        fit <-  dhpToAge[[i]]$model
        type <- dhpToAge[[i]]$modelType
        corr <- dhpToAge[[i]]$ageDelta
        if(type == "linear") {
          age <- (dhp*10)/fit$coefficients
        }
        if(type == "Chapman-Richards") {
          params <- coef(fit)
          
          age <- numeric()
          for(j in dhp) {
            age <-  append(age, inverse_chapman_richards(y = j*10,
                              A = params[1],
                              k = params[2],
                              m = params[3],
                              t0 = params[4])
                           ) 
          }

        }
      
          
        for (sp in dhpToAge[[i]]$sppLandis) {
            ageCorr <- age + dhpToAge[[i]]$ageDelta[[sp]]
            ageCorr <- data.frame(dhp, ageCorr)
            sppAllo <- vegCodes[match(sp, vegCodes$LandisCode), "allometryCode"]
            x <- biomass_tree_fcn(sp = sppAllo,
                                  dbh = dhp)
            x <- x %>%
              group_by(Species_en, dbh) %>%
              mutate(ratio = biomass_kg/sum(biomass_kg),
                     biomassTotal_kg = sum(biomass_kg))
            
            
            ### Woody biomass total
            x <- x %>%
              filter(Component_en != "Foliage") %>%
              group_by(Species_en, dbh) %>%
              mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
              filter(Component_en == "Wood") %>%
              mutate(merchProp = volM_to_volTot(dbh = dbh,
                                                dTop = 7,
                                                stumpHeight = .15,
                                                height = 15)) %>%
              mutate(propStem = woodyBiomassRatio*merchProp)  %>%
              merge(ageCorr, by.x = "dbh", by.y = "dhp") %>%
              filter(ageCorr <= 150)
      
            ## pred
            index <- which.max(x$propStem)
            aParamSeed <- round(x[index, "propStem"], 3) 
            minAge <- round(x[min(which(x$propStem >0)), "ageCorr"])
            # bParam <- ifelse(minAge>25, 0.97,
            #             ifelse(minAge>20, 0.95, 0.925))
            # 
            # bParam <- .9
            # bParam <- .925
            # bParam <- .95
            # bParam <- .975
            # bParam <- .99
            # 
            #### looking for the best pParam value
            aParamVals <- round(seq(from = .85*aParamSeed, to = 1.15*aParamSeed, length.out = 25), 3)
            bParamVals <- seq(from = 0.9, to = 0.99, by =0.005)
            
            ageMin <- c(round(0.8*minAge), round(1.2*minAge))
            ageMin <-  seq(from = min(ageMin),  max(ageMin), by = 1)
              
            paramGrid <- expand.grid(aParamVals, bParamVals, ageMin)
            #
            rsq <- mae <- mse <- numeric()
            
            for (k in 1:nrow(paramGrid)) {
              aParam <- paramGrid[k, 1] 
              bParam <- paramGrid[k, 2] 
              pred <-  aParam*(1-bParam^x$ageCorr)
              predThresh <- pred
              predThresh[x$age<paramGrid[k,3]] <- 0
              if(k == 1) {
                tmp <- predThresh
              } else {
                tmp <- cbind(tmp, predThresh)
              }
              rsq <- append(rsq, cor(x$propStem, predThresh)^2)
              mae <- append(mae, mean(abs((x$propStem-predThresh))))
              mse <- append(mse, mean((x$propStem-predThresh)^2))
            }
            ### minimizing the MAE
            paramBestFit <- paramGrid[which.min(mse),]
            
            ####### plotting 
            dfTmp <- data.frame(spp = sp,
                                ageCorr = x$ageCorr,
                                reference = x$propStem,
                                prediction = tmp[,which.min(mse)],
                                aParam = paramBestFit[,1],
                                bParam = paramBestFit[,2],
                                minAge = paramBestFit[,3])
            if (i == 1 &
                sp == dhpToAge[[1]]$sppLandis[[1]]) {
              dfPlot <- dfTmp
            } else {
              dfPlot <- rbind(dfPlot, dfTmp)
            }
            
            # for (k in 1:ncol(tmp)) {
            #   lines(x$ageCorr, tmp[,k], col = "red")
            # }
            #lines(x$ageCorr, tmp[,which.max(rsq)], col = "blue")
            #lines(x$ageCorr, tmp[,which.min(mae)], col = "green")
            #lines(x$ageCorr, tmp[,which.min(mse)], col = "darkgreen")
            
            index <- which(forCS$SpeciesParameters$table[,1] == sp)
            forCS$SpeciesParameters$table[index,4] <- paramBestFit[,3]
            forCS$SpeciesParameters$table[index,5] <- paramBestFit[,1]
            forCS$SpeciesParameters$table[index,6] <- paramBestFit[,2]
          print(sp)
          }
      }
      
      
      dfPlot <- filter(dfPlot, spp %in% species[,1]) %>%
        pivot_longer(reference:prediction, names_to = "group", values_to = "propMerch") %>%
        mutate(groupLabel = ifelse(group == "prediction", "Landis-II / ForCS", "reference*"))
      

      
      
      p <- ggplot(dfPlot, aes(x = ageCorr, y = propMerch, colour = group)) +
        facet_wrap(vars(spp)) +
        theme_bw() +
        scale_x_continuous(limits = c(0, 150)) +
        #scale_color_manual(name = "Management\nscenario",
        #                   values = cols[[a]]) +
        
        geom_line(linewidth =0.75) + #+
        scale_color_manual(name = "",
                           labels = dfPlot$groupLabel,
                           values = c("darkolivegreen", "darkgoldenrod1"))+
        theme(plot.caption = element_text(size = rel(.6), hjust = 1),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Proportion of AGB that is merchantable",
             subtitle = paste0("Study area: ", a),
             #subtitle = paste(areaName, simName),
             x = "Cohort age", y = "Merchantable fraction of AGB",
             #y = expression(paste("tonnes C"," ha"^"-1","\n")),
             caption = paste0("*Reference values are based on tree-level allometric equations from Ung et al. 2008. Can. J. For. Res 38:1123-2232 and regional dbh to age relationship."))
        
        
      png(filename= paste0("allometry_AGBtoMerch_", a, ".png"),
          width = 9, height = 6, units = "in", res = 600, pointsize=10)
      
      print(p)
      
      dev.off()
      
      
      
      print("Done!")
    } else {
      # Merch Curve shape params a and b (from Dymond et al 2016)
      #forCS$SpeciesParameters$table[,4] <- minAge
      forCS$SpeciesParameters$table[,5] <- NA#0.7546 #Merch curve shape param 'a'
      forCS$SpeciesParameters$table[,6] <- NA#0.983 #Merch curve shape param 'b'
      print("Done!")
      
    }
    
    ############################################################################
    ### Dompools - "Proportion of the decayed material that goes to the atmosphere'
    print("Preparing / updating 'Dompools'")
    tmp <- forCS$DOMPools$table
    tmp[,3] <-  DomFetch(aidbURL = aidbURL)$PropToAtmosphere
    ### updating with Hararuk values(needed to average out values for poolID 8 and 9, i.e. snags)
    tmp[1:9,3] <- c(0.789, 0.669, 0.869, 0.817, 0.793,
                 1, 1, 0.513, 0.659)
  
    
    
    
    # tmp[which(tmp$V2 == "Slow Aboveground"), 3] <- 0.83
    forCS$DOMPools$table <- tmp
    
    print("Done!")
    
    ############################################################################
    ### EcoSppDOMParameters
    print("Preparing / updating 'EcoSppDOMParameters'")
    forCS$EcoSppDOMParameters$table <- EcoSppDOMParametersFetch(sppNames = names(spp),
                                                                landtypeNames = lt)
    
    ### update CBM decay rates and q10 with Hararuk's
    decayRatesHararuk <- read.csv("../data/DOM-decayParams.csv") %>%
      select(spp, poolID, decayRate_Hararuk, q10_Hararuk) %>%
      mutate(poolID = as.factor(poolID)) %>%
      group_by(spp, poolID) %>%
      summarize(decayRateHararuk = unique(decayRate_Hararuk),
                q10Hararuk = unique(q10_Hararuk))
    
    forCS$EcoSppDOMParameters$table <- forCS$EcoSppDOMParameters$table %>%
      left_join(decayRatesHararuk) %>%
      mutate(OrganicMatterDecayRate = decayRateHararuk,
             Q10 = q10Hararuk) %>%
      select(landtype, spp, poolID, OrganicMatterDecayRate, amountAtT0, Q10)
    
    
    # for soil spin-up
    if(spinup) {
        forCS$EcoSppDOMParameters$table[,5] <- 0
    } else {
      if(domOBS) {
        tmp <- read.csv("../inputsLandis/DOM-initPools_obs.csv") ###### integrated in initForCS_fnc.R
        orgMinPools <- c(VF_A = "organic",
                         VF_B = "mineral",
                         Fast_A = "organic",
                         Fast_B = "mineral",
                         MED = "organic",
                         Slow_A = "organic",
                         Slow_B = "mineral",
                         Sng_Stem = "organic",
                         Sng_Oth = "organic",
                         Extra = NA)
        #### Assign DOM values based on proportions suggested by ForCS default spinup
        initDOMprop <- read.csv(paste0("../inputsLandis/DOM-initPools_", a, ".csv"))
        #initDOMprop[,"pool"] <-  names(domPools)[match(initDOMprop$poolID, domPools)]
        initDOMprop <- initDOMprop %>%
          mutate(orgMin = orgMinPools[initDOMprop$poolID]) %>%
          group_by(landtype, orgMin) %>%
          mutate(propDOM = prop.table(amountAtT0)) %>%
          ungroup() %>% 
          left_join(tmp, by = c("landtype" = "MapCode",
                                "orgMin" = "orgMin")) %>%
          mutate(amountAtT0Updated = SOCTotal_tonnesPerHa*propDOM*100)
        
        tmp <- initDOMprop %>%
          mutate(amountAtT0 = round(amountAtT0Updated, 2)) %>%
          select(landtype, spp, poolID, amountAtT0)
        
      } else { ### when using ForCS spinup procedure
        tmp <- read.csv(paste0(inputPathLandis, "/DOM-initPools_", a, ".csv"))
        tmp <- tmp %>%
          mutate(poolID = as.factor(poolID),
                 landtype = as.factor(landtype))
      }
       
        # merging in new data
        tmp  <- forCS$EcoSppDOMParameters$table %>%
            merge(tmp, by = c("landtype", "spp", "poolID"),
                  all = T) %>%
            arrange(spp, landtype, poolID) %>%
            mutate(amountAtT0 = ifelse(is.na(amountAtT0.y),
                                       0, amountAtT0.y)) %>%
            dplyr::select(landtype, spp, poolID,
                   OrganicMatterDecayRate,
                   amountAtT0, Q10)
   
        forCS$EcoSppDOMParameters$table <- tmp
    }
    print("Done!")
    
    ############################################################################
    ### ForCSProportions
    print("Preparing / updating 'ForCSProportions'")
    forCS$ForCSProportions$table <- ForCSProprotionsFetch(landtypes, landtypes_AT,
                                                      aidbURL = aidbURL)
    ### replacing values with those from Hararuk
    forCS$ForCSProportions$table[,1] <- 0.663 #BiomassFine (from Hararuk, Prop of dead fine roots that goes into AGVF, the rest going into BGVF; Hararuk: 0.663; CBM : 0.5)
    forCS$ForCSProportions$table[,2] <- 0.523 #BiomassCoarse (from Prop of dead coarse roots that goes into AGF, the rest going into BGF; Hararuk: 0.523, CBM : 0.5)
    forCS$ForCSProportions$table[,3] <- 0.004 #SlowAGtoSlowBG aka SlowMixingRate (Hararuk: 0.004, CBM = 0.006)
    forCS$ForCSProportions$table[,4] <- 0.066 #SoftwoodStemSnagToDOM  (Hararuk: 0.066, CBM = 0.03)
    forCS$ForCSProportions$table[,5] <- 0.081 #SoftwoodBranchSnagToDOM (Hararuk: 0.081, CBM = 0.1)
    ### so they are hard coded at this moment.
    print("Done!")
    
    
    
    
    ############################################################################
    ############################################################################
    ############################################################################
    #### disturbance matrices
    print("Preparing / updating Disturbance matrices - 'DisturbFireTransferDOM'")
    
    ### DisturbFireTransferDOM
    dm <- DMFetch(landtypes,landtypes_AT,
                  aidbURL = aidbURL,
                  #from = "DOM",
                  forCS_type = "fire")$fromDOM
    for (i in 1:5) {
        df <-  data.frame(Intensity = i, dm)
        if(i == 1) {
            dm_Fire <- df
        } else {
            dm_Fire <- rbind(dm_Fire, df)
        }
    }
    
    forCS$DisturbFireTransferDOM$table <- dm_Fire
    print("Done!")
    
    ### DisturbOtherTransferDOM
    print("Preparing / updating Disturbance matrices - 'DisturbOtherTransferDOM'")
    dmID <- c(harvest = 135, #"97% clearcut","Sylva CPRS clearcut 97%"
              wind = 227, #"Uprooting and decay for All Eco Boundaries","Uprooting and decay for All Eco Boundaries"
              bda = 427)#, # "DMID 427: Spruce Budworm in QC, Severe annual defoliation, 6yr cumulative defoliation > 85%"
              #bdaSalv = 25) # "Stand Replacing Matrix #25","Insects followed by Salvage Logging Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada."
    
    
    for (i in seq_along(dmID)) {
        dt <-names(dmID)[i]
        df <- DMFetch(landtypes,landtypes_AT,
                      aidbURL = aidbURL,
                      #from = "DOM",
                      forCS_type = "other",
                      dmID = dmID[i])$fromDOM
        if(nrow(df) > 0) {
            df <- data.frame(DisturbanceType = dt,
                             df)
                        
            if(i == 1) {
                otherTransferDOM <- df
            } else {
                otherTransferDOM <- rbind(otherTransferDOM, df) 
            }
        }
        rm(df)
    }
    forCS$DisturbOtherTransferDOM$table <- otherTransferDOM
    print("Done!")
    
    ### DisturbFireTransferBiomass
    print("Preparing / updating Disturbance matrices - 'DisturbFireTransferBiomass'")
    dm <- DMFetch(landtypes,landtypes_AT,
                  forCS_type = "fire",
                  aidbURL = aidbURL,)$fromBiomass
    # each lines must sum up to 1, else, C will disappear
    dm[,2:4] <- t(apply(dm[,2:4], 1, function(x) round(x/sum(x), 4)))
    
    for (i in 1:5) {
        df <-  data.frame(Intensity = i, dm)
        if(i == 1) {
            dm_Fire <- df
        } else {
            dm_Fire <- rbind(dm_Fire, df)
        }
    }
    
    forCS$DisturbFireTransferBiomass$table <- dm_Fire
    print("Done!")
    
    ### DisturbOtherTransferBiomass
    print("Preparing / updating Disturbance matrices - 'DisturbOtherTransferBiomass'")
   
    for (i in seq_along(dmID)) {
        dt <-names(dmID)[i]
        df <- data.frame(DisturbanceType = dt,
                         DMFetch(landtypes,landtypes_AT,
                                 #from = "DOM",
                                 forCS_type = "other",
                                 aidbURL = aidbURL,
                                 dmID = dmID[i])$fromBiomass)
        if(i == 1) {
            otherTransferBiomass <- df
        } else {
            otherTransferBiomass <- rbind(otherTransferBiomass, df)
        }
    }
    dm <-otherTransferBiomass
    # each lines must sum up to 1, else, C will disappear
    dm[,3:5] <- t(apply(dm[,3:5], 1, function(x) round(x/sum(x), 4)))
    
    forCS$DisturbOtherTransferBiomass$table <- dm
    print("Done!")
    
    ############################################################################
    #### Dynamic inputs
    
    
    if(interpolate) {
      if(length(unique(bsDyn$year))<2) {
        warning("only one period provided, no interpolation possible")
      } else {
        print("Interpolating dynamic inputs for an annual timestep")
        for (sp in unique(bsDyn$species)) {
          for (l in unique(bsDyn$landtype)) {
            tmp <- filter(bsDyn, landtype == l, species == sp) %>%
              arrange(year)
            
            xout <- min(tmp$year):max(tmp$year)
            
            periodLength <- diff(tmp$year)
            periodLength <- c(periodLength, last(periodLength))
            tmp$year <- tmp$year + periodLength/2
            
            
            
            probEst <- round(as.data.frame(approx(tmp$year, tmp$probEst,  xout = xout))$y, 3)
            probEst[is.na(probEst)] <- probEst[!is.na(probEst)][1]
            maxANPP <- round(as.data.frame(approx(tmp$year, tmp$maxANPP,  xout = xout))$y, 0)
            maxANPP[is.na(maxANPP)] <- maxANPP[!is.na(maxANPP)][1]
            maxB <- round(as.data.frame(approx(tmp$year, tmp$maxB,  xout = xout))$y)
            maxB[is.na(maxB)] <- maxB[!is.na(maxB)][1]
            
            tmp <- data.frame(year = xout, landtype = l, species = sp,
                              probEst, maxANPP, maxB)
            
            
            if(sp == unique(bsDyn$species)[1] &
               l == unique(bsDyn$landtype)[1]) {
              x <- tmp 
            } else {
              x <- rbind(x, tmp)
            }
          }
        }
        x <- arrange(x, year, landtype, species)
        bsDyn <- x
      }
      
    }
    
    
    
    #####
    print("Resetting update year based on an annual timestep...")
    tsCorr <- bsMain$Timestep-1
    tsCorr <- tsCorr - inputOffset
    
    
    f <- function(x) {
      return(as.numeric(x[1])-tsCorr)
    }
    
    
     #### ANPPTimeSeries
    print("Preparing / updating 'ANPPTimeSeries'")
    tmp <- bsDyn[, c("year", "landtype", "species", "maxANPP")]
    tmp[,"year"] <- apply(tmp, 1, f)
    tmp <- tmp[tmp$year>=-tsCorr,]
    if(length(unique(tmp$year)) == 1) {
      if(unique(tmp$year) == -tsCorr) {
        tmp$year <- 0
      }
    }
    forCS$ANPPTimeSeries$table <- tmp[tmp$year>=0,]
    # add standard deviation
    forCS$ANPPTimeSeries$table[,"ANPP-Std"] <- 1
    print("Done!")
    
    #### MaxBiomassTimeSeries
    print("Preparing / updating 'MaxBiomassTimeSeries'")
    tmp <- bsDyn[, c("year", "landtype", "species", "maxB")]
    tmp[,"year"] <-  apply(tmp, 1, f)
    tmp <- tmp[tmp$year>=-tsCorr,]
    if(length(unique(tmp$year)) == 1) {
      if(unique(tmp$year) == -tsCorr) {
        tmp$year <- 0
      }
    }
    forCS$MaxBiomassTimeSeries$table <- tmp[tmp$year>=0,]
    print("Done!")
    
    #### EstablishProbabilities
    print("Preparing / updating 'MaxBiomassTimeSeries'")
    tmp <- bsDyn[, c("year", "landtype", "species", "probEst")]
    tmp[,"year"] <-  apply(tmp, 1, f)
    tmp <- tmp[tmp$year>=-tsCorr,]
    if(length(unique(tmp$year)) == 1) {
      if(unique(tmp$year) == -tsCorr) {
        tmp$year <- 0
      }
    }
    forCS$EstablishProbabilities$table <- tmp[tmp$year>=0,]
    print("Done!")
  
    ### storing T0 baseline dynamic inputs is alignment with
    ### other scenarios are required
    if(s == "baseline" &
       alignT0withBaseline) {
      dynInputT0 <- list(ANPP = filter(forCS$ANPPTimeSeries$table,
                                       year == 0),
                         maxB = filter(forCS$MaxBiomassTimeSeries$table,
                                       year == 0),
                         SEP = filter(forCS$EstablishProbabilities$table,
                                       year == 0))
    }
    
    if(s != "baseline" &
       alignT0withBaseline) {
      
      dynInputT0 <- out$dynInputT0
      index <- which(forCS$ANPPTimeSeries$table$year == 0)
      forCS$ANPPTimeSeries$table[index,] <- dynInputT0$ANPP
      forCS$MaxBiomassTimeSeries$table[index,] <- dynInputT0$maxB
      forCS$EstablishProbabilities$table[index,] <- dynInputT0$SEP
      
    }

    
    ############################################################################
    #### RootDynamics
    
    ### some of the parameters are in tblSpeciesTypeDefault, but here I'm using
    ### equations from Li et al. 2003
    print("Preparing / updating 'RootDynamics'")
    step <- 2500 ### doesn't work with 2000, appears it has to be >= 2500
    maxBRounded <- ceiling(max(bsDyn$maxB)/step)*step
    breaks <- seq(0, maxBRounded, step) ### max values will be eliminated through the process
    
    
    forCS$RootDynamics$table <- rootBiomassParamsFetch(spp, landtypes_AT,
                                                       aidbURL = aidbURL,
                                                       breaks)
    ## updating some parameters using Hararuk
    forCS$RootDynamics$table[,"FRturnover"] <- 0.864 ### Hararuk: 0.864, CBM: 0.641
    forCS$RootDynamics$table[,"CRturnover"] <- 0.019 ### Hararuk: 0.019, CBM: 0.020
    
    print("Done!")
    

    
    ############################################################################
    #### SoilSpinUp
    forCS <- soilSpinUp(forCS,
                        soilSpinUp = spinup,
                        tolerance = 0.5, 
                        maxIter = 100)

    ############################################################################
    #### Writing ForCS parameters to file
    if(spinup) {
        file <- paste0("forCS-input_", a, "_spinup.txt")
    } else {
        file <- paste0("forCS-input_", a, "_", s, ".txt")
    }
    
    print(paste0("Writing updated ForCS inputs to file '", file, "'"))
    forCS_writeToFile(x = forCS, file, version)
    print("Done!")
    
    
    ############################################################################
    #### Writing ForCS_DM parameters to file
    if(as.numeric(version) >= 3.1) {
      file <- paste0("ForCS_DM_", a, "_", s, ".txt")
      print(paste0("Producing forCS disturbance matrices inputs and writing to file '", file, "'"))
      forCS_dm_writeToFile(x = forCS, file)
      
      print("Done!") 
    }
    
    ############################################################################
    #### producing forCS climate input file
    if(climate) {
        file <- paste0("forCS-climate_", a, "_", s, ".txt")#forCS$ForCSClimateFile
        print(paste0("Producing forCS climate inputs and writing to file '", file, "'"))
        tMean_fetch(landtypes, landtypes_AT,
                    area = a,
                    t0 = t0,
                    scenario = s,
                    writeToFile = file,
                    outputTable = F)
        print("Done!") 
    }
    
    
    
    if(alignT0withBaseline) {
      out <- list(dynInputT0 = dynInputT0)
    } else {
      out <- list()
    }
    return(out)
    
}

