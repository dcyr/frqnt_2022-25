###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
home <- path.expand("~")
home <- gsub("\\\\", "/", home)
home <- gsub("/Documents", "", home) # necessary on my Windows machine
wwd <- ifelse(Sys.info()["sysname"] == "Linux", paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency_calib/", sep = "/"),
                ifelse(Sys.info()["user"] == "CyrDo", paste(home, "Documents/Sync/Travail/ECCC/Landis-II/frqnt_2022-25", sep = "/"),
                       "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/frqnt_2022-25"))
setwd(wwd)
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(dplyr)


ecoInd_corr <- T ### with ForCS v 3.1 and before, there's a bug in how NEP is calculated. 
initYear <- 2020
unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
simName <- "mixedwood-042-51 - 2026-01-27"
a <- ifelse(grepl("mixedwood-042-51", simName), "mixedwood-042-51",
            ifelse(grepl("boreal-085-51", simName), "boreal-085-51",
                   ifelse(grepl("temperate-2a-3b", simName), "temperate-2a-3b")))

areaName <- a
# areaName <- ifelse(a == "ForMont", "For?t Montmorency",
#                    ifelse(a == "Hereford", "For?t Hereford", a))

carbonTbl <- list("mixedwood-042-51" = c(AAC = 1166700, #cubic meters
                                         area = 636270, # ha
                                         Cdensity = .214), #tonnes of C per cubic meters,
                  "temperate-2a-3b" = c(AAC = NA, #cubic meters
                                        area = NA, # ha
                                        Cdensity = NA)) #tonnes of C per cubic meters

require(ggplot2)
require(dplyr)
require(tidyr)
require(scales)

##########################################################
##### pools


treatLevels <- list("boreal-085-51" = c("generic" = "Generic",
                                   #"CPI-CP" = "Couvert permanent",
                                   "noHarvest" = "Conservation"),
                   "mixedwood-042-51" = c("No_ND" = "No natural disturbances",
                                          "Wind" = "Wind",
                                          "Wind_Sbw" = "Wind and Spruce Budworm",
                                          "Wind_Fire" = "Wind and wildfires",
                                          "Wind_Sbw_Fire" = "Wind, Spruce Budworm and wildfires"),
                     #c("generic" = "Generic",
                                   #"CPI-CP" = "Couvert permanent",
                                   #"noHarvest" = "Conservation"),
                   "temperate-2a-3b" = c("No_ND" = "No natural disturbances",
                                         "Wind" = "Wind",
                                         "Wind_Sbw" = "Wind and Spruce Budworm",
                                         "Wind_Fire" = "Wind and wildfires",
                                         "Wind_Sbw_Fire" = "Wind, Spruce Budworm and wildfires"))

mgmtLevels <- list("boreal-085-51" = c("baseline_50p" = "Baseline 50%",
                                       "baseline_75p" = "Baseline 75%",
                                       "baseline" = "Baseline 100%",
                                       
                                       #"CPI-CP" = "Couvert permanent",
                                       "noHarvest" = "Conservation"),
                   "mixedwood-042-51" = c("baseline_45p" = "Baseline",
                                          #"baseline_75p" = "Baseline 75%",
                                          #"baseline" = "Baseline 100%",
                                          #"CPI-CP" = "Couvert permanent",
                                          "noHarvest" = "Conservation"),
                                          #c("generic" = "Generic",
                                          #"CPI-CP" = "Couvert permanent",
                                          #"noHarvest" = "Conservation"),
                   #c("generic" = "Generic",
                   #"CPI-CP" = "Couvert permanent",
                   #"noHarvest" = "Conservation"),
                   "temperate-2a-3b" = c("generic" = "Generic",
                                         #"CPI-CP" = "Couvert permanent",
                                         "noHarvest" = "Conservation"))

mgmtLvls <- c("CPRS", "Couvert permanent", "Conservation")




cols <- list("boreal-085-51" = c("Generic" = "black",
                                    #"Couvert permanent" = "dodgerblue3",
                                    "Conservation" = "goldenrod3"),
             "mixedwood-042-51" = c("No_ND" = "black",
                                    "Wind" = "lightgreen",#"Couvert permanent" = "dodgerblue3",
                                    "Wind_Sbw" = "goldenrod3",
                                    "Wind_Fire" = "indianred",
                                    "Wind_Sbw_Fire" = "darkred"),
             "temperate-2a-3b" = c("No_ND" = "black",
                                   "Wind" = "lightgreen",#"Couvert permanent" = "dodgerblue3",
                                   "Wind_Sbw" = "goldenrod3",
                                   "Wind_Fire" = "indianred",
                                   "Wind_Sbw_Fire" = "darkred"))

spp <- read.table(paste0("../inputsLandis/species_", a, ".txt"),
                      skip = 1, comment.char = ">")

spGr <- c("ABIE.BAL" = "Fir/Spruce/Pine/Larch",
          "ACER.RUB" = "tolerant hardwoods",
          "ACER.SAH" = "tolerant hardwoods",
          "BETU.ALL" = "tolerant hardwoods",
          "BETU.PAP" = "intolerant hardwoods",
          "FAGU.GRA" = "tolerant hardwoods",
          "LARI.LAR" = "Fir/Spruce/Pine/Larch",
          "PICE.GLA" = "Fir/Spruce/Pine/Larch",
          "PICE.MAR" = "Fir/Spruce/Pine/Larch",
          "PICE.RUB" = "Fir/Spruce/Pine/Larch",
          "PINU.BAN" = "Fir/Spruce/Pine/Larch",
          "PINU.RES" = "Red and White Pines",
          "PINU.STR" = "Red and White Pines",
          "POPU.TRE" = "intolerant hardwoods",
          "QUER.RUB" = "tolerant hardwoods",
          "THUJ.SPP.ALL" = "Thuja",
          "TSUG.CAN" = "Tsuga")

spGrCol <- c("Fir/Spruce/Pine/Larch" = "darkolivegreen",
             "intolerant hardwoods" = "lemonchiffon3",
             "tolerant hardwoods" = "green3",
             "Red and White Pines" = "coral4",
             "Thuja" = "orange3",
             "Tsuga" = "coral1")
spGr <- factor(spGr, levels = names(spGrCol))




# scenRef <-  list("boreal-085-51" = "generic",
#                  "mixedwood-042-51" = "Wind_Sbw_Fire",
#                  "temperate-2a-3b" = "generic")
################################################################################
################################################################################
################################################################################
outputSummary <- get(load(paste0("../outputCompiled/output_summary_", simName, ".RData")))
fps <- read.csv(paste0("../outputCompiled/output_BioToFPS_", simName, ".csv"))#read.csv("output_BioToFPS_Hereford.csv")#
AGB <- get(load(paste0("../outputCompiled/output_bio_", simName, ".RData")))




###### before ForCS 4.0, there was a bug in how ecosystem indicators were calculated,
####
if(ecoInd_corr) {
  variableLvl <- levels(outputSummary$variable)
  df <- pivot_wider(outputSummary, names_from = variable, values_from = value) %>%
    mutate(totalC = ABio + BBio + TotalDOM) %>%
    group_by(simID) %>% 
    arrange(Time) %>%
    mutate(NBPcorr = totalC - lag(totalC)) %>%
    ungroup() %>%
    mutate(distOutflux = NEP - NBP,
           NBP = NBPcorr,
           NEP = NBP + distOutflux,
           NPP = NEP + Rh,
           NetGrowth = NPP - Turnover) %>%
    dplyr::select(simID, Time,
                  ABio, BBio, TotalDOM, DelBio, Turnover, 
                  NetGrowth, NPP, Rh, NEP, NBP) %>%
    pivot_longer(cols = c("ABio", "BBio", "TotalDOM", "DelBio", "Turnover",
                          "NetGrowth", "NPP", "Rh", "NEP", "NBP"),
                 names_to = "variable",
                 values_to = "valueCorr")
  ##
  outputSummary <- left_join(outputSummary, df) %>%
    mutate(value = valueCorr,
           variable = factor(variable, levels = variableLvl)) %>%
    dplyr::select(-valueCorr)
}
  
################################################################################
### renaming stuff

outputSummary <- outputSummary %>%
    filter(variable != "mgmtScenarioName") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(ND_scenarioName  = factor(treatLevels[[a]][match(as.character(ND_scenario), names(treatLevels[[a]]))],
                                     levels = treatLevels[[a]]),
           mgmtScenarioName  = factor(mgmtLevels[[a]][match(as.character(mgmtScenarioName), names(mgmtLevels[[a]]))],
                                    levels = mgmtLevels[[a]]))
           
outputSummary <- droplevels(outputSummary)

AGB <- AGB %>%
  mutate(ND_scenarioName  = factor(treatLevels[[a]][match(as.character(ND_scenario), names(treatLevels[[a]]))],
                                   levels = treatLevels[[a]]),
         mgmtScenarioName  = factor(mgmtLevels[[a]][match(as.character(mgmtScenarioName), names(mgmtLevels[[a]]))],
                                    levels = mgmtLevels[[a]]))

fps <- fps %>%
  mutate(ND_scenarioName  = factor(treatLevels[[a]][match(as.character(ND_scenario), names(treatLevels[[a]]))],
                                   levels = treatLevels[[a]]),
         mgmtScenarioName  = factor(mgmtLevels[[a]][match(as.character(mgmtScenarioName), names(mgmtLevels[[a]]))],
                                    levels = mgmtLevels[[a]]))



require(ggplot2)
require(dplyr)
require(tidyr)


### pools

variableLvl <- c("TotalEcosys", "TotalDOM", "ABio", "BBio") ## ordering levels for plotting

df <- outputSummary %>%
    filter(Time >=1,variable %in% variableLvl) %>%
    group_by(areaName, scenario, 
             ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName, 
             ##################
             ##plantedSp,
             
            # mgmtID,
             Time, variable) %>%
    summarise(value = mean(value),
              mgmtArea_ha = unique(mgmtArea_ha)) %>%
  group_by(areaName, scenario, 
           ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName, 
           ##################
           ##plantedSp,
           
           # mgmtID,
           Time, variable) %>%
    summarise(valueTotal = sum(value*mgmtArea_ha),
              mgmtArea_ha = sum(mgmtArea_ha)) %>%
    mutate(value = valueTotal/mgmtArea_ha) %>%
    as.data.frame()


df <- df %>%
    group_by(areaName, scenario,
             ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName, 
             ##################
             #plantedSp, mgmt,
             Time) %>%
    summarise(valueTotal = sum(valueTotal),
              mgmtArea_ha = unique(mgmtArea_ha)) %>%
    mutate(value = valueTotal / mgmtArea_ha,
           variable = "TotalEcosys") %>%
    as.data.frame()%>%
    rbind(df) %>%
    mutate(variable = factor(variable, levels = variableLvl)) 



# x <- df %>%
#     filter(areaName == a,
#            ND_scenarioName == scenRef[[a]])
# cNames <- colnames(df)
# cNames <- cNames[-which(cNames %in% c("treatment", "mgmtScenario", "mgmtName", "ndName", "valueTotal", "value", "mgmtArea_ha"))]

# dfRef <- merge(df, x,
#              by = cNames,
#              suffixes = c("",".ref"))
# 
# cNames <- c(colnames(df), "value.ref")
# dfRef <- dfRef[,cNames]
# 
# ref <- df

  
p <- ggplot(df, aes(x = initYear+Time, y = value*unitConvFact,
                    linetype = mgmtScenarioName ,
               colour = ND_scenario)) +
    facet_grid(variable ~ scenario, scale = "free") +
    theme_bw() +
  scale_color_manual(name = "Natural disturbance\nscenario",
                     values = cols[[a]],
                     labels = treatLevels[[a]]) +
  scale_linetype_manual(name = "Management\nscenario",
                        values = c("Baseline" = "solid",
                                   "Conservation" = "dotted")) +

    geom_line(#linetype = mgmtName,
              linewidth = 0.5) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Evolution of carbon density",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("Allometric equations from: Ung et al. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.\n",
                          "Taper equations from: Honer et al. 1983. Metric timber tables for the commercial tree species of Central and Eastern Canada."))

 
pHeight <- 1 + 1.5*length(unique(df$variable))
pWidth <- 4 + 3*length(unique(df$scenario))

png(filename= paste0("pools_", simName, ".png"),
    width = pWidth, height = 6, units = "in", res = 600, pointsize=10)
    
    print(p)

dev.off()



### fluxes

# 
variableLvl <- c("DelBio",  "Turnover",
                 "NetGrowth",  "NPP",
                 "Rh",  "NEP",
                 "NBP")

caption <- c("DelBio: Annual change in biomass stocks",
             "Turnover: Annual transfer of biomass (above-and belowground) to dead organic matter and soil pools before disturbances occur",
             "NetGrowth: Change in biomass from growth alone: the difference between the biomass at the beginning and the end of the growth routine in the timestep. This value could be negative\n         as the stand ages and mortality outpaces growth. DelBio and NetGrowth will be the same when there are no losses caused by disturbances.",
             "NPP : Net Primary Production (includes above and belowground). This includes growth and replacement of litterfall and annual turnover, i.e., the sum of NetGrow and turnover.",
             "Rh : Heterotrophic respiration. This is the sum of the 'To Air' fluxes through decomposition, not disturbance.",
             "NEP: Net Ecosystem Productivity. NPP minus Rh.",
             "NBP : Net Biome Productivity. NEP minus losses from the ecosystem due to disturbances. (Both emissions to air from combustion and losses to the forest products sector.)")
    

df <- outputSummary %>%
    filter(Time >=1,
           mgmtID >= 10000 |
               mgmtID == 1,
           variable %in% variableLvl) %>%
        ##################
    group_by(areaName, scenario,
             ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName, 
             replicate, Time, variable) %>%
    summarize(totalArea = sum(mgmtArea_ha),
              value = weighted.mean(value, mgmtArea_ha)) %>%
    ungroup() %>%
  group_by(areaName, scenario,
           ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName, 
           Time, variable) %>%
    summarise(value = mean(value))

p <- ggplot(df, aes(x = initYear+Time, y = value*unitConvFact, #group = simID,
                    linetype = mgmtScenarioName,
                    colour = ND_scenario)) +
  
    #facet_grid(variable ~ scenario, scale = "free") +#facet_wrap( ~ scenario) +
    facet_grid(variable ~ scenario , scale = "free") +#facet_wrap( ~ scenario) +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = 1, color = "grey25", size = 0.35) +
    geom_line() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(name = "Natural disturbance\nscenario",
                       values = cols[[a]],
                       labels = treatLevels[[a]]) +
    scale_linetype_manual(name = "Management\nscenario",
                          values = c("Baseline" = "solid",
                                     "Conservation" = "dotted")) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
    labs(title = paste("Carbon dynamics"),
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
         caption = paste(caption, collapse = "\n")) 



png(filename= paste0("fluxes_", simName, ".png"),
    width = 7, height = 8, units = "in", res = 600, pointsize=10)

print(p +  theme(legend.text=element_text(size=rel(.75))))
dev.off()


### fluxes, rel to 'Wind' only
dfRef <- df %>%
  filter(ND_scenario == "Wind_Sbw_Fire" &
           mgmtScenario == "baseline_45p")


dfRel  <- df %>%
  #filter(ND_scenario != "Wind") %>%
  left_join(dfRef,
            by = c("areaName", "scenario",
                   #"mgmtScenario", "mgmtScenarioName",
                   "Time", "variable")) %>%
  mutate(value.rel = value.x - value.y,
         ND_scenario = ND_scenario.x,
         ND_scenarioName = ND_scenarioName.x,
         mgmtScenario = mgmtScenario.x,
         mgmtScenarioName = mgmtScenarioName.x) %>%
  #rbind(dfRef) %>%
  dplyr::select(areaName, scenario, ND_scenario, ND_scenarioName,
         mgmtScenario, mgmtScenarioName,
         Time, variable, value.x, value.rel)
  

p <- ggplot(dfRel, aes(x = initYear+Time, y = value.rel*unitConvFact, #group = simID,
                    linetype = mgmtScenarioName,
                    colour = ND_scenario)) +
  
  #facet_grid(variable ~ scenario, scale = "free") +#facet_wrap( ~ scenario) +
  facet_grid(variable ~ scenario , scale = "free") +#facet_wrap( ~ scenario) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 1, color = "grey25", size = 0.35) +
  geom_line() +
  theme(plot.caption = element_text(size = rel(.5), hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(name = "Natural disturbance\nscenario",
                     values = cols[[a]],
                     labels = treatLevels[[a]]) +
  scale_linetype_manual(name = "Management\nscenario",
                        values = c("Baseline" = "solid",
                                   "Conservation" = "dotted")) +
  theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
  labs(title = paste('Carbon dynamics\nRelative to "Wind-only ND scenario"'),
       subtitle = paste(areaName, simName),
       x = "",
       y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
       caption = paste(caption, collapse = "\n")) 


png(filename= paste0("fluxesRel_", simName, ".png"),
    width = 7, height = 8, units = "in", res = 600, pointsize=10)

print(p +  theme(legend.text=element_text(size=rel(.75))))
dev.off()



###
### cumulative, relative to "wind_only


dfRelCumul  <- dfRel %>%
  group_by(areaName, scenario, ND_scenario, ND_scenarioName,
           mgmtScenario, mgmtScenarioName, 
           variable) %>%
  arrange(Time) %>%
  mutate(value.rel.cumul = cumsum(replace_na(value.rel, 0)))


p <- ggplot(dfRelCumul, aes(x = initYear+Time, y = value.rel.cumul*unitConvFact, #group = simID,
                       linetype = mgmtScenarioName,
                       colour = ND_scenario)) +
  
  #facet_grid(variable ~ scenario, scale = "free") +#facet_wrap( ~ scenario) +
  facet_grid(variable ~ scenario , scale = "free") +#facet_wrap( ~ scenario) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 1, color = "grey25", size = 0.35) +
  geom_line() +
  theme(plot.caption = element_text(size = rel(.5), hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(name = "Natural disturbance\nscenario",
                     values = cols[[a]],
                     labels = treatLevels[[a]]) +
  scale_linetype_manual(name = "Management\nscenario",
                        values = c("Baseline" = "solid",
                                   "Conservation" = "dotted")) +
  theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
  labs(title = paste('Carbon dynamics\nCumulative differences - Relative to "Wind-only / No harvest " scenario'),
       subtitle = paste(areaName, simName),
       x = "",
       y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
       caption = paste(caption, collapse = "\n")) 


png(filename= paste0("fluxesRelCumul_", simName, ".png"),
    width = 7, height = 8, units = "in", res = 600, pointsize=10)

print(p +  theme(legend.text=element_text(size=rel(.75))))
dev.off()
################################################################################
################################################################################
### to FPS
df <- fps %>%
    group_by(areaName, scenario,
             ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
             species, Time) %>%
   summarise(BioToFPS_tonnesCTotal = mean(BioToFPS_tonnesCTotal),
              areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha)) %>%
    mutate(spGr = spGr[species]) %>%
    group_by(areaName, scenario,
           ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
           spGr, Time) %>%
  summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal),
            areaManagedTotal_ha = unique(areaManagedTotal_ha),
            areaHarvestedTotal_ha = unique(areaHarvestedTotal_ha))
  


dfTotal <- df %>%
  group_by(areaName, scenario,
           ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
           Time) %>%
    summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal),
              areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha))
    
labdf <- df %>%
  group_by(areaName, scenario,
           ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName) %>%
  summarise(areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha)) %>%
  mutate(cEquivAAC = areaManagedTotal_ha/carbonTbl[[a]]["area"]*carbonTbl[[a]]["AAC"]*carbonTbl[[a]]["Cdensity"])

yMax <- df %>%
    group_by(areaName, scenario,
             ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
             Time) %>%
    summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) %>%
    group_by() %>%
    summarise(yMax = max(BioToFPS_tonnesCTotal))
yMax <- as.numeric(yMax)


require(RColorBrewer)
colourCount = length(unique(df$spGr))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))

### stacked (per species)
pWidth <- 4*length(unique(df$scenario))+4
pHeight <- 1+2.25*length(unique(df$ND_scenarioName))
png(filename= paste0("fps_spp_", simName, ".png"),
    width = pWidth, height = pHeight, units = "in", res = 600, pointsize=10)

#ggplot(df, aes(x = 2010+Time, y = BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) + 
ggplot(df, aes(x = initYear+Time, y = BioToFPS_tonnesCTotal)) + 
    stat_summary(aes(fill = spGr), fun.y="sum", geom="area", position = "stack") +
    facet_grid(ND_scenarioName ~ mgmtScenarioName) +
    scale_fill_manual(values = spGrCol, name = NULL) +
    #scale_fill_manual(values = getPalette(colourCount)) +
    scale_y_continuous(labels = label_number(suffix = "kt C", scale = 1e-3)) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Transfers to harvested wood products",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("Transfers to HWP",))) +
    geom_text(data = labdf, aes(label = paste("Managed area:", areaManagedTotal_ha, "ha"),
                                y = yMax, x = initYear),
              hjust = 0, vjust = 1, size = 3) +
  geom_hline(yintercept = labdf$cEquivAAC,
              linewidth = 1, linetype = "dotted")
    
dev.off()


### total

p <- ggplot(dfTotal, aes(x = initYear+Time, y = BioToFPS_tonnesCTotal,
                         colour = ND_scenario)) +
    geom_line() +
    facet_wrap(~scenario ) +#mgmtScenarioName) +
    scale_color_manual(name = "Disturbance\nscenario",
                       values = cols[[a]],
                       labels = treatLevels[[a]]) +
    scale_y_continuous(labels = label_number(suffix = "kt C", scale = 1e-3),
                       limits = c(0, 1.25*max(dfTotal$BioToFPS_tonnesCTotal))) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    #lims(y = c(0,20000)) +
    labs(title = "Transfers to harvested wood products",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("Transfers to HWP",))) +
  geom_hline(yintercept = labdf$cEquivAAC,
             linewidth = 1, linetype = "dotted")
         #y = expression(paste("tonnes C"," ha"^"-1", "r?colt?","\n")))

pWidth <- 4*length(unique(df$scenario))+4
pHeight <- pWidth*.5

png(filename= paste0("fps_total_", simName, ".png"),
    width = pWidth, height = pHeight, units = "in", res = 600, pointsize=10)
print(p)

dev.off()


### area harvested
p <- ggplot(dfTotal, aes(x = initYear+Time, y = areaHarvestedTotal_ha,
                    colour = ND_scenario)) +
  geom_line() +
  facet_wrap(scenario ~ mgmtScenarioName) +
  scale_color_manual(name = "Disturbance\nscenario",
                     values = cols[[a]],
                     labels = treatLevels[[a]]) +
  scale_y_continuous(labels = label_number(suffix = "", scale = 1e0),#1e-3),s
                     limits = c(0, 1.25*max(dfTotal$areaHarvestedTotal_ha))) +
  theme_dark() +
  theme(plot.caption = element_text(size = rel(.5), hjust = 0),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  #lims(y = c(0,20000)) +
  labs(title = "Total area harvested",
       subtitle = paste(areaName, simName),
       x = "",
       y = expression(paste("Area (ha)",))) 

pWidth <- 4*length(unique(df$scenario))+4
pHeight <- pWidth*.5
png(filename= paste0("harvest_areaHarvested_", simName, ".png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)
print(p)

dev.off()
################################################################################
################################################################################
################################################################################
### Aboveground biomass
################################################################################
require(raster)
# 
# ### must correct this
# # 
# if(a == "ForMont") {
#     r <- raster("../inputsLandis/landtypes_ForMont_cropped.tif")
# }
# if(a == "Hereford") {
#     r <- raster("../inputsLandis/landtypes_Hereford_cropped.tif")
# }
# 
# # totalArea <- AGB %>%
# #     distinct(areaName, landtype, landtypeArea_ha)
# # totalArea <- sum(totalArea$landtypeArea_ha)
# 
# # 
# # foo <- filter(AGB,
# #               scenario == "baseline",
# #               mgmtScenario == "0",
# #               replicate == 1, Time == 10, species == "ABIE.BAL")
# 
# # sum(foo$agb_tonnesTotal)/totalArea


##without the age classes
df <- AGB %>%

  group_by(areaName, scenario,
           ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
           Time, replicate,
           species) %>%
  summarise(agb_tonnesTotal = sum(agb_tonnesTotal),
                                 areaTotal_ha = sum(unique(landtypeArea_ha))) %>%
  group_by(areaName, scenario,
           ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
           Time, species) %>%
  summarise(agb_tonnesTotal = mean(agb_tonnesTotal),
            areaTotal_ha = unique(areaTotal_ha)) %>%
  as.data.frame()

  


require(RColorBrewer)

### stacked (total)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
pHeight <- .5 + 3*length(unique(df$mgmtScenario))
pWidth <- .5 + 3*length(unique(df$ND_scenario))
png(filename= paste0("agb_sppStack_", simName, ".png"),
    width = pWidth, height = pHeight, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = 2020 + Time, y = agb_tonnesTotal/areaTotal_ha)) + 
    stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(mgmtScenarioName ~ ND_scenarioName) +
    scale_fill_manual("",
                      values = getPalette(colourCount)) +
    theme_bw() +
    theme(plot.caption = element_text(size = rel(.75), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Evolution of landscape composition  - Aboveground biomass*",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Values are expressed as total dry mass (biomass), not carbon mass.")


dev.off()


### line (total)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

png(filename= paste0("agb_sppLine_", simName, ".png"),
    width = pWidth, height = pHeight, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/areaTotal_ha,
               colour = species,
               group = species)) + 
    geom_line() +
    #stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
  facet_grid(mgmtScenarioName ~ ND_scenarioName) +
    scale_colour_manual("",
                        values = getPalette(colourCount)) +
    #scale_fill_manual(values = getPalette(colourCount)) +
    theme_bw() +
    theme(plot.caption = element_text(size = rel(.75), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Evolution of landscape composition  - Aboveground biomass*",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Values are expressed as total dry mass (biomass), not carbon mass.")


dev.off()
# 



################################################################################
################################################################################
################################################################################
### Aboveground biomass (by age classes)
################################################################################
####
cols = brewer.pal(n = 9, name = 'Greens')[3:9]
##########################




df <- AGB %>% mutate(mgmt =  mgmtScenario) %>% 
    group_by(areaName, scenario,
             ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
             Time, replicate,
             ageClass,
             species) %>%
    summarise(agb_tonnesTotal = sum(agb_tonnesTotal),
              areaTotal_ha = sum(unique(landtypeArea_ha))) %>%
    group_by(areaName, scenario,
             ND_scenario, ND_scenarioName, mgmtScenario, mgmtScenarioName,
             ageClass,
             Time, species) %>%
    summarise(agb_tonnesTotal = mean(agb_tonnesTotal),
              areaTotal_ha = unique(areaTotal_ha)) %>%
    as.data.frame()


#require(RColorBrewer)


################################################################################
### stacked (age classes, global)
cols = rev(brewer.pal(n = 9, name = 'Greens')[3:9])

# ################################################################################


acLvls <- levels(df$ageClass)
acLvls <- rev(acLvls)
df[,"ageClass"] <- factor(df$ageClass, levels = acLvls)


#### mgmt ~ scenario
pHeight <- .5 + 3*length(unique(df$mgmtScenario))
pWidth <- .5 + 3*length(unique(df$ND_scenario))

p <- ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/areaTotal_ha)) + 
    stat_summary(aes(fill = ageClass), fun.y="sum", geom="area", position = "stack") +
  facet_grid(mgmtScenarioName ~ ND_scenarioName, scales = "fixed") +#scales = "free_y") +
    scale_fill_manual("Age classes",
                      values = cols)+
    theme_bw() +
    theme(plot.caption = element_text(size = rel(1), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0("Evolution of age structure - ", areaName, " - ", simName),
         subtitle = paste0("Aboveground biomass broken down by age classes"),
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Values are expressed as total dry mass (biomass), not carbon mass.")

png(filename= paste0("agb_AgeClassStacked_",simName, ".png"),
    width = pWidth, height = pHeight, units = "in", res = 600, pointsize=10)
    print(p)
dev.off()


