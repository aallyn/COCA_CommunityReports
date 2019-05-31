#####
## Treemap script to generate and save treemaps for community reports
#####

# Preliminaries -----------------------------------------------------------
# Generating file paths to Mills Lab functions and then data on shared folder in COCA-conf, as well as
os.use<- .Platform$OS.type
lab.funcs.path<- switch(os.use,
                   "unix" = "~/Box/Mills Lab/Functions/",
                   "windows" = "")

# Source functions -- this is a little clumsy with the output text, but works
lab.funcs<- list.files(lab.funcs.path, full.names = TRUE)
sapply(lab.funcs, source)

# Install and load required libraries
library_check(c("tidyverse", "treemapify", "here", "grid", "cowplot"))

# Shared COCA-conf stem directory
stem.path<- switch(os.use,
                   "unix" = "/Volumes/Shared/Research/COCA-conf/",
                   "windows" = "J:/Research/COCA-conf/")

code.path<- paste(stem.path, "Landings/code/", sep = "")
summary.data.path<- paste(stem.path, "Landings/summaries/", sep = "")
econ.path<- paste(stem.path, "Landings/econ ref tables/", sep = "")

# Source common aesthetic file (untouched here, managed by Brian)
source(paste(code.path, "comm_reports_aesthetics.R", sep = ""))

# Source treemap function code
source(here("Code", "treemap_functions.R"))

# Loading in the datasets and doing some quick name tidying ---------------
# First, CFDERS community and species landings summary files
cfders.by.spp<- read.csv(paste(summary.data.path, "Comm.Sppsummary.csv", sep = ""))
colnames(cfders.by.spp)[1]<- "jgs"

# Defining groundfish species
cfders.nemulti<- c("FLOUNDER, AM. PLAICE", "COD", "HALIBUT, ATLANTIC", "POLLOCK", "WOLFFISH,ATLANTIC", "HADDOCK", "POUT, OCEAN", "REDFISH", "HAKE, WHITE", "FLOUNDER, SAND-DAB", "FLOUNDER, WINTER", "FLOUNDER, WITCH", "FLOUNDER, YELLOWTAIL")

# Adding this alternative species naming convention, with groundfish grouped
cfders.by.spp <- cfders.by.spp %>%
  mutate(., spp.alt = case_when(spp_common_name %in% cfders.nemulti ~ "N.E. Multispecies",
                                !spp_common_name %in% cfders.nemulti ~ as.character(spp_common_name)))

# Next, for page 2 (ecology page) fill is going to be based on projected distribution changes. So, need to read in that file and then deal with any infinite percent increases
#cfders.spp.impact<- read.csv(paste(summary.data.path, "SpeciesCommunityChangesLandings_03032019.csv", sep = ""))
cfders.spp.impact<- read.csv(paste(summary.data.path, "SpeciesCommunityCFDERSWeightedChanges.csv", sep = ""))

if(FALSE){
  stone<- cfders.spp.impact %>% 
    filter(., Community == "STONINGTON_ME") %>%
    dplyr::select(., -X)
  write.csv(stone, "~/Desktop/Stone_SpeciesChangesandLandingsData_05292019.csv")
  
  focal.comms<- cfders.spp.impact %>% 
    filter(., Community %in% c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")) %>%
    dplyr::select(., -X)
  write.csv(focal.comms, "~/Desktop/FocalCommunities_SpeciesChangesandLandingsData_05292019.csv")
  
  all.out<- cfders.spp.impact %>% 
    dplyr::select(., -X)
  write.csv(all.out, "~/Desktop/AllPorts_SpeciesChangesandLandingsData.csv")
}

# Change infinite to NA?
inf.df<- cfders.spp.impact[is.infinite(cfders.spp.impact$ProjectionValue),]
unique(inf.df$CommonName)
cfders.spp.impact$ProjectionValue[is.infinite(cfders.spp.impact$ProjectionValue)]<- 0

# For page 3 (economics page) fill is going to be based on projected profit. Reading in files, old and new
econ.results.old<- read.csv(paste(summary.data.path, "community_value.csv", sep = ""))
econ.results.old$jgs<- ifelse(as.character(econ.results.old$port) == "New Bedford", "NEW BEDFORD_MA", 
                          ifelse(as.character(econ.results.old$port) == "Point Judith", "POINT JUDITH_RI",
                                 ifelse(as.character(econ.results.old$port) == "Stonington", "STONINGTON_ME", "PORTLAND_ME")))
colnames(econ.results.old)[4]<- "CommonName"

# Species names reference table
spp.names.table<- read.csv(paste(econ.path, "spp_names_stripcommas.csv", sep = "")) %>%
  dplyr::select(., comma_names, nice_names)
colnames(spp.names.table)<- c("CFDERSCommonName", "CommonName")

# Add this to econ results
econ.results.old<- econ.results.old %>%
  left_join(., spp.names.table)

# Now, new ones
econ.results.new<- read.csv(paste(summary.data.path, "landing_value_05202019.csv", sep = ""))
colnames(econ.results.new)[c(5:6)]<- c("GAMSCommonName", "land_value")
econ.results.new$jgs<- ifelse(as.character(econ.results.new$port) == "NewBedford_MA", "NEW BEDFORD_MA", 
                          ifelse(as.character(econ.results.new$port) == "PointJudith_RI", "POINT JUDITH_RI",
                                 ifelse(as.character(econ.results.new$port) == "Stonington_ME", "STONINGTON_ME", "PORTLAND_ME")))

# Species names reference table
spp.names.table<- read.csv(paste(econ.path, "spp_names_05202019.csv", sep = "")) %>%
  dplyr::select(., spp, spp_nice)
colnames(spp.names.table)<- c("CommonName", "GAMSCommonName")

# Add this to econ results
econ.results.new<- econ.results.new %>%
  left_join(., spp.names.table)

# Species names reference table
spp.names.table<- read.csv(paste(econ.path, "spp_names_stripcommas.csv", sep = "")) %>%
  dplyr::select(., comma_names, nice_names)
colnames(spp.names.table)<- c("CFDERSCommonName", "CommonName")

# Add this to both econ results
econ.results.old<- econ.results.old %>%
  left_join(., spp.names.table)

econ.results.new<- econ.results.new %>%
  left_join(., spp.names.table)

# Alternative speces names, too
econ.results.old<- econ.results.old %>%
  mutate(., spp.alt = case_when(CFDERSCommonName %in% cfders.nemulti ~ "N.E. Multispecies",
                                !CFDERSCommonName %in% cfders.nemulti ~ as.character(CFDERSCommonName)))

econ.results.new<- econ.results.new %>%
  mutate(., spp.alt = case_when(CFDERSCommonName %in% cfders.nemulti ~ "N.E. Multispecies",
                                !CFDERSCommonName %in% cfders.nemulti ~ as.character(CFDERSCommonName)))
# Nice names reference table
cfders.nice.names<- read.csv(paste(econ.path, "cfders_nice_names.csv", sep = "")) 
colnames(cfders.nice.names)[1]<- "jgs"

# Data set creation, includes top species and then treemap fill function -------------------------------------------------------
# Focal communities
jgs.ports <- c("STONINGTON_ME" ,"PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
sdm.top.n<- c(10, 7, 6, 10)
econ.top.n<- c(1, 10, 10, 10)

# Setting output directories
graphic.folder <- "C:/Users/bkennedy/Dropbox/COCA community report graphics/"
pg.1<- "1_intro/"
pg.2<- "2_sdm and vulnerability/"
pg.3<- "3_econ impacts/"
pg.4<- "4_adaptation/"

for(i in seq_along(jgs.ports)){
  
  # First, identify which species we have models for in the given port
  spp.modeled<- cfders.spp.impact %>% 
    filter(Community == jgs.ports[i]) %>%
    drop_na(CFDERSCommonName) %>%
    dplyr::select(., Community, CommonName, CFDERSCommonName) %>%
    distinct(CFDERSCommonName) %>%
    mutate(., spp.alt = case_when(CFDERSCommonName %in% cfders.nemulti ~ "N.E. Multispecies",
                        !CFDERSCommonName %in% cfders.nemulti ~ as.character(CFDERSCommonName))) %>%
    dplyr::select(., spp.alt) %>%
    distinct()
  
  # Alring, now top species for sdm and then for the econ data
  sdm.top<- cfders_top_spp_func(input.data = cfders.by.spp, top.n = sdm.top.n[i], port.name = jgs.ports[i])
  
  # Now, econ
  econ.top<- econ_top_spp_func(input.data = econ.results.new, top.n = econ.top.n[i], port.name = jgs.ports[i])
  
  # Now, econ and new results by gear
  econ.gear.top<- econ_top_spp_gear_func(input.data = econ.results.new, econ.top.n[i], port.name = jgs.ports[i])
  
  # Next, need to fill in the data to account for species that we did not model BUT that are important to the landings in the port. This only comes into play for the sdm 
  sdm.filled<- sdm_fill_func(model.dat = cfders.spp.impact, top.spp.dat = sdm.top, port.name = jgs.ports[i], scenario.name = "Future_mean_percdiff.combo.b")
  
  # Now all the treemaps...
  treemap.width<- 4.5
  treemap.height<- 2
  # Value plot -- page 1
  val.plot<- treemap_value_plot(sdm.filled, total.value = FALSE)
  ggsave(here(paste("Results/", jgs.ports[i], sep = ""), "treemap_value.png"), val.plot, width = treemap.width, height = treemap.height, units = "in")
  
  # SDM changes by baseline value -- page 2.
  # Treemap option first
  sdm.changes.plot<- treemap_fill_plot(sdm.filled, type = "sdm")
  ggsave(here(paste("Results/", jgs.ports[i], sep = ""), "treemap_sdm.png"), sdm.changes.plot, width = treemap.width, height = treemap.height, units = "in")
  
  # Barplot in case that didn't work
  sdm.changes.barplot<- sdm_bar_plot(sdm.filled)
  ggsave(here(paste("Results/", jgs.ports[i], sep = ""), "barplot_sdm.png"), sdm.changes.barplot, width = treemap.width, height = treemap.height, units = "in")
  
  # Economic results -- page 3 with multiple scenarios
  scenarios.use<- c("baseline", "calibration_test", "no_adaptation", "area+gear+species")
  econ.changes.plot<- treemap_fill_plot(econ.top, type = "econ", scenarios = scenarios.use, spp.modeled = spp.modeled, final = FALSE)
  
  # Save each of em --
  for(l in seq_along(econ.changes.plot)){
    ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste("treemap_", names(econ.changes.plot)[l], ".png", sep = "")), width = treemap.width, height = treemap.height, econ.changes.plot[[l]])
  }
  
  # Combine into one plot?
  all.econ.out<- plot_grid(plotlist = econ.changes.plot, align = "h", nrow = 1, label_size = font.size-1, label_fontfamily = font.family)
  ggsave(here(paste("Results/", jgs.ports[i], sep = ""), "treemap_allecon.png"), all.econ.out, width = 11, height = 8)
  
  # Economic Results, one off
  scenarios.use<- c("area+gear+species")
  econ.changes.one.plot<- treemap_fill_plot(econ.top, type = "econ", scenarios = scenarios.use, spp.modeled = spp.modeled, final = TRUE, treetext.minsize = 3)
  ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste(scenarios.use, "final_econ.png", sep = "")), plot = econ.changes.one.plot[[1]], width = treemap.width, height = treemap.height, units = "in")
  
  # Economic results -- by gear
  econ.changes.gear.plot<- treemap_fill_plot(econ.gear.top, type = "econ.gear", final = FALSE)
  
  # Write each of these out
  for(m in seq_along(econ.changes.gear.plot)){
    if(any(is.na(econ.changes.gear.plot[[m]]))){
      print(m)
      next()
    } else {
      ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste(names(econ.changes.gear.plot)[m], ".png", sep = "")), plot = econ.changes.gear.plot[[m]], width = 9, height = 3, units = "in")
      dev.off()
    }
  }
}











# Problem ports!!! -------------------------------------------------------
# Focal communities
jgs.ports <- c("STONINGTON_ME")
sdm.top.use<- 10
econ.top.use<- 6

# First, identify which species we have models for in the given port
spp.modeled<- cfders.spp.impact %>% 
  filter(Community == jgs.ports) %>%
  drop_na(CFDERSCommonName) %>%
  dplyr::select(., Community, CommonName, CFDERSCommonName) %>%
  distinct(CFDERSCommonName) %>%
  mutate(., spp.alt = case_when(CFDERSCommonName %in% cfders.nemulti ~ "N.E. Multispecies",
                                !CFDERSCommonName %in% cfders.nemulti ~ as.character(CFDERSCommonName))) %>%
  dplyr::select(., spp.alt) %>%
  distinct()

# Alring, now top species for sdm and then for the econ data
sdm.top<- cfders_top_spp_func(input.data = cfders.by.spp, top.n = sdm.top.use, port.name = jgs.ports)

# Now, econ
econ.top<- econ_top_spp_func(input.data = econ.results.new, top.n = econ.top.use, port.name = jgs.ports)

# Now, econ and new results by gear
econ.gear.top<- econ_top_spp_gear_func(input.data = econ.results.new, econ.top.use, port.name = jgs.ports)

# Next, need to fill in the data to account for species that we did not model BUT that are important to the landings in the port. This only comes into play for the sdm 
sdm.filled<- sdm_fill_func(model.dat = cfders.spp.impact, top.spp.dat = sdm.top, port.name = jgs.ports, scenario.name = "Future_mean_percdiff.combo.b")

# Now all the treemaps...
# Value plot -- page 1
val.plot<- treemap_value_plot(sdm.filled)
ggsave(here(paste("Results/", jgs.ports[i], sep = ""), "treemap_value.png"), val.plot, width = 4.5, height = 3, units = "in")


ggsave(here(paste("Results/", jgs.ports, sep = ""), "sdm_bar.png"), sdm.bar, width = 4.5, height = 3, units = "in")


# Economic results -- page 3 with multiple scenarios
scenarios.use<- c("baseline", "calibration_test", "no_adaptation", "area+gear+species")
econ.changes.plot<- treemap_fill_plot(econ.top, type = "econ", scenarios = scenarios.use, spp.modeled = spp.modeled)

# Save each of em --
for(l in seq_along(econ.changes.plot)){
  ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste("treemap_", names(econ.changes.plot)[l], ".png", sep = "")), econ.changes.plot[[l]])
}

# Combine into one plot?
all.econ.out<- plot_grid(plotlist = econ.changes.plot, align = "h", nrow = 1, label_size = font.size-1, label_fontfamily = font.family)
ggsave(here(paste("Results/", jgs.ports[i], sep = ""), "treemap_allecon.png"), all.econ.out, width = 11, height = 8)

# Economic Results, one off
scenarios.use<- c("area+gear+species")
econ.changes.one.plot<- treemap_fill_plot(econ.top, type = "econ", scenarios = scenarios.use, spp.modeled = spp.modeled, final = TRUE)
ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste(scenarios.use, "single_econ.png", sep = "")), plot = econ.changes.one.plot, width = 4.5, height = 3, units = "in")

# Economic results -- by gear
econ.changes.gear.plot<- treemap_fill_plot(econ.gear.top, type = "econ.gear")

# Write each of these out
for(m in seq_along(econ.changes.gear.plot)){
  if(any(is.na(econ.changes.gear.plot[[m]]))){
    print(m)
    next()
  } else {
    ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste(names(econ.changes.gear.plot)[m], ".png", sep = "")), plot = econ.changes.gear.plot[[m]], width = 9, height = 3, units = "in")
    dev.off()
  }
}

