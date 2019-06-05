######
## CFDERS data summary code
######

# Background --------------------------------------------------------------
## Here is where you would actually source the functions in "CFDERS_data_summary_for_communities_functions.R" Importantly, all of these functions will require connection to the shared drive.
library(here)

# Set path to functions code and load them all into R
source(here("Code", "CFDERS_data_summary_for_communities_functions.R"))

# This should have loaded nine different functions -- see the README.md file for more information on each function, or feel free to open the "CFDERS_data_summary_for_communities_functions.R" code and inspect each of the functions. 

# Setting paths -------------------------------------
# First, set stem directory to the shared drive, which will be different on a Mac than PC. After doing this, we can then generate all the folder paths to execute subsequent functions. 
# Change stem directory
stem.dir<-  "/Volumes/Shared/Research/"

# Path to the CFDERS yearly data file:
landings.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Data/Landings/", sep = "")

# Path to the references tables folder, which includes things like unique VTR communities, unique CFDERS ports, and eventually the VTR_CFDERS name matches with long/lats.
ref.tables.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Data/Reference Tables/", sep = "")

# Path to save processed CFDERRS files (summaries of trip data to different levels)
proc.summ.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Processed Summaries/", sep = "")

# Path to save Figures
fig.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Figures and Tables/", sep = "")

# Path to SDM results
sdm.path<- paste(stem.dir, "COCA-conf/SDM and CFDERS Integration/Data/SDM Projections/", sep = "")

# Executing functions -----------------------------------------------------
# First, bind_cfders to get cfders data together. This function takes a long time, so it is currently set within a run statement. If you want to run it to see if it works, set run = TRUE.
run<- FALSE
if(run){
  bind_cfders(landings.path = landings.path, out.path = landings.path)
}

# Next, we would use the cfders_community_name_match and cfders_communitylonglat functions. Theses are also set within a run TRUE/FALSE block since it requires a google API key and some post processing to get the full list of matching vtr communities and cfders ports. However, also worth project team looking through things to make sure the function is behaving correctly.
run<- FALSE
if(run){
  cfders.comm.match<- cfders_community_name_match(data.path = ref.tables.path, out.path = ref.tables.path)
  cfders.comm.longlat<- cfders_community_longlat(data.path = ref.tables.path, google.api, out.path = ref.tables.path)
}

# After successfully creating reference tables to communicate between CFDERS fisheries data and VTR community fishing data, we can now proceed with calculating fisheries data summaries, joining these with species distribution model projections, and calculating community level importance weighted change vulnerability metrics. 
# Summarizing CFDERS data
cfders.summ<- summarize_cfders(landings.path = landings.path, ref.tables.path = ref.tables.path, out.path = proc.summ.path, focal.comms = c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI"))

# In addition to writing these to a folder, each is saved in the cfders.summ object, which is a list:
names(cfders.summ)

# Summarizing GAR data
gar.summ<- summarize_gar(landings.path = landings.path, ref.tables.path = ref.tables.path, out.path = proc.summ.path)

# In addition to writing the sumamry to folder, summary is also returned as a data frame
str(gar.summ)

# Joining CFDERS and GAR fisheries data to species distribution model projections. Although a bit less efficient, to keep things simpler, this function needs to run with three different landings files: (1) the CFDERS "FocalComm.Spp.Gearsummary" summary file; (2) the "Comm.Sppsummary" summary file and (3) the "GARsummary" file. Instead of doing this one at a time, we can write a quick loop to get them all done.
# Vector of all three of the different fisheries data sets
landings.files<- paste(proc.summ.path, c("FocalComm.Spp.Gearsummary.csv", "Comm.Sppsummary.csv", "GARsummary.csv"), sep = "")

# Empty list to store results
sdm.land.merge<- vector("list", length(landings.files))
names(sdm.land.merge)<- c("FocalComm.Spp.Gearsummary.csv", "Comm.Sppsummary.csv", "GARsummary.csv")

# Quick loop to run each through the "sdm_landings_merged" function, and save each one as part of an overall list
for(i in seq_along(landings.files)){
  file.use<- landings.files[i]
  sdm.land.merge[[i]]<- sdm_landings_merged(sdm.path = sdm.path, landings.file = file.use, focal.comms = c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI"), ref.tables.path = ref.tables.path, out.path = proc.summ.path)
  print(paste(file.use, " is done", sep = ""))
}

# Summarize improtance weighted changes across communities and make a map of these vulnerability values across the Northeast Shelf Large Marine Ecosystem
# Again, we can execute a loop given that each of these could be done for the three different files created by the "sdm_landings_merged" function. 
# Vector of all three of the different fisheries data sets
sdm.land.files<- paste(proc.summ.path, c("SpeciesFocalCommunityGearTypeCFDERSWeightedChanges.csv", "SpeciesCommunityCFDERSWeightedChanges.csv", "SpeciesCommunityGARWeightedChanges.csv"), sep = "")

# Empty list to store results
comm.vuln<- vector("list", length(sdm.land.files))
names(comm.vuln)<- c("FocalCommunityGearAggregatedCFDERSWeightedChanges.csv", "CommunityAggregatedCFDERSWeightedChanges.csv", "CommunityAggregatedGARWeightedChanges.csv")

# Quick loop to run each through the "sdm_landings_merged" function, and save each one as part of an overall list -- percent first
for(i in seq_along(sdm.land.files)){
  file.use<- sdm.land.files[i]
  comm.vuln[[i]]<- community_weighted_changes(sdm.landings.file = file.use, projection.scenario = c("Percent"), out.path = proc.summ.path, plot.path = fig.path, plot = "Community")
  print(paste(file.use, " is done", sep = ""))
}

# Next, raw values
for(i in seq_along(sdm.land.files)){
  file.use<- sdm.land.files[i]
  comm.vuln[[i]]<- community_weighted_changes(sdm.landings.file = file.use, projection.scenario = c("Raw"), out.path = proc.summ.path, plot.path = fig.path, plot = "Community")
  print(paste(file.use, " is done", sep = ""))
}
# Looking at each community, how confident can we be in our modeling effort to say that we are actually successfully modeling species important to each of the ports? For this, only going to use the CFDERS sdm.landings merged data file and then the community_successfulmodels function, which calculates the total proportion of landed value/volume across species with at least AUC of >0.7 in both seasons (this could be changed to assess other model fit criteria). Using that filtered dataset, it then codes communities depending on if that total proportion is greater or less than some threshold percentages and maps that information.
sdm.success<- community_successfulmodels(sdm.landings.file = paste(proc.summ.path, "SpeciesCommunityCFDERSWeightedChanges.csv", sep = ""), mod.criteria = "AUC", mod.cut = 0.7, percent = 0.75, out.path = proc.summ.path)

# Need to bring in species names matches to go between the COMNAME of model results and the spp_common_name names from the landings data...
cfders.sppnames<- read_csv(paste(ref.tables.path, "spp_names_stripcommas.csv", sep = "")) %>%
  dplyr::select(., comma_names, nice_names)
colnames(cfders.sppnames)<- c("CFDERSCommonName", "CommonName")

# Let's look at communities where we failed to successfully model at least 0.75% of landed value/volume...
sdm.fails<- sdm.success %>%
  dplyr::filter(., SuccessfulValueCheck == "No" | SuccessfulVolumeCheck == "No")

# Bring in the CFDERS landings data...
land.dat<- read_csv(paste(proc.summ.path, "Comm.Sppsummary.csv", sep = ""))
colnames(land.dat)[1:4]<- c("Community", "Long", "Lat", "CFDERSCommonName")

# Now, we want to figure out what species account for a LOT of landings and that we didn't model correctly...
mod.results<- read_csv(paste(sdm.path, "mod.results.csv", sep = "")) %>%
  mutate(., "AUC.Check" = ifelse(AUC.SDM >= 0.7, 1, 0))

auc.check<- mod.results %>%
  group_by(COMNAME) %>%
  summarize_at(., .vars = "AUC.Check", sum, na.rm = TRUE) %>%
  filter(., AUC.Check != 2)

spp.poormod<- mod.results %>%
  filter(., COMNAME  %in% auc.check$COMNAME) 
spp.poormod<- data.frame("CommonName" = unique(spp.poormod$COMNAME))

# Going to need the CFDERS naming too...
spp.poormod<- spp.poormod %>%
  left_join(., cfders.sppnames)

# Okay, now digging in...
# Value
sdm.fails.val<- sdm.fails %>%
  left_join(., land.dat) %>%
  dplyr::select(., Community, Long, Lat, ProportionValueSuccessfulModeled, CFDERSCommonName, Totalsppvalue) %>%
  dplyr::filter(., CFDERSCommonName %in% spp.poormod$CFDERSCommonName) %>%
  arrange(Community, -Totalsppvalue)

write_csv(sdm.fails.val, paste(proc.summ.path, "CommunitySDMFailuresValue.csv", sep = ""))

# Top ones?
val.fails.top<- sdm.fails.val %>%
  group_by(Community) %>%
  top_n(2)
unique(val.fails.top$CFDERSCommonName)

# Volume
sdm.fails.vol<- sdm.fails %>%
  left_join(., land.dat) %>%
  dplyr::select(., Community, Long, Lat, ProportionVolumeSuccessfulModeled, CFDERSCommonName, Totalspplndlb) %>%
  dplyr::filter(., CFDERSCommonName %in% spp.poormod$CFDERSCommonName) %>%
  arrange(Community, -Totalspplndlb)
write_csv(sdm.fails.vol, paste(proc.summ.path, "CommunitySDMFailuresVolume.csv", sep = ""))

vol.fails.top<- sdm.fails.vol %>%
  group_by(Community) %>%
  top_n(2)
unique(vol.fails.top$CFDERSCommonName)


# Long to Wide Summary for KM -------------------------------------------------------------
#Long to Wide for KM
temp<- read_csv(paste(proc.summ.path, "SpeciesCommunityCFDERSWeightedChanges.csv", sep = "")) %>%
  filter(., CFDERSCommonName != "SKATE,LITTLE/WINTER")
unique(temp$ProjectionScenario)

# Want Weighted changes for Future_mean_diff.combo.b and Future_mean_percdiff.combo.b
keep<- c("Future_mean_diff.combo.b", "Future_mean_percdiff.combo.b")
temp1.diff<- temp %>%
  filter(., ProjectionScenario == "Future_mean_diff.combo.b") %>%
  dplyr::select(., CommonName, Community, Gear, Footprint, ChangeWeightedValue, ChangeWeightedVolume)
colnames(temp1.diff)[5:6]<- c("RawChangeWeightedValue", "RawChangeWeightedVolume")

temp1.diff<- temp1.diff %>% 
  mutate(., Match = paste(CommonName, Community, Gear, Footprint))

temp2.diff<- temp %>%
  filter(., ProjectionScenario == "Future_mean_percdiff.combo.b") %>%
  dplyr::select(., CommonName, Community, Gear, Footprint, ChangeWeightedValue, ChangeWeightedVolume)
colnames(temp2.diff)[5:6]<- c("PercentChangeWeightedValue", "PercentChangeWeightedVolume")

temp2.diff<- temp2.diff %>% 
  mutate(., Match = paste(CommonName, Community, Gear, Footprint))

temp.diffs<- temp1.diff %>%
  left_join(., temp2.diff)

# Now, spread on the differences...
temp2<- temp %>%
  dplyr::select(., CommonName, Community, Long, Lat, Gear, Footprint, AUC.SDM, TotalValue, CommTotalValue, ProportionValue, TotalVolume, CommTotalVolume, ProportionVolume, ProjectionScenario, ProjectionValue) %>%
  spread(., ProjectionScenario, ProjectionValue) %>%
  dplyr::select(., CommonName, Community, Long, Lat, Gear, Footprint, AUC.SDM, TotalValue, CommTotalValue, ProportionValue, TotalVolume, CommTotalVolume, ProportionVolume, Baseline.combo.b, Future_mean.combo.b, Future_mean_diff.combo.b, Future_mean_percdiff.combo.b) %>%
  mutate(., 
         "RawChangeWeightedValue" = Future_mean_diff.combo.b*ProportionValue,
         "RawChangeWeightedVolume" = Future_mean_diff.combo.b*ProportionVolume,
         "PercentChangeWeightedValue" = Future_mean_percdiff.combo.b*ProportionValue,
         "PrecentChangeWeightedVolume" = Future_mean_percdiff.combo.b*ProportionVolume)
write_csv(temp2, paste(proc.summ.path, "SpeciesCommunityCFDERSWeightedChangesWide.csv", sep = ""))

# Focal Communities Only
temp3<- temp2 %>%
  dplyr::filter(., Community %in% focal.comms)
write_csv(temp3, paste(proc.summ.path, "SpeciesFocalCommunityCFDERSWeightedChangesWide.csv", sep = ""))


# Treemap for top species -------------------------------------------------
# Treeplot diagrams with fill for species vulnerability
# Organizing the data we need
# Read in the landings data...
land.dat<- read_csv(paste(proc.summ.path, "Comm.Sppsummary.csv", sep = ""))

# Let's nest the dataset by focal community, then get top species
focal.comms<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")

land.dat.nest<- land.dat %>%
  filter(., jgs %in% focal.comms) %>%
  group_by(jgs) %>%
  nest(.key = "LandData")

# Grabbing numbers from econ_figures.R code
top.spp<- data.frame("jgs" = c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI"), "Top.N" = c(1, 8, 6, 11))

# Joining to land.dat.nest
land.dat.nest<- land.dat.nest %>%
  left_join(., top.spp)

# Get top N species by community, save the vector of species names
landings_top_spp_func<- function(df, top.n){
  
  if(FALSE){
    df<- land.dat.nest$data[[4]]
    top.n<- land.dat.nest$Top.N[[4]]
  }
  
  top.species<- df %>% 
    dplyr::select(., spp_common_name, Meansppvalue) %>%
    top_n(top.n, Meansppvalue) %>%
    arrange(-Meansppvalue)
  return(data.frame("CFDERSCommonName" = top.species$spp_common_name))
}

# Apply it
land.dat.nest<- land.dat.nest %>%
  dplyr::mutate(., Top.Species = map2(LandData, Top.N, landings_top_spp_func))

# Now, bring in species projections data...
spp.land.dat<- read_csv(paste(proc.summ.path, "SpeciesCommunityCFDERSWeightedChanges.csv", sep = "")) %>%
  dplyr::select(., -X1) 

# We only need projection scenarios for Future percent differences, regular footprint, mean stats for focal communities
focal.comms<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
scenarios.keep<- c("Future_cold_percdiff.combo.b", "Future_mean_percdiff.combo.b", "Future_warm_percdiff.combo.b")
foot.keep<- c("Regular")
stat.keep<- c("Mean")

spp.land.dat.sub<- spp.land.dat %>%
  filter(., Community %in% focal.comms & ProjectionScenario %in% scenarios.keep & Footprint %in% foot.keep & Statistic %in% stat.keep)

# Also don't need all these columns
spp.land.dat.sub<- spp.land.dat.sub %>%
  dplyr::select(., CFDERSCommonName, Community, Long, Lat, Gear, TotalValue, TotalVolume, MeanValue, MeanVolume, DistinctDealers, CommTotalValue, CommTotalVolume, ProportionValue, ProportionVolume, ProjectionScenario, ProjectionValue, ChangeWeightedValue, ChangeWeightedVolume)

# Plots are going to be by community...
spp.land.nest<- spp.land.dat.sub %>%
  group_by(Community) %>%
  nest(.key = "SDMData")

# Join back in top species info...
colnames(spp.land.nest)[1]<- "jgs"
sdm.land.nest<- land.dat.nest %>%
  left_join(spp.land.nest)

# loading package
library(treemapify)
source("/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/comm_reports_aesthetics.R")

# Now a treemap function
treemap_sdmvuln_func<- function(community, df, top.species){

  if(FALSE){
    community<- sdm.land.nest$jgs[[3]]
    df<- spp.land.nest$SDMData[[3]]
    top.species<- sdm.land.nest$Top.Species[[3]]
  }
  
  spp.keep<- as.character(top.species$CFDERSCommonName)
  
  df.use<- df %>%
    filter(., CFDERSCommonName %in% spp.keep)
  df.use$ProjectionScenario<- factor(df.use$ProjectionScenario, levels = scenarios.keep, labels = c("Cold climate scenario", "Average climate scenario", "Warm climate scenario"))
  df.use$ProjectionValue[is.infinite(df.use$ProjectionValue)]<- NA

  treeplot.val<- ggplot(data = df.use, aes(area = TotalValue, label = CFDERSCommonName, fill = ProjectionValue)) +
    geom_treemap(alpha = 0.8, color = "white", size = 2) +
    geom_treemap_text(colour = "black", place = "topleft", reflow = T, alpha = .6,
                      grow = FALSE, family = font.family, size = font.size, min.size = 3) +
    scale_fill_gradient2(name = "Projected species percent change", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0) +
    ggtitle("Landed Value") +
    theme_tufte() + 
    theme(legend.position = "top", 
          legend.key.width = unit(.5, "in"),
          legend.key.height = unit(.1, "in"),
          legend.title.align = .5,
          text = element_text(family = font.family, size = font.size),
          aspect.ratio = .5) +
    facet_wrap(~ProjectionScenario)
  
  # treeplot.vol<- ggplot(data = df.use, aes(area = TotalVolume, label = CFDERSCommonName, fill = ProjectionValue)) +
  #   geom_treemap(alpha = 0.8, color = "white", size = 2) +
  #   geom_treemap_text(colour = "black", place = "topleft", reflow = T, alpha = .6,
  #                     grow = FALSE, family = font.family, size = font.size, min.size = 3) +
  #   scale_fill_gradient2(name = "Projected species percent change", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0) +
  #   ggtitle("Landed Volume") +
  #   theme_tufte() + 
  #   theme(legend.position = "top", 
  #         legend.key.width = unit(.5, "in"),
  #         legend.key.height = unit(.1, "in"),
  #         legend.title.align = .5,
  #         text = element_text(family = font.family, size = font.size),
  #         aspect.ratio = .5)
  #   facet_wrap(~ProjectionScenario)
  
  #plot.out<- plot_grid(treeplot.val, treeplot.vol, nrow = 2)
  plot.out<- treeplot.val
  ggsave(paste(plot.path, community, ".ProjectedDistributionChangesTreeMap.jpg", sep = ""), plot.out, width = 11, height = 8, units = "in")
  return(plot.out)
}

sdm.land.nest<- sdm.land.nest %>%
  mutate("TreeMap.SDMVuln" = pmap(list(community = jgs, df = SDMData, top.species = Top.Species), treemap_sdmvuln_func))


# Example footprint and distribution plot ---------------------------------
library(ggmap)
library(akima)
library(sf)
# What will we need for this? Species projections data, port name, gear type
# Species projections
proj.file<- "~/GitHub/COCA/Results/NormalVoting_BiomassIncPresNoExposure_03152019/SDMPredictions.rds"
results<- read_rds(proj.file)

# Fishing footprints -- from COCA project code
# Bring in fishing footprints
all.foot.dat<- readRDS("~/GitHub/COCA/Data/VTR fishing footprints by community and gear type 2011-2015.rds")
ports.names<- all.foot.dat$JGS.COMMUNITY

# Spatial projections
proj.wgs84<- "+init=epsg:4326" #WGS84
proj.utm<- "+init=epsg:2960" #UTM 19

# Some work on the names
ports.names<- gsub("\\_(?=[^_]*\\_)", " ", ports.names, perl = TRUE)
ports.names<- gsub(' +', ' ', ports.names)
ports.names<- gsub("/", " ", ports.names)

port.and.state<- strsplit(ports.names, split = "_")

ports.geo<- data.frame("PortName" = unlist(lapply(port.and.state, "[", 1)), "PortState" = unlist(lapply(port.and.state, "[", 2)))

# Let's get the lat and long for these areas -- first write it out to do some small edits
google.api<- "AIzaSyDqGwT79r3odaMl0Hksx9GZhHmqe37KSEQ"
register_google(key = google.api)
geos.longlats<- geocode(location = unique(paste(ports.geo$PortName, ports.geo$PortState)), output = "latlon")
geo.merge<- data.frame("PortMerge" = unique(paste(ports.geo$PortName, ports.geo$PortState)), "Long" = geos.longlats$lon, "Lat" = geos.longlats$lat)

ports.geo<- ports.geo %>%
  mutate(., PortMerge = paste(PortName, PortState)) %>%
  left_join(., geo.merge)

ports.geo$JGSCommunity<- all.foot.dat$JGS.COMMUNITY

# Let's check this
write_csv(ports.geo, "~/GitHub/COCA/Data/ports.geocoded.csv")

# Had to update Hampton Bays/Shinnecock...read it back in
ports.geo<- read_csv("~/GitHub/COCA/Data/ports.geocoded.csv")

# Fishing port footprints -- gear type specific
gear.types<- all.foot.dat$COST_ID
gear.types<- ifelse(gear.types == 1, "Dredge",
                    ifelse(gear.types == 2, "Gillnet",
                           ifelse(gear.types == 3, "Longline",
                                  ifelse(gear.types == 4, "Pot/Trap",
                                         ifelse(gear.types == 5, "Purse/Seine",
                                                ifelse(gear.types == 6, "Trawl", "Other"))))))
port.foot.names<- paste(ports.names, gear.types, sep = "-")

# Safe vs. unsafe
unsafe<- FALSE
if(unsafe){
  ports.all.foots<- all.foot.dat$JGS.COMMUNITY.GEAR.FOOTPRINTS
} 

if(!unsafe) {
  ports.all.foots<- all.foot.dat$JGS.NOAA.SAFE.COMMUNITY.GEAR.FOOTPRINTS
}
names(ports.all.foots)<- port.foot.names

# Get proportion layer we want for each port-gear type
port.foots<- unlist(lapply(ports.all.foots, "[", 3))
port.foots.stack<- raster::stack(port.foots) # 476 layers
names1<- names(port.foots.stack)
rast.ind<- nlayers(port.foots.stack)

# Also need an "all gear" option and a max distance option
if(unsafe){
  ports.only<- str_replace_all(names(port.foots.stack), c(".Pot.Trap.JGS.PROPORTION" = "", ".Other.JGS.PROPORTION" = "", ".Gillnet.JGS.PROPORTION" = "", ".Trawl.JGS.PROPORTION" = "", ".Dredge.JGS.PROPORTION" = "", ".Purse.Seine.JGS.PROPORTION" = "", ".Longline.JGS.PROPORTION" = ""))
} 

if(!unsafe){
  ports.only<- str_replace_all(names(port.foots.stack), c(".Pot.Trap.JGS.SAFE.PROPORTION" = "", ".Other.JGS.SAFE.PROPORTION" = "", ".Gillnet.JGS.SAFE.PROPORTION" = "", ".Trawl.JGS.SAFE.PROPORTION" = "", ".Dredge.JGS.SAFE.PROPORTION" = "", ".Purse.Seine.JGS.SAFE.PROPORTION" = "", ".Longline.JGS.SAFE.PROPORTION" = ""))
}

ports.unique<- unique(ports.only) # 126 ports

all.stack<- raster::stack()

for(i in 1:length(ports.unique)){
  port.use<- ports.unique[i]
  port.foots.ind<- which(grepl(port.use, names(port.foots.stack)), arr.ind = T)
  stack.use<- port.foots.stack[[port.foots.ind]]
  if(nlayers(stack.use) == 1){
    all.gear.out<- stack.use[[1]]
  } else {
    all.gear.out<- calc(stack.use, sum, na.rm = T)
  }
  all.stack<- raster::stack(all.stack, all.gear.out)
}

# Combine
if(unsafe){
  names.all<- c(names1, paste(ports.unique, ".All.JGS.PROPORTION", sep = ""))
} 
if(!unsafe) {
  names.all<- c(names1, paste(ports.unique, ".All.JGS.SAFE.PROPORTION", sep = ""))
}
check.stack<- raster::stack(port.foots.stack, all.stack)
names(check.stack)<- names.all
port.foots.stack<- check.stack

# Now max distance option
dist.fac<- 1.5

# Similar loop, but we don't want to do this for the "All gear" scenario...
if(unsafe){
  port.foots.stack.noall<- port.foots.stack[[-which(grepl(".All.JGS.PROPORTION", names(port.foots.stack)))]]
} 
if(!unsafe) {
  port.foots.stack.noall<- port.foots.stack[[-which(grepl(".All.JGS.SAFE.PROPORTION", names(port.foots.stack)))]]
}

# Empty stack for results
stack.maxd<- raster::stack()

# Loop
for(i in 1:nlayers(port.foots.stack.noall)){
  lay.use<- port.foots.stack.noall[[i]]
  
  if(unsafe){
    port.name.use<- str_replace_all(names(lay.use), c(".Pot.Trap.JGS.PROPORTION" = "", ".Other.JGS.PROPORTION" = "", ".Gillnet.JGS.PROPORTION" = "", ".Trawl.JGS.PROPORTION" = "", ".Dredge.JGS.PROPORTION" = "", ".Purse.Seine.JGS.PROPORTION" = "", ".Longline.JGS.PROPORTION" = ""))
  }
  
  if(!unsafe) {
    port.name.use<- str_replace_all(names(lay.use), c(".Pot.Trap.JGS.SAFE.PROPORTION" = "", ".Other.JGS.SAFE.PROPORTION" = "", ".Gillnet.JGS.SAFE.PROPORTION" = "", ".Trawl.JGS.SAFE.PROPORTION" = "", ".Dredge.JGS.SAFE.PROPORTION" = "", ".Purse.Seine.JGS.SAFE.PROPORTION" = "", ".Longline.JGS.SAFE.PROPORTION" = ""))
  }
  pt.use<- unique(ports.geo[str_replace_all(ports.geo$JGSCommunity, "[^[:alnum:]]", "") == str_replace_all(port.name.use, "[^[:alnum:]]", ""),])
  coordinates(pt.use)<- ~Long+Lat
  proj4string(pt.use)<- proj.wgs84
  
  # Get distance from port, then get maximum distance given fishing for max dist and maxdist * 1.5
  dist.rast<- distanceFromPoints(lay.use, pt.use)
  max.d<- dist.rast
  max.d.maxval<- maxValue(raster::mask(dist.rast, lay.use))
  max.d[max.d >= max.d.maxval]<- NA
  max.d<- raster::mask(max.d, lay.use, inverse = T)
  
  max.d.fac<- dist.rast
  max.d.fac[max.d.fac >= max.d.maxval*dist.fac]<- NA
  max.d.fac<- raster::mask(max.d.fac, lay.use, inverse = T)
  
  stack.maxd<- raster::stack(stack.maxd, max.d, max.d.fac)
  print(port.name.use)
  print(i)
}

# Combine
if(unsafe){
  names.stackmaxd<- paste(rep(names(port.foots.stack.noall), each = 2), c("MaxD.JGS.PROPORTION", "1.5xMaxD.JGS.PROPORTION"), sep = "")
}
if(!unsafe) {
  names.stackmaxd<- paste(rep(names(port.foots.stack.noall), each = 2), c("MaxD.JGS.SAFE.PROPORTION", "1.5xMaxD.JGS.SAFE.PROPORTION"), sep = "")
}
names(stack.maxd)<- names.stackmaxd
names.all<- c(names(port.foots.stack), names.stackmaxd)
check.stack<- raster::stack(port.foots.stack, stack.maxd)
names(check.stack)<- names.all
port.foots.stack<- check.stack
proj4string(port.foots.stack)<- proj.wgs84

# Save em as a raster stack
type<- if(unsafe){
  "unsafe"
} else if(!unsafe){
  "safe"
}
writeRaster(port.foots.stack, paste("~/GitHub/COCA/Data/All VTR ", type, " fishing footprints by community and gear type 2011-2015.grd", sep = ""), overwrite = TRUE)

#### Okay, now for each focal community, show one species and gear type footprint...
focal.comms<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
spp.gear.df<- data.frame("Community" = focal.comms, "Species" = c("AMERICAN LOBSTER", "LONGFIN SQUID", "SEA SCALLOP", "SUMMER FLOUNDER"), "Season" = c("FALL", "SPRING", "FALL", "SPRING"), "Gear" = c("Pot.Trap", "Trawl", "Dredge", "Trawl"))
foot.only<- TRUE

for(i in seq_along(focal.comms)){
  
  # Get correct variables from spp.gear.df
  df.use<- spp.gear.df[i,]
  
  # Extract projections
  proj.vals.base<- results %>%
    filter(., COMNAME == df.use$Species, Proj.Class == "Baseline.combo.b" & SEASON == df.use$Season) %>%
    unnest()
  
  proj.diff<- results %>%
    filter(., COMNAME == df.use$Species, Proj.Class == "Future_mean_diff.combo.b" & SEASON == df.use$Season) %>%
    unnest()
  
  # Rescaling for nicer maps...
  # NELME domaine
  nelme<- st_read("~/GitHub/COCA/Data/NELME_clipped.shp")
  st_crs(nelme)<- proj.wgs84
  
  # Interpolation grid
  coords.df<- data.frame("x" = proj.vals.base$x, "y" = proj.vals.base$y)
  pred.df<- na.omit(data.frame("x" = coords.df$x, "y" = coords.df$y, "layer" = rep(0, length(coords.df$x))))
  pred.df.interp<- interp(pred.df[,1], pred.df[,2], pred.df[,3], duplicate = "mean", extrap = TRUE,
                          xo=seq(-87.99457, -57.4307, length = 115),
                          yo=seq(22.27352, 48.11657, length = 133))
  pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(pred.df.interp$z))
  pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
  
  # Baseline
  data.use<- proj.vals.base
  pred.df.base<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
  pred.df.interp<- interp(pred.df.base[,1], pred.df.base[,2], pred.df.base[,3], duplicate = "mean", extrap = TRUE,
                          xo=seq(-87.99457, -57.4307, length = 115),
                          yo=seq(22.27352, 48.11657, length = 133))
  pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
  pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
  
  # Clip to nelme
  pred.df.temp.base<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
  coords.keep<- as.data.frame(st_coordinates(pred.df.temp.base))
  row.names(coords.keep)<- NULL
  pred.df.base<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.base$z)))
  names(pred.df.base)<- c("long", "lat", "z")
  
  # Difference
  data.use<- proj.diff
  pred.df.diff<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
  pred.df.interp<- interp(pred.df.diff[,1], pred.df.diff[,2], pred.df.diff[,3], duplicate = "mean", extrap = TRUE,
                          xo=seq(-87.99457, -57.4307, length = 115),
                          yo=seq(22.27352, 48.11657, length = 133))
  pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
  pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
  
  # Clip to nelme
  pred.df.temp.diff<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
  coords.keep<- as.data.frame(st_coordinates(pred.df.temp.diff))
  row.names(coords.keep)<- NULL
  pred.df.diff<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.diff$z)))
  names(pred.df.diff)<- c("long", "lat", "z")
  
  # Extract footprint
  foot.ind<- paste(df.use$Community, ".", df.use$Gear, ".", "JGS.SAFE.PROPORTION", sep = "")
  foot.use<- port.foots.stack[[which(names(port.foots.stack) == foot.ind)]]
  
  # Reclassify
  m <- c(0, Inf, 1,  -Inf, 0, 0)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  lay.bin<- raster::reclassify(foot.use, rclmat)
  
  # Get coordinates of footprint
  foot.pts <- as.data.frame(lay.bin, xy = TRUE)
  foot.pts$layer<- ifelse(foot.pts$layer == 1, "1", NA)
  
  if(foot.only){
    # Make the plots
    keep.preds<- raster::extract(lay.bin, pred.df.temp.diff)
    
    pred.df.base<- pred.df.base[which(keep.preds == 1),]
    pred.df.diff<- pred.df.diff[which(keep.preds == 1),]
    

    # Spatial stuff -- gets us the states and shoreline
    state.data<- as_tibble(map_data("state")) %>% 
      rename(state_group = group, state_order = order) %>% 
      filter(region %in% c("maine", "new hampshire", "massachusetts", 
                           "vermont", "rhode island", "connecticut", "new york", "new jersey",
                           "pennsylvania", "delaware", "maryland", "district of columbia", "virginia", 
                           "north carolina"))
    
    # Alright, plot time
    gmri.orange <- "#EA4F12"
    gmri.light.gray <- "#E9E9E9"
    gmri.blue <- "#00608A"
    font.style <- "Arial Narrow"
    font.size <- 8
    gmri.gray <- "#535353"
    
    # Arbitrary labeling
    pred.df.base<- pred.df.base %>%
      drop_na(z)
    base.breaks.use<- c(0, as.numeric(quantile(pred.df.base$z)))
    base.labels.use<- c("Absent", "Low", "Fair", "Average", "Moderate", "High")
    
    pred.df.diff<- pred.df.diff %>%
      drop_na(z)
    diff.breaks.use<- c(min(pred.df.diff$z), -0.001, 0.001, max(pred.df.diff$z))
    diff.labels.use<- c("Decreasing", "", "Neutral", "Increasing")
    
    plot.out.base<- ggplot() +
      # Update "fill" and "color" to change map color
      geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                   alpha = .15, colour = "white") + 
      # Here you'd make adjustments to the point colors...
      geom_tile(data = pred.df.base, aes(x = long, y = lat, fill = z)) +
      scale_fill_gradient(name = "2011-2015\nRelative biomass", low = gmri.light.gray, high = gmri.orange, breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
      #geom_tile(data = foot.pts, aes(x = x, y = y, color = layer), fill = NA, show.legend = FALSE, size= 0.75) +
      #scale_color_manual(name = "Fished cells", values = c(gmri.gray, NA)) +
      xlim(c(-74, -66.5)) + 
      ylim(c(40, 47.5)) +
      theme_map() + 
      coord_fixed(1.3) +
      theme(legend.position = "right", text = element_text(family = font.style, size = font.size)) 
    
    plot.out.diff<- ggplot() +
      # Update "fill" and "color" to change map color
      geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                   alpha = .15, colour = "white") + 
      # Here you'd make adjustments to the point colors...
      geom_tile(data = pred.df.diff, aes(x = long, y = lat, fill = z)) +
      scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = c(min(pred.df.diff$z), max(pred.df.diff$z))) +
      #geom_tile(data = foot.pts, aes(x = x, y = y, color = layer), fill = NA, show.legend = FALSE, size= 0.75) +
      #scale_color_manual(name = "Fished cells", values = c(gmri.gray, NA)) +
      xlim(c(-74, -66.5)) + 
      ylim(c(40, 47.5)) +
      theme_map() + 
      coord_fixed(1.3) +
      theme(legend.position = "right", text = element_text(family = font.style, size = font.size)) 
    
    plot.out<- plot_grid(plot.out.base, plot.out.diff, nrow = 1, align = "hv")
    ggsave(paste(plot.path, df.use$Community, df.use$Species, df.use$Season, df.use$Gear, "maps.jpg", sep = ""), plot.out, width = 11, height = 8, units = "in")
  }
  

}

