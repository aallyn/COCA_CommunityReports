library_check<- function(libraries) {
  ## Details
  # This function will check for and then either load or install any libraries needed to run subsequent functions
  
  # Args:
  # libraries = Vector of required library names
  
  # Returns: NA, just downloads/loads libraries into current work space.
  
  ## Start function
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  ## End function
}

bind_cfders<- function(landings.path = landings.path, out.path = landings.path){
  ## Details
  # This function reads in yearly CFDERS datasets and binds them together. The individual reading and saving is incredible time consuming. So, ideally shouldn't have to do this more than once. After it is done, we can use the full dataset
  
  # Args:
    # data.path = Path to the CFDERS data files
    # out.path = Path to save combined 2011-2015 files; defaults to the same directory as data.path
  # Returns: Data frame with all of the yearly CFDERS datasets. This dataframe is saved as a flat file in out.path directory
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    landings.path = "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Landings/"
    out.path = "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Landings/"
  }
  
  # Read in yearly cfders datasets
  df<- list.files(path = landings.path, pattern = "cfders_", full.names = TRUE) %>% 
    map_df(~read.csv(.))
  # Make all columns lower case 
  colnames(df)<- tolower(colnames(df))
  
  # Save it 
  write_csv(df, paste(out.path, "2011_to_2015_combined_cfders.csv", sep = ""))
}

cfders_community_name_match<- function(data.path = ref.tables.path, out.path = ref.tables.path){
  ## Details
  # This function reads in a list of unique cfders ports and unique VTR communities from "VTR fishing footprints by community and gear type 2011-2015.rds" and does the beginning steps to match names. Importantly, this can only get us so far and there is some additional post function processing that goes on. Not for future processing and alignment of cfders data with species distribution model and vtr community data
  
  # Args:
  # data.path = Path to the CFDERS data file of unique names and to VTR raster data for community names
  # out.path = Path to save file that has matched names and miss matched names (again, this requires additional processing to get to the fully matched reference table "VTR_CFDERS_Comparison_Edited_NoCounties.csv"). Defaults to data.path
  
  # Returns: NA
  
  ## Start function
  # Install libraries
  library_check(c("raster", "tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    data.path = "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Reference Tables/"
    out.path = data.path
  }
  
  # Read in VTR community names
  vtr.dat<- read_csv(paste(data.path, "VTRCommunities.csv", sep = ""))
  comm.names<- vtr.dat$Community
  
  # Read in CFDERS ports
  cfders.dat<- read_csv(paste(data.path, "port_name_list.csv", sep = "")) %>%
    dplyr::select(., -X1)
  #unique(cfders.dat$PORT) # 748 unique port codes
  #unique(cfders.dat$PORT_NAME) # Only 684 unique port names
  #unique(cfders.dat$BRAD_PORT_NAME_STATE) # 708 unique brad port names
  
  # Preliminary inspection -- what are going to be some known issues? what column of CFDERRS is most like the VTR community names
  #head(comm.names)
  #head(cfderrs.dat)
  
  # Well, immediate issue is that the VTR communities have multiple "_". Let's split the names by the LAST instance of the "_". This should give us two columns, one for community, one for state. Then we can remove any special characters or spaces from the community column in the VTR data as well as the PORT_NAME column for the cfderrs data. Finally, combining the stripped commuinity column with the second state column for VTR and then matching up to a combined stripped PORT_NAME column and PORT_STATE from the CFDERRS should give us the best chance to match. 
  # VTR data first
  comm.names.split<- strsplit(comm.names, "_\\s*(?=[^_]+$)", perl=TRUE)
  
  # First item in each list element will have community, second has the state...
  comm.dat<- data.frame("JGS" = comm.names, "CommunityOnly" = unlist(lapply(comm.names.split, "[", 1)), "StateOnly" = unlist(lapply(comm.names.split, "[", 2)))
  
  # Did that make sense?
  #unique(comm.dat$StateOnly)
  #unique(comm.dat$CommunityOnly)
  
  # Seems good...lets clean up the Community only column to remove all spaces and special characters
  comm.dat$CommunityOnlyStripped<- str_replace_all(comm.dat$CommunityOnly, "[^[:alnum:]]", "")
  
  # Make the community merge column
  comm.dat$MatchColumn<- paste(comm.dat$CommunityOnlyStripped, comm.dat$StateOnly, sep = "")
  
  # Now CFDERS. In Brad's, there is some addition bizarreness as there are many Port's that have Name (County). So, first get rid of anything bizarre in parentheses?
  cfders.dat$NoParen<- str_replace(cfders.dat$BRAD_PORT_NAME_STATE, " \\(.*\\)", "")
  # Once more, without space before paren
  cfders.dat$NoParen<- str_replace(cfders.dat$NoParen, "\\(.*\\)", "")
  
  # Looks good, no strip characters
  cfders.dat$MatchColumn<- str_replace_all(cfders.dat$NoParen, "[^[:alnum:]]", "")
  
  ## Merge them together and save the result
  comm.dat.unique<- comm.dat[!duplicated(comm.dat$JGS),]
  comm.cfders.dat<- comm.dat.unique %>%
    left_join(., cfders.dat)
  names.missed<- comm.cfders.dat[is.na(comm.cfders.dat$BRAD_PORT_NAME_STATE),]
  write_csv(comm.cfders.dat, paste(out.path, "VTR_CFDERS_Comparison.csv", sep = ""))
  write_csv(names.missed, paste(out.path, "VTR_CFDERS_Missed.csv", sep = ""))
  
  ## What is going on with these misses?
  #names.missed$JGS
  
  # Some make a lot of sense -- a bunch of smaller ports combined into one community as in Stonington_Mystic_Pawcatuck_CT. Others though, seem weird. Like Provincetown_MA, Ocean City_MD, Beals Island_ME, Stueben_ME. 
  
  # Can we find those in the original CFDERRS data?
  port.search<- c("PROVINCETOWN", "OCEAN CITY", "BEALS", "STUEBEN")
  
  # All of the string detection stuff seems to be behaving oddly. Going back to the basics...
  for(i in seq_along(port.search)){
    port.search.use<- port.search[i]
    temp<- cfders.dat[grepl(port.search.use, cfders.dat$BRAD_PORT_NAME_STATE),]
    
    if(i == 1){
      cfders.searched<- temp
    } else {
      cfders.searched<- bind_rows(cfders.searched, temp)
    }
  }
  
  # Alright, so that helps a little bit. We could manually enter these. For the other ones, those are all going to be combinations of ports, I think?
  find<- "REEDVILLE"
  find.df<- data.frame(cfders.dat[grepl(find, cfders.dat$BRAD_PORT_NAME_STATE),])
  #find.df
  
  # To fill these in, I went through the VTR_CFDERS_Comparison file that is generated from the code above. For one's with NAs for Brad's stuff, I then searched the CFDERS data and added them in manually. I tried to cover all combinations of things. For some, this means we now have multiple rows for the CFDERS stuff for one JGS community (for example: Stonington, Mystic, Pawcatuck has two rows for Brad/CFDERS stuff as Stonington and Mystic are unique ports). This was pretty straight forward, with a few exceptions. Like "Prospect" -- there is a Prospect Township and a Prospect Harbor. I only included the Harbor as the Township was island and a long ways from Gouldsboro/Corea and other ports in that JGS community. Finally, I deleted all incidences of (State) or (County). The resulting file is "VTR_CFDERS_Comparison_Edited_NoCounties.csv"
  
  # End function
}
c
cfders_community_longlat<- function(data.path = ref.tables.path, google.api, out.path = ref.tables.path){
  ## Details
  # This is a real simple function that reads in the "VTR_CFDERS_Comparison_Edited_NoCounties.csv" and then gets the long/lat information for each community/port.
  
  # Args:
  # data.path = Path to the "VTR_CFDERS_Comparison_Edited_NoCounties.csv"
  # google.api = API key to use google to extract long/lats
  # out.path = Path to save file that has matched VTR and CFDERS names with associated long/lats
  
  # Returns: NA
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse", "ggmap"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    data.path = "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Reference Tables/"
    google.api = "AIzaSyDqGwT79r3odaMl0Hksx9GZhHmqe37KSEQ"
    out.path = data.path
  }
  
  # Read in community/port matched names file
  comm.dat<- read_csv(paste(data.path, "VTR_CFDERS_Comparison_Edited_NoCounties.csv", sep = ""))
  
  # LongLat Data
  register_google(key = google.api)
  geos.longlats<- geocode(location = unique(comm.dat$BRAD_PORT_NAME_STATE), output = "latlon")
  comm.geo<- data.frame("BRAD_PORT_NAME_STATE" = unique(comm.dat$BRAD_PORT_NAME_STATE), "Long" = geos.longlats$lon, "Lat" = geos.longlats$lat)
  
  # Add long lat data to comm.dat and save it
  comm.dat<- comm.dat %>%
    left_join(., comm.geo)
  write_csv(comm.dat, paste(out.path, "VTR_CFDERS_Comparison_Edited_NoCounties_LongLat.csv", sep = ""))
  
}

summarize_cfders<- function(landings.path = landings.path, ref.tables.path = ref.tables.path, out.path = proc.summ.path, focal.comms = c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")){
  ## Details
  # This function summarizes CFDERS landed value and volume data and dealer data. Summaries are provided for:
    # Community - Gear - Species: 2011-2015 Total Landed Value/Volume, Mean Annual Landed Value/Volume, number of distinct dealers for focal communities
    # Community - Species: 2011-2015 Total landed Value, Volume, Mean Annual Landed Value/Volume, number of distinct dealers for all communities
  
  # Args:
    # landings.path = Path to the CFDERS datafile (created by bind_cfders)
    # ref.tables.path = Path to VTR_CFDERS matching names reference table
    # out.path = Path to save summary files
    # focal.comms = VTR focal communities to filter full dataset before calculating gear-species summaries

  # Returns: List of tidy datasets, one for each of the summary combinations outlined above. Each of these is also saved individually to the output location specified by out.path
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    landings.path =  "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Landings/"
    ref.tables.path =  "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Reference Tables/"
    out.path = "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/"
    focal.comms<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
  }
  
  # Bring in cfders and vtr community data
  cfders<- read_csv(paste(landings.path, "2011_to_2015_combined_cfders.csv", sep = ""),
                    col_types = cols(cf_license = col_character(),
                                     state_dnum = col_character(),
                                     dealer_rpt_id = col_integer(),
                                     landing_seq = col_integer(),
                                     doe = col_character(),
                                     entry_date = col_character(),
                                     landing_date_safis = col_character(), 
                                     doc = col_character(),
                                     vtrserno = col_character(),
                                     docid = col_character(),
                                     area = col_character(),
                                     harvest_area = col_integer()))
  cfders.orig<- cfders
  vtr.comm<- read_csv(paste(ref.tables.path, "VTR_CFDERS_Comparison_Edited_NoCounties_LongLat.csv", sep = ""))
  colnames(vtr.comm)<- tolower(colnames(vtr.comm))
  
  # Need a walk between -- use the CFDERS port_name_list reference table
  port.list<- read_csv(paste(ref.tables.path, "port_name_list.csv", sep = "")) %>%
    dplyr::select(., -X1) 
  colnames(port.list)<- tolower(colnames(port.list))
  
  # Join to cfders by port code...
  cfders<- cfders %>%
    left_join(., port.list, by = "port")
  
  # Now, bring in vtr.comm to cfders 
  df<- cfders %>%
    left_join(., vtr.comm)
  
  # Data summaries 
  # First some house keeping, get rid of NA values, NA port names and County or State port names
  df.sub<- df %>%
    drop_na(., spplndlb, sppvalue, jgs) 
  
  # Now summaries -- every kind of summary that doesn't include community
  # Empty list to store the results
  out.list<- vector("list", length = 2)
  
  # Focal communities by gear first
  focalcomm.spp.gear.temp<- df.sub %>%
    filter(., jgs %in% focal.comms) %>%
    mutate(., gear.type = case_when(negear_name %in% c('BEAM TRAWL, OTHER/NK SPECIES', 'BEAM TRAWL,FISH',
                                                       'OTTER TRAWL, BEAM','OTTER TRAWL, BOTTOM,FISH',
                                                       'OTTER TRAWL, BOTTOM,OTHER', 'OTTER TRAWL, BOTTOM,SCALLOP',
                                                       'OTTER TRAWL, BOTTOM,SHRIMP','OTTER TRAWL, HADDOCK SEPARATOR',
                                                       'OTTER TRAWL, MIDWATER','OTTER TRAWL, RUHLE',
                                                       'OTTER TRAWL,BOTTOM,TWIN','PAIR TRAWL, MIDWATER',
                                                       'TRAWL,OTTER,BOTTOM PAIRED','TRAWL,OTTER,BOTTOM,FISH',
                                                       'TRAWL,OTTER,BOTTOM,OTHER/NK SPECIES',
                                                       'TRAWL,OTTER,BOTTOM,SCALLOP','TRAWL,OTTER,BOTTOM,SHRIMP',
                                                       'TRAWL,OTTER,MIDWATER', 'TRAWL,OTTER,MIDWATER PAIRED')  ~ 'Trawl',
                                    
                                    negear_name %in% c('PURSE SEINE, OTHER/NK SPECIES','SEINE, PURSE')  ~ 'Purse-Seine',
                                    
                                    negear_name %in% c('POT, CONCH/WHELK',    'POT, CRAB',
                                                       'POT, EEL', 'POT, FISH', 'POT, HAG',    'POT, LOBSTER',
                                                       'POT, OTHER','POT/TRAP, LOBSTER INSH NK',
                                                       'POT/TRAP, LOBSTER OFFSH NK', 'POTS + TRAPS, HAGFISH',
                                                       'POTS + TRAPS,EEL', 'POTS + TRAPS,FISH',
                                                       'POTS + TRAPS,OTHER/NK SPECIES', 'TRAP') ~ 'Pots / Traps',
                                    
                                    negear_name %in% c('LONGLINE, BOTTOM', 'LONGLINE, PELAGIC') ~ 'Longline',
                                    
                                    negear_name %in% c('GILL NET, ANCHORED-FLOATING, FISH', 'GILL NET, DRIFT,LARGE MESH',
                                                       'GILL NET, DRIFT,SMALL MESH','GILL NET, DRIFT-SINK, FISH',
                                                       'GILL NET, FIXED OR ANCHORED,SINK, OTHER/NK SPECIES',
                                                       'GILL NET, OTHER','GILL NET, RUNAROUND', 'GILL NET, SINK') ~ 'Gillnet',
                                    
                                    negear_name %in% c('DREDGE, CLAM','DREDGE, OCEAN QUAHOG/SURF CLAM',
                                                       'DREDGE, OTHER','DREDGE, OTHER/NK SPECIES',
                                                       'DREDGE, SCALLOP,SEA','DREDGE, SCALLOP-CHAIN MAT',
                                                       'DREDGE, SURF CLAM + OCEAN QUAHO','DREDGE, URCHIN',
                                                       'DREDGE,SCALLOP,CHAIN MAT,MOD') ~ 'Dredge'))
  
  # First get yearly totals of landed value and volume
  yr.focalcomm.spp.gear<- focalcomm.spp.gear.temp %>%
    group_by(year, jgs, long, lat, spp_common_name, gear.type) %>% 
    summarize(.,
              "sppvalue.sum" = sum(sppvalue, na.rm = TRUE),
              "spplndlb.sum" = sum(spplndlb, na.rm = TRUE)) %>%
    ungroup()
  
  # Now, get sum across years and average annual landed value and volume
  focalcomm.spp.gear<- yr.focalcomm.spp.gear %>%
    group_by(jgs, long, lat, spp_common_name, gear.type) %>%
    summarize(., 
              "Totalsppvalue" = sum(sppvalue.sum, na.rm = TRUE),
              "Totalspplndlb" = sum(spplndlb.sum, na.rm = TRUE),
              "Meansppvalue" = mean(sppvalue.sum, na.rm = TRUE),
              "Meanspplndlb" = mean(spplndlb.sum, na.rm = TRUE)) %>%
    ungroup()
  
  # Also want distinct dealers across years
  focalcomm.dealers<- focalcomm.spp.gear.temp %>%
    group_by(jgs, long, lat, spp_common_name, gear.type) %>%
    summarize(., 
              "DistinctDealers" = n_distinct(dealnum)) %>%
    ungroup() 
  
  # Join dealers with focalcomm.spp.gear and save the file
  focalcomm.spp.gear<- focalcomm.spp.gear %>%
    left_join(., focalcomm.dealers)
  
  out.list[[1]]<- focalcomm.spp.gear
  names(out.list)[1]<- "FocalComm.Spp.Gear"

  # Next, all communities by species (no gear) 
  # First get yearly totals of landed value and volume
  yr.comm.spp<- df.sub %>%
    group_by(year, long, lat, jgs, spp_common_name) %>% 
    summarize(.,
              "sppvalue.sum" = sum(sppvalue, na.rm = TRUE),
              "spplndlb.sum" = sum(spplndlb, na.rm = TRUE)) %>%
    ungroup()
  
  # Now, get sum across years and average annual landed value and volume
  comm.spp<- yr.comm.spp %>%
    group_by(jgs, long, lat, spp_common_name) %>%
    summarize(., 
              "Totalsppvalue" = sum(sppvalue.sum, na.rm = TRUE),
              "Totalspplndlb" = sum(spplndlb.sum, na.rm = TRUE),
              "Meansppvalue" = mean(sppvalue.sum, na.rm = TRUE),
              "Meanspplndlb" = mean(spplndlb.sum, na.rm = TRUE)) %>%
    ungroup()
  
  # Also want distinct dealers across years
  comm.dealers<- df.sub %>%
    group_by(jgs, long, lat, spp_common_name) %>%
    summarize(., 
              "DistinctDealers" = n_distinct(dealnum)) %>%
    ungroup() 
  
  # Join dealers with comm.spp  and save the file
  comm.spp<- comm.spp %>%
    left_join(., comm.dealers)
  
  out.list[[2]]<- comm.spp
  names(out.list)[2]<- "Comm.Spp"
  
  # Save each independently and then return the list
  for(i in seq_along(out.list)){
    write_csv(data.frame(out.list[[i]]), path = paste(out.path, names(out.list[i]), "summary.csv", sep = ""))
  }
  return(out.list)
  # End function
} 

summarize_gar<- function(landings.path = landings.path, ref.tables.path = ref.tables.path, out.path = landings.path){
  ## Details
  # This function summarizes the GAR landings file to get the total landings by community and species for the 2011 to 2015 baseline period
  
  # Args:
  # landings.path = Path to the GAR 1982-2015 datafile 
  # ref.tables.path = Path to VTR_CFDERS matching names reference table
  # out.path = Path to save new GAR landings file
 
  # Returns: Tidy dataset and saves new created file
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    landings.path =  "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Landings/"
    ref.tables.path =  "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/Reference Tables/"
    out.path = landings.path
  }

  
  # Bring in GAR data 
  gar.df<- read_csv(paste(landings.path, "Mills_1982-2015 GAR Landings combined sheets.csv", sep = ""),
                    col_types = cols(NHULL = col_integer(),
                                     NDLR = col_integer()))
  # Subset years
  keep.years<- c(2011, 2012, 2013, 2014, 2015)
  gar.base<- gar.df %>%
    filter(., YEAR %in% keep.years) 
  colnames(gar.base)<- tolower(colnames(gar.base))
  
  # Summarize
  yr.comm.spp<- gar.base %>%
    group_by(year, port, state, species) %>% 
    summarize(.,
              "sppvalue.sum" = sum(value, na.rm = TRUE),
              "spplndlb.sum" = sum(landed_lbs, na.rm = TRUE)) %>%
    ungroup()
  
  # Now, get sum across years and average annual landed value and volume
  comm.spp<- yr.comm.spp %>%
    group_by(port, state, species) %>%
    summarize(., 
              "Totalsppvalue" = sum(sppvalue.sum, na.rm = TRUE),
              "Totalspplndlb" = sum(spplndlb.sum, na.rm = TRUE),
              "Meansppvalue" = mean(sppvalue.sum, na.rm = TRUE),
              "Meanspplndlb" = mean(spplndlb.sum, na.rm = TRUE)) %>%
    ungroup()
  
  # Save comm.spp file and return it
  write_csv(comm.spp, path = paste(out.path, "GARsummary.csv", sep = ""))
  return(comm.spp)
  
  # End function
}

sdm_landings_merged<- function(sdm.path = sdm.path, landings.file, focal.comms = c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI"), ref.tables.path = ref.tables.path, out.path = proc.summ.path){
  ## Details
  # This function merges landings data (either from CFDERS OR GAR) to the projected species distribution model changes results and model fit results. Depending on which file is referenceD (CFDERS Focalcomm.Spp.Gear, CFDERS Comm.Spp, GAR), the produced file will be slightly different.
  
  # Args:
  # sdm.path = Path to the SDM "EcoToEconPortData03032019.csv" file and "mod.results.csv" file
  # landings.file = Path to either CFDERS "FocalComm.Spp.Gearsummary" summary file or "Comm.Sppsummary" summary file or GARsummary" file
  # focal.comms = Vector of focal communities, needed to run the CFDERS FocalComm.Spp.Gear component.
  # ref.tables.path = Path to community lat long file
  # out.path = Path to save new file that merges landed value and volume importance weights to the SDM EcoToEcon results -- hierarchy will be slightly different as if CFDERS FocalComm.Spp.Gear is supplied, weighting is done at a community - gear level. 

  # Returns: Joined dataset, which is also saved as csv to out.path
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    sdm.path<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Data/SDM Projections/"
    landings.file<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/Comm.Spp.Gearsummary.csv"
    focal.comms<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
    out.path<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/"
  }
  
  # For any of these, same process for dealing with model results
  mod.stats<- read_csv(paste(sdm.path, "mod.results.csv", sep = "")) %>%
    dplyr::select(-X1) %>%
    group_by(COMNAME) %>%
    summarize_if(., is.numeric, mean, na.rm = TRUE)
  colnames(mod.stats)[1]<- "CommonName"
  
  # First, determine what file is being used for weights CFDERS "FocalComm.Spp.Gear" summary file or "Comm.Spp" summary file or GAR summary file
  file.use<- basename(landings.file)
  
  # Proceed, depending on file type, to calculate the proportion value and volume, projected changes weighted by value and volume
  if(file.use == "FocalComm.Spp.Gearsummary.csv"){
    # Read in species changes and remove "All" gear 
    comm.diffs<- read_csv(paste(sdm.path, "EcoToEconPortData06032019.csv", sep = "")) %>%
      dplyr::select(., -X1) %>%
      filter(., Community %in% focal.comms & Gear != "All") %>%
      drop_na(CFDERSCommonName, Value) %>%
      distinct(., CommonName, CFDERSCommonName, Community, Gear, Footprint, ProjectionScenario, Statistic, Value)
    colnames(comm.diffs)[8]<- "ProjectionValue"
      
    # Merge over model results
    comm.diffs<- comm.diffs %>%
      left_join(., mod.stats)
    
    # Fisheries landings data
    fish.dat<- read_csv(landings.file) 
    colnames(fish.dat)<- c("Community", "Long", "Lat", "CFDERSCommonName", "Gear", "TotalValue", "TotalVolume", "MeanValue", "MeanVolume", "DistinctDealers")
    
    # Now, get total community value and volume
    fish.dat.agg<- fish.dat %>%
      group_by(., Community, Long, Lat) %>%
      summarize(., "CommTotalValue" = sum(TotalValue, na.rm = TRUE),
                "CommTotalVolume" = sum(TotalVolume, na.rm = TRUE))
    
    # Join community totals to community-species-gear dataset and then calculate proportion
    fish.dat<- fish.dat %>%
      left_join(., fish.dat.agg) %>%
      mutate(., "ProportionValue" = TotalValue/CommTotalValue,
             "ProportionVolume" = TotalVolume/CommTotalVolume)
    
    # Adjust gears to match the port.diffs
    fish.dat$Gear<- ifelse(fish.dat$Gear == "Pots / Traps", "Pot_Trap",
                           ifelse(fish.dat$Gear == "Purse-Seine", "Purse_Seine", fish.dat$Gear))
    fish.dat$Gear[is.na(fish.dat$Gear)]<- "Other"
    
    # Merge in fisheries landings data -- lat long first...
    comm.longlat<- fish.dat %>%
      dplyr::select(., Community, Long, Lat) %>%
      distinct()
    
    comm.diffs<- comm.diffs %>%
      left_join(., comm.longlat)
    
    # Now landings info...
    comm.diffs<- comm.diffs %>%
      left_join(., fish.dat)
    
    # Calculate weights and then weight changes
    #comm.diffs$ProjectionValue[is.infinite(comm.diffs$ProjectionValue)]<- 500
    comm.diffs<- comm.diffs %>%
      mutate(., "ChangeWeightedValue" = ProjectionValue*ProportionValue,
             "ChangeWeightedVolume" = ProjectionValue*ProportionVolume)
    
    # Characteristics of NAs?
    comm.diffs.nas<- comm.diffs[is.na(comm.diffs$Long),]
    # Seems okay...
    
    # Remove Skate issue...
    comm.diffs<- comm.diffs %>%
      dplyr::filter(., CFDERSCommonName != "SKATE,LITTLE/WINTER")
    
    # Write this out and return it
    write.csv(comm.diffs, file = paste(out.path, "SpeciesFocalCommunityGearTypeCFDERSWeightedChanges.csv", sep= ""))
    return(comm.diffs)
  }
  
  if(file.use == "Comm.Sppsummary.csv"){
    # No focal communities, so summaries only at ALL gear levels
    # Read in species changes
    comm.diffs<- read_csv(paste(sdm.path, "EcoToEconPortData06032019.csv", sep = "")) %>%
      dplyr::select(., -X1) %>%
      filter(., Gear == "All") %>%
      distinct(., CommonName, CFDERSCommonName, Community, Gear, Footprint, ProjectionScenario, Statistic, Value)
    colnames(comm.diffs)[8]<- "ProjectionValue"
    
    # Merge over model results
    comm.diffs<- comm.diffs %>%
      left_join(., mod.stats)
    
    # Fisheries landings data
    fish.dat<- read_csv(landings.file)
    colnames(fish.dat)<- c("Community", "Long", "Lat", "CFDERSCommonName", "TotalValue", "TotalVolume", "MeanValue", "MeanVolume", "DistinctDealers")
    
    # Now, get total community value and volume
    fish.dat.agg<- fish.dat %>%
      group_by(., Community, Long, Lat) %>%
      summarize(., "CommTotalValue" = sum(TotalValue, na.rm = TRUE),
                "CommTotalVolume" = sum(TotalVolume, na.rm = TRUE))
    
    # Join community totals to community-species-gear dataset and then calculate proportion
    fish.dat<- fish.dat %>%
      left_join(., fish.dat.agg) %>%
      mutate(., "ProportionValue" = TotalValue/CommTotalValue,
             "ProportionVolume" = TotalVolume/CommTotalVolume)
    
    # Merge in fisheries landings data
    comm.longlat<- fish.dat %>%
      dplyr::select(., Community, Long, Lat) %>%
      distinct()
    
    comm.diffs<- comm.diffs %>%
      left_join(., comm.longlat)
    
    # Now landings info...
    comm.diffs<- comm.diffs %>%
      left_join(., fish.dat)
    
    # Calculate weights and then weight changes
    #comm.diffs$ProjectionValue[is.infinite(comm.diffs$ProjectionValue)]<- 500
    comm.diffs<- comm.diffs %>%
      mutate(., "ChangeWeightedValue" = ProjectionValue*ProportionValue,
             "ChangeWeightedVolume" = ProjectionValue*ProportionVolume)
    
    # Remove Skate issue...
    comm.diffs<- comm.diffs %>%
      dplyr::filter(., CFDERSCommonName != "SKATE,LITTLE/WINTER")
    
    # Write this out and save it
    write.csv(comm.diffs, file = paste(out.path, "SpeciesCommunityCFDERSWeightedChanges.csv", sep= ""))
    return(comm.diffs)
  }
  
  if(file.use == "landport1115.csv"){
    # No focal communities, so summaries only at ALL gear levels
    # Read in species changes
    comm.diffs<- read_csv(paste(sdm.path, "EcoToEconPortData06032019.csv", sep = "")) %>%
      dplyr::select(., -X1) %>%
      filter(., Gear == "All")
    colnames(comm.diffs)[10]<- "ProjectionValue"
    
    # Merge over model results
    comm.diffs<- comm.diffs %>%
      left_join(., mod.stats)
    
    # Need to adjust CFDERSPortName to match up with LandPort file convention...
    comm.diffs$Match<- str_replace_all(comm.diffs$CFDERSPortName, "[^[:alnum:]]", "")
    
    # Fisheries landings data
    fish.dat<- read_csv(landings.file) %>%
      gather(., CommonName, Landings, -Port.short, -Port.state, -Port.long)
    fish.dat$Match<- paste(fish.dat$Port.short, fish.dat$Port.state, fish.dat$Port.long, sep = "")
    
    # A whole lot of crap with the port names...
    landport.ports<- fish.dat[,c(1:3)]
    landport.ports<- landport.ports[!duplicated(landport.ports),]
    
    temp<- landport.ports[is.na(landport.ports$Port.state),]
    temp$Port.state.new<- c("NY", "VA", "SC", "NY", "VA", "RI")
    temp$Port.long.new<- paste(temp$Port.short, temp$Port.state.new, sep = ".")
    temp$Match<- paste(temp$Port.short, temp$Port.state, temp$Port.long, sep = "")
    
    fish.dat[which(fish.dat$Match == temp$Match[[1]]),c(1:3)]<- temp[1,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[2]]),c(1:3)]<- temp[2,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[3]]),c(1:3)]<- temp[3,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[4]]),c(1:3)]<- temp[4,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[5]]),c(1:3)]<- temp[5,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[6]]),c(1:3)]<- temp[6,c(1,4,5)]
    
    # Okay, now...remove all symbols?
    fish.dat$Match<- paste(fish.dat$Port.short, fish.dat$Port.state, sep = "")
    fish.dat$Match<- str_replace_all(fish.dat$Match, "[^[:alnum:]]", "")
    
    # Clean up
    fish.dat<- fish.dat %>%
      dplyr::select(Match, CommonName, Landings)
   
    # Now, get total community value and volume
    fish.dat.agg<- fish.dat %>%
      group_by(., Match) %>%
      summarize(., "CommTotalLandings" = sum(Landings, na.rm = TRUE))
    
    # Join community totals to community-species-gear dataset and then calculate proportion
    fish.dat<- fish.dat %>%
      left_join(., fish.dat.agg) %>%
      mutate(., "ProportionLandings" = Landings/CommTotalLandings)
    
    # Merge in fisheries landings data
    comm.diffs<- comm.diffs %>%
      left_join(., fish.dat, by = c("Match", "CommonName"))
    comm.diffs
    summary(comm.diffs)
    
    # Calculate weights and then weight changes
    comm.diffs$ProjectionValue[is.infinite(comm.diffs$ProjectionValue)]<- 500
    comm.diffs<- comm.diffs %>%
      mutate(., "ChangeWeightedLandings" = ProjectionValue*ProportionLandings)
    
    # Write this out
    write.csv(comm.diffs, file = paste(out.path, "SpeciesCommunityLandPort1115WeightedChanges.csv", sep= ""))
  }
  
  if(file.use == "valport1115.csv"){
    # No focal communities, so summaries only at ALL gear levels
    # Read in species changes
    comm.diffs<- read_csv(paste(sdm.path, "EcoToEconPortData06032019.csv", sep = "")) %>%
      dplyr::select(., -X1) %>%
      filter(., Gear == "All")
    colnames(comm.diffs)[10]<- "ProjectionValue"
    
    # Merge over model results
    comm.diffs<- comm.diffs %>%
      left_join(., mod.stats)
    
    # Need to adjust CFDERSPortName to match up with LandPort file convention...
    comm.diffs$Match<- str_replace_all(comm.diffs$CFDERSPortName, "[^[:alnum:]]", "")
    
    # Fisheries landings data
    fish.dat<- read_csv(landings.file) %>%
      gather(., CommonName, Landings, -Port)
    fish.dat$Match<- paste(fish.dat$Port, fish.dat$Port.state, sep = "")
    
    # A whole lot of crap with the port names...
    valport.ports<- fish.dat[,c(1:2)]
    valport.ports<- valport.ports[!duplicated(valport.ports),]
    
    temp<- valport.ports[is.na(valport.ports$Port.state),]
    temp$Port.state.new<- c("NY", "VA", "SC", "NY", "VA", "RI")
    temp$Port.long.new<- paste(temp$Port.short, temp$Port.state.new, sep = ".")
    temp$Match<- paste(temp$Port.short, temp$Port.state, temp$Port.long, sep = "")
    
    fish.dat[which(fish.dat$Match == temp$Match[[1]]),c(1:3)]<- temp[1,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[2]]),c(1:3)]<- temp[2,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[3]]),c(1:3)]<- temp[3,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[4]]),c(1:3)]<- temp[4,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[5]]),c(1:3)]<- temp[5,c(1,4,5)]
    fish.dat[which(fish.dat$Match == temp$Match[[6]]),c(1:3)]<- temp[6,c(1,4,5)]
    
    # Okay, now...remove all symbols?
    fish.dat$Match<- paste(fish.dat$Port.short, fish.dat$Port.state, sep = "")
    fish.dat$Match<- str_replace_all(fish.dat$Match, "[^[:alnum:]]", "")
    
    # Clean up
    fish.dat<- fish.dat %>%
      dplyr::select(Match, CommonName, Landings)
    
    # Now, get total community value and volume
    fish.dat.agg<- fish.dat %>%
      group_by(., Match) %>%
      summarize(., "CommTotalLandings" = sum(Landings, na.rm = TRUE))
    
    # Join community totals to community-species-gear dataset and then calculate proportion
    fish.dat<- fish.dat %>%
      left_join(., fish.dat.agg) %>%
      mutate(., "ProportionLandings" = Landings/CommTotalLandings)
    
    # Merge in fisheries landings data
    comm.diffs<- comm.diffs %>%
      left_join(., fish.dat, by = c("Match", "CommonName"))
    comm.diffs
    summary(comm.diffs)
    
    # Calculate weights and then weight changes
    comm.diffs$ProjectionValue[is.infinite(comm.diffs$ProjectionValue)]<- 500
    comm.diffs<- comm.diffs %>%
      mutate(., "ChangeWeightedLandings" = ProjectionValue*ProportionLandings)
    
    # Write this out
    write.csv(comm.diffs, file = paste(out.path, "SpeciesCommunityVolPort1115WeightedChanges.csv", sep= ""))
  }
  
  if(file.use == "GARsummary.csv"){
    # No focal communities, so summaries only at ALL gear levels
    # Read in species changes
    comm.diffs<- read_csv(paste(sdm.path, "EcoToEconPortData06032019.csv", sep = "")) %>%
      dplyr::select(., -X1) %>%
      filter(., Gear == "All") %>%
      distinct(., CommonName, CFDERSCommonName, CFDERSPortName, Community, Gear, Footprint, ProjectionScenario, Statistic, Value)
    colnames(comm.diffs)[9]<- "ProjectionValue"
    
    # Merge over model results
    comm.diffs<- comm.diffs %>%
      left_join(., mod.stats)
    
    # Bring in long/lat
    longlat<- read_csv(paste(ref.tables.path, "VTR_CFDERS_Comparison_Edited_NoCounties_LongLat.csv", sep = "")) %>%
      dplyr::select(., JGS, Long, Lat) %>%
      distinct()
    colnames(longlat)[1]<- "Community"
    
    comm.diffs<- comm.diffs %>%
      left_join(., longlat, by = "Community")
    
    # Need to adjust CFDERSPortName to match up with LandPort file convention...
    comm.diffs$Match<- str_replace_all(comm.diffs$CFDERSPortName, "[^[:alnum:]]", "")
    
    # Fisheries landings data
    fish.dat<- read_csv(landings.file) 
    fish.dat$Match<- paste(fish.dat$port, fish.dat$state, sep = "")
    fish.dat$Match<- str_replace_all(fish.dat$Match, "[^[:alnum:]]", "")
    colnames(fish.dat)[3]<- "CFDERSCommonName"
    
    # Clean up
    fish.dat<- fish.dat %>%
      dplyr::select(Match, CFDERSCommonName, Totalsppvalue, Totalspplndlb, Meansppvalue, Meanspplndlb)
    
    # Now, get total community value and volume
    fish.dat.agg<- fish.dat %>%
      group_by(., Match) %>%
      summarize(., "CommTotalValue" = sum(Totalsppvalue, na.rm = TRUE),
                "CommTotalVolume" = sum(Totalspplndlb, na.rm = TRUE))
    
    # Join community totals to community-species-gear dataset and then calculate proportion
    fish.dat<- fish.dat %>%
      left_join(., fish.dat.agg) %>%
      mutate(., "ProportionValue" = Totalsppvalue/CommTotalValue,
             "ProportionVolume" = Totalspplndlb/CommTotalVolume)
    
    # Merge in fisheries landings data
    comm.diffs<- comm.diffs %>%
      left_join(., fish.dat, by = c("Match", "CFDERSCommonName"))
    comm.diffs
    summary(comm.diffs)
    
    # Calculate weights and then weight changes
    #comm.diffs$ProjectionValue[is.infinite(comm.diffs$ProjectionValue)]<- 500
    comm.diffs<- comm.diffs %>%
      mutate(., "ChangeWeightedValue" = ProjectionValue*ProportionValue,
             "ChangeWeightedVolume" = ProjectionValue*ProportionVolume)
    
    # Remove Skate issue...
    comm.diffs<- comm.diffs %>%
      dplyr::filter(., CFDERSCommonName != "SKATE,LITTLE/WINTER")
    
    # Write this out and save it
    write_csv(comm.diffs, paste(out.path, "SpeciesCommunityGARWeightedChanges.csv", sep= ""))
    return(comm.diffs)
  }

  # End function
}

community_weighted_changes<- function(sdm.landings.file, projection.scenario = c("Raw", "Percent"), out.path = proc.summ.path, plot.path = fig.path, plot = "Community"){
  ## Details
  # This function calculates community level summaries of projected distribution changes weighted by value or volume importance and then plots them across the shelf
  
  # Args:
  # sdm.landings.file = Path to file created by sdm_landings_merged, which should have the necessary information. One of SpeciesFocalCommunityGearTypeCFDERSWeightedChanges.csv, SpeciesCommunityCFDERSWeightedChanges.csv, or SpeciesCommunityGARWeightedChanges.csv
  # projection.scenario = Character string indicating which projection scenario data to plot. If "raw" , then the function uses Future_mean_diff.combo.b, Future_warm_diff.combo.b, or Future_cold_diff.combo.b. If "percent" then function uses Future_mean_percdiff.combo.b, Future_warm_percdiff.combo.b, or Future_cold_percdiff.combo.b.
  # out.path = Path to save output, which will be community aggregated changes, and associated map
  # plot = One of "Community" to plot each community's value OR "County" to plot county averages.

  # Returns: NULL; simply saves files in out.path
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse", "raster", "rgeos", "ggplot2", "viridis", "cowplot", "sp", "tigris", "ggthemes"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    sdm.landings.file<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/SpeciesCommunityCFDERSWeightedChanges.csv"
    projection.scenario<- "Percent"
    out.path<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/"
    plot.path<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Figures and Tables/"
    plot<- "Community"
  }
  
  # First, determine what file is being used for weights CFDERS "FocalComm.Spp.Gear" summary file or "Comm.Spp" summary file or GAR file
  file.use<- basename(sdm.landings.file)
  
  # Next determine what projection scnearios
  scenarios.use<- switch(projection.scenario,
                         "Raw" = c("Future_cold_diff.combo.b", "Future_mean_diff.combo.b", "Future_warm_diff.combo.b"),
                         "Percent" = c("Future_cold_percdiff.combo.b", "Future_mean_percdiff.combo.b", "Future_warm_percdiff.combo.b"))
  
  # Factor levels for reordering
  scenarios.levels<- switch(projection.scenario,
                            "Raw" = factor(scenarios.use, levels = c("Future_cold_diff.combo.b", "Future_mean_diff.combo.b", "Future_warm_diff.combo.b")),
                            "Percent" = factor(scenarios.use, levels = c("Future_cold_percdiff.combo.b", "Future_mean_percdiff.combo.b", "Future_warm_percdiff.combo.b")))
  
  # Focal Communities-Gear Types
  if(file.use == "SpeciesFocalCommunityGearTypeCFDERSWeightedChanges.csv"){
    # Read in sdm.landings data, filter to projection scenario, then summarize to get community-gear level changes, across species)
    sdm.land.dat<- read_csv(sdm.landings.file) %>%
      dplyr::select(., -X1) %>%
      filter(., ProjectionScenario %in% scenarios.use) %>%
      filter(., Footprint == "Regular") %>%
      group_by(Community, Long, Lat, Gear, ProjectionScenario) %>%
      summarize(., "TotalChangeValue" = sum(ChangeWeightedValue, na.rm = TRUE),
                       "TotalChangeVolume" = sum(ChangeWeightedVolume, na.rm = TRUE))
    
    # Order factor levels
    sdm.land.dat$ProjectionScenario<- factor(sdm.land.dat$ProjectionScenario, levels = scenarios.levels, labels = c("Cold climate scenario", "Average climate scenario", "Warm climate scenario"))
    
    # Save the file
    write_csv(sdm.land.dat, paste(out.path, "FocalCommunityGearAggregatedCFDERS", projection.scenario, "WeightedChanges.csv", sep = ""))
  }
  
  # Communities with CFDERS landings data
  if(file.use == "SpeciesCommunityCFDERSWeightedChanges.csv"){
    # Read in sdm.landings data, filter to projection scenario, then summarize to get community-gear level changes, across species)
    sdm.land.dat<- read_csv(sdm.landings.file) %>%
      filter(., ProjectionScenario %in% scenarios.use) %>%
      filter(., Footprint == "Regular") 
    
    # Dealing with infinit increases
    if(projection.scenario == "Percent"){
      sdm.land.dat$ChangeWeightedValue[is.infinite(sdm.land.dat$ChangeWeightedValue)]<-NA
      sdm.land.dat$ChangeWeightedVolume[is.infinite(sdm.land.dat$ChangeWeightedVolume)]<-NA
    }
    
    sdm.land.dat<- sdm.land.dat %>%
      group_by(Community, Long, Lat, ProjectionScenario) %>%
      summarize(., "TotalChangeValue" = sum(ChangeWeightedValue, na.rm = TRUE),
                "TotalChangeVolume" = sum(ChangeWeightedVolume, na.rm = TRUE))
    
    # Order factor levels
    sdm.land.dat$ProjectionScenario<- factor(sdm.land.dat$ProjectionScenario, levels = scenarios.levels, labels = c("Warm climate scenario", "Warmer climate scenario", "Warmest climate scenario"))

    # Save the file
    write_csv(sdm.land.dat, paste(out.path, "CommunityAggregatedCFDERS", projection.scenario, "WeightedChanges.csv", sep = ""))
  }
  
  # Communities with GAR data
  if(file.use == "SpeciesCommunityGARWeightedChanges.csv"){
    # Read in sdm.landings data, filter to projection scenario, then summarize to get community-gear level changes, across species)
    sdm.land.dat<- read_csv(sdm.landings.file, 
                            col_types = cols(Totalsppvalue = col_double(),
                                             Totalspplndlb = col_double(),
                                             Meansppvalue = col_double(),
                                             Meanspplndlb = col_double(),
                                             CommTotalValue = col_double(),
                                             CommTotalVolume = col_double(),
                                             ProportionValue = col_double(), 
                                             ProportionVolume = col_double(),
                                             ChangeWeightedValue = col_double(),
                                             ChangeWeightedVolume = col_double())) %>%
      filter(., ProjectionScenario %in% scenarios.use) %>%
      filter(., Footprint == "Regular") %>%
      group_by(Community, Long, Lat, ProjectionScenario) %>%
      summarize(., "TotalChangeValue" = sum(ChangeWeightedValue, na.rm = TRUE),
                "TotalChangeVolume" = sum(ChangeWeightedVolume, na.rm = TRUE))
    
    # Order factor levels
    sdm.land.dat$ProjectionScenario<- factor(sdm.land.dat$ProjectionScenario, levels = scenarios.levels, labels = c("Cold climate scenario", "Average climate scenario", "Warm climate scenario"))
    
    # Save the file
    write_csv(sdm.land.dat, paste(out.path, "CommunityAggregatedGAR", projection.scenario, "WeightedChanges.csv", sep = ""))
  }
  
  # Now plotting
  if(plot == "County"){
    # Spatial stuff -- gets us the states and shoreline
    # Spatial projections
    proj.wgs84<- CRS("+init=epsg:4326") #WGS84
    proj.utm<- CRS("+init=epsg:2960") #UTM 19
    
    #Bounds
    xlim.use<- c(-77, -65)
    ylim.use<- c(35.05, 45.2)
    states <- c("Maine", "New Hampshire", "Massachusetts", "Vermont", "New York", "Rhode Island", "Connecticut", "Delaware", "New Jersey", "Maryland", "Pennsylvania", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia", "West Virginia")
    provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")
    us <- raster::getData("GADM",country="USA",level=1)
    us.states <- us[us$NAME_1 %in% states,]
    canada <- raster::getData("GADM",country="CAN",level=1)
    ca.provinces <- canada[canada$NAME_1 %in% provinces,]
    us.states.f<- fortify(us.states, NAME_1)
    ca.provinces.f<- fortify(ca.provinces, NAME_1)
    
    # Can we get the sdm land dat data into a nicer format for visualization?
    counties<- counties(states, cb = TRUE)
    counties<- spTransform(counties, proj.wgs84)
    counties@data$id <- rownames(counties@data)
    counties.f<- fortify(counties)
    counties.f<- counties.f %>%
      left_join(., counties@data, by = "id")
    
    # Group sdm.land dat by projection scenario, and for each, calculate the average weighted change at the community level
    sdm.land.dat.pts<- data.frame("Long" = sdm.land.dat$Long, "Lat" = sdm.land.dat$Lat)
    coordinates(sdm.land.dat.pts)<- ~Long+Lat
    proj4string(sdm.land.dat.pts)<- proj.wgs84
    
    sdm.land.dat$GEOID<- over(sdm.land.dat.pts, counties)$GEOID
   
    sdm.land.plot<- sdm.land.dat %>%
      group_by(GEOID, ProjectionScenario) %>%
      summarize(., 
                "TotalChangeValue" = mean(TotalChangeValue, na.rm = TRUE), 
                "TotalChangeVolume" = mean(TotalChangeVolume, na.rm = TRUE))
    
    sdm.land.plot<- sdm.land.plot %>%
      left_join(., counties.f)
    
    # Alright, plot time
    plot.out.value<- ggplot() +
      # Update "fill" and "color" to change map color
      geom_map(data = us.states.f, map = us.states.f,
               aes(map_id = id, group = group),
               fill = "#d9d9d9", color = "gray45", size = 0.15) +
      geom_map(data = ca.provinces.f, map = ca.provinces.f,
               aes(map_id = id, group = group),
               fill = "#d9d9d9", color = "gray45", size = 0.15) +
      geom_polygon(data = sdm.land.plot, aes(x = long, y = lat, fill = TotalChangeValue, group = group), color = "gray45") +
      # Here you'd make adjustments to the point colors...
      scale_fill_gradient2(name = "Value", low = "blue", mid = "white", high = "red") +
      ylim(ylim.use) + ylab("Lat") +
      scale_x_continuous("Long", breaks = c(-75.0, -70.0, -65.0), labels = c("-75.0", "-70.0", "-65.0"), limits = xlim.use) +
      coord_fixed(1.3) +
      theme(panel.background = element_rect(fill = "white", color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color = "black")) +
      facet_wrap(~ProjectionScenario)
    
    plot.out.volume<- ggplot() +
      # Update "fill" and "color" to change map color
      geom_map(data = us.states.f, map = us.states.f,
               aes(map_id = id, group = group),
               fill = "#d9d9d9", color = "gray45", size = 0.15) +
      geom_map(data = ca.provinces.f, map = ca.provinces.f,
               aes(map_id = id, group = group),
               fill = "#d9d9d9", color = "gray45", size = 0.15) +
      geom_polygon(data = sdm.land.plot, aes(x = long, y = lat, fill = TotalChangeValue, group = group), color = "gray45") +
      # Here you'd make adjustments to the point colors...
      scale_fill_gradient2(name = "Volume", low = "blue", mid = "white", high = "red") +
      ylim(ylim.use) + ylab("Lat") +
      scale_x_continuous("Long", breaks = c(-75.0, -70.0, -65.0), labels = c("-75.0", "-70.0", "-65.0"), limits = xlim.use) +
      coord_fixed(1.3) +
      theme(panel.background = element_rect(fill = "white", color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color = "black")) +
      facet_wrap(~ProjectionScenario)
    
    plot.out<- plot_grid(plot.out.value, plot.out.volume, nrow = 2, labels = c("Value", "Volume"))
    ggsave(paste(out.path, gsub(".csv", "", file.use), "CommunityAggregatedWeightedChanges.jpg", sep = ""), plot.out, width = 11, height = 8, units = "in")
    
  }
  
  if(plot == "Community"){
    # Spatial stuff -- gets us the states and shoreline
    state.data<- as_tibble(map_data("state")) %>% 
      rename(state_group = group, state_order = order) %>% 
      filter(region %in% c("maine", "new hampshire", "massachusetts", 
                           "vermont", "rhode island", "connecticut", "new york", "new jersey",
                           "pennsylvania", "delaware", "maryland", "district of columbia", "virginia",
                           "north carolina", "south carolina", "georgia", "florida", "west virginia"))
    
    canada <- raster::getData("GADM",country="CAN",level=1)
    provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")
    ca.provinces <- canada[canada$NAME_1 %in% provinces,]
    ca.provinces <- gSimplify(ca.provinces, tol = 0.025, topologyPreserve = TRUE)
    ca.provinces.f<- fortify(ca.provinces, NAME_1)
    
    sdm.land.plot<- sdm.land.dat %>%
      group_by(Community, Long, Lat, ProjectionScenario) %>%
      summarize(., 
                "TotalChangeValue" = mean(TotalChangeValue, na.rm = TRUE), 
                "TotalChangeVolume" = mean(TotalChangeVolume, na.rm = TRUE))
    
    # Alright, plot time
    gmri.orange <- "#EA4F12"
    gmri.light.gray <- "#E9E9E9"
    gmri.blue <- "#00608A"
    font.style <- "Arial Narrow"
    font.size <- 8
    
    # # Focal communities
    # focal.comm<- c("STONINGTON_ME", "PORTLAND_ME", "NEW BEDFORD_MA", "POINT JUDITH_RI")
    # 
    # # Subset to focal communities
    # sdm.land.plot<- sdm.land.plot %>%
    #   filter(., Community %in% focal.comm)
    
    plot.out.value<- ggplot() +
      # Update "fill" and "color" to change map color
      geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                   alpha = .15, colour = "white") + 
      geom_point(data = sdm.land.plot, aes(x = Long, y = Lat, fill = TotalChangeValue), pch = 21, size = 1.75, alpha = 0.5) +
      # Here you'd make adjustments to the point colors...
      scale_fill_gradient2(low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0) +
      xlim(c(-75.75, -66.5)) + 
      ylim(c(37, 47.5)) +
      theme_map() + 
      coord_fixed(1.3) +
      theme(legend.position = "right", text = element_text(family = font.style, size = font.size)) +
      facet_wrap(~ProjectionScenario) +
      ggtitle(paste(projection.scenario, " Value Projected Values", sep = ""))
    
    plot.out.volume<- ggplot() +
      # Update "fill" and "color" to change map color
      geom_polygon(data = state.data, aes(x=long, y=lat, group=state_group),
                   alpha = .15, colour = "white") + 
      geom_point(data = sdm.land.plot, aes(x = Long, y = Lat, fill = TotalChangeVolume), pch = 21, size = 1.75, alpha = 0.5) +
      # Here you'd make adjustments to the point colors...
      scale_fill_gradient2(low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0) +
      xlim(c(-75.75, -66.5)) + 
      ylim(c(37, 47.5)) +
      theme_map() + 
      coord_fixed(1.3) +
      theme(legend.position = "right", text = element_text(family = font.style, size = font.size)) +
      facet_wrap(~ProjectionScenario) +
      ggtitle(paste(projection.scenario, " Volume Projected Values", sep = ""))
    
    plot.out<- plot_grid(plot.out.value, plot.out.volume, nrow = 2)
    ggsave(paste(plot.path, gsub(".csv", "", file.use), "CommunityAggregated", projection.scenario, "WeightedChanges.jpg", sep = ""), plot.out, width = 11, height = 8, units = "in")
    
  }
  
  # End function
}

community_successfulmodels<- function(sdm.landings.file, mod.criteria = "AUC", mod.cut = 0.7, percent = 0.75, out.path){
  # This function calculates and plots the proportion of successfully modeled species for each port according to value and volume
  
  # Args:
  # sdm.landings.file = Path to the file that has species distribution model and landings information
  # mod.criteria = Character string signaling which model selection criteria to use (AUC only now)
  # mod.cut = Numeric AUC value to use for the cut off
  # percent = Percentage of landings to use to determine if community meets model cutoff or not
  # out.path = Path to save summary output file
  
  # Returns: Community, ProportionValue and ProportionVolume successfully modeled dataframe (also saved to out.path). Function also outputs a map of communities where we successfully model at least a certain proportion of the species. 
  
  # Preliminaries
  library_check(c("tidyverse", "raster", "rgeos", "ggplot2", "viridis", "cowplot"))
  
  # Debugging
  if(FALSE){
    sdm.landings.file<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/SpeciesCommunityCFDERSWeightedChanges.csv"
    mod.criteria<- "AUC"
    mod.cut<- 0.7
    percent<- 0.75
    out.path<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/"
  }
  
  # Determine sdm.landings file name
  file.use<- basename(sdm.landings.file)
  
  # Model fit statistics 
  mod.stats<- read_csv(paste(sdm.path, "mod.results.csv", sep = "")) %>%
    mutate(., "AUC.Check" = ifelse(AUC.SDM >= mod.cut, 1, 0))
  
  auc.check<- mod.stats %>%
    group_by(COMNAME) %>%
    summarize_at(., .vars = "AUC.Check", sum, na.rm = TRUE) %>%
    filter(., AUC.Check == 2)
  
  spp.keep<- mod.stats %>%
    filter(., COMNAME  %in% auc.check$COMNAME) 
  spp.keep<- unique(spp.keep$COMNAME)
  
  if(file.use == "SpeciesCommunityCFDERSWeightedChanges.csv"){
    # Read in sdm.landings data, filter to projection scenario)
    sdm.land.dat<- read_csv(sdm.landings.file) %>%
      dplyr::select(., -X1) %>%
      filter(., CommonName %in% spp.keep & ProjectionScenario == "Baseline.combo.b") 
    
    # Now, determine how well we did modeling species in each community based on their importance (value or volume)
    sdm.successful<- sdm.land.dat %>%
      group_by(Community, Long, Lat) %>%
      summarize(., 
                "ProportionValueSuccessfulModeled" = sum(ProportionValue, na.rm = T),
                "ProportionVolumeSuccessfulModeled" = sum(ProportionVolume, na.rm = T)) %>%
      mutate(., "SuccessfulValueCheck" = ifelse(ProportionValueSuccessfulModeled >= percent, "Yes", "No"),
             "SuccessfulVolumeCheck" = ifelse(ProportionVolumeSuccessfulModeled >= percent, "Yes", "No"))
    
    write.csv(sdm.successful, file = paste(out.path, "CommunitySuccessfulModels_03152019.csv", sep= ""))
    sdm.successful$SuccessfulValueCheck<- factor(sdm.successful$SuccessfulValueCheck, levels = c("Yes", "No"))
    sdm.successful$SuccessfulVolumeCheck<- factor(sdm.successful$SuccessfulVolumeCheck, levels = c("Yes", "No"))
    
    sdm.succ.l<- sdm.successful %>%
      dplyr::select(., Community, Long, Lat, SuccessfulValueCheck, SuccessfulVolumeCheck) %>%
      gather(., "Check", "Value", -Community, -Long, -Lat)
    sdm.succ.l$Value<- factor(sdm.succ.l$Value, levels = c("Yes", "No"))
    
    return(sdm.successful)
  }
  
  # Plot
  # Spatial projections
  proj.wgs84<- CRS("+init=epsg:4326") #WGS84
  proj.utm<- CRS("+init=epsg:2960") #UTM 19
  
  #Bounds
  xlim.use<- c(-77, -65)
  ylim.use<- c(35.05, 45.2)
  states <- c("Maine", "New Hampshire", "Massachusetts", "Vermont", "New York", "Rhode Island", "Connecticut", "Delaware", "New Jersey", "Maryland", "Pennsylvania", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia", "West Virgina")
  provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")
  us <- raster::getData("GADM",country="USA",level=1)
  us.states <- us[us$NAME_1 %in% states,]
  canada <- raster::getData("GADM",country="CAN",level=1)
  ca.provinces <- canada[canada$NAME_1 %in% provinces,]
  us.states.f<- fortify(us.states, NAME_1)
  ca.provinces.f<- fortify(ca.provinces, NAME_1)
  
  # Alright, plot time
  plot.out<- ggplot() +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "#d9d9d9", color = "gray45", size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "#d9d9d9", color = "gray45", size = 0.15) +
    geom_point(data = sdm.succ.l, aes(x = Long, y = Lat, fill = Value), shape = 21, size = 2.5, alpha = 0.9) +
    # Here you'd make adjustments to the point colors...
    scale_fill_manual(name = "Proportion of successful modeled species", values = c("#4daf4a", "Black")) +
    ylim(ylim.use) + ylab("Lat") +
    scale_x_continuous("Long", breaks = c(-75.0, -70.0, -65.0), labels = c("-75.0", "-70.0", "-65.0"), limits = xlim.use) +
    coord_fixed(1.3) +
    theme(panel.background = element_rect(fill = "white", color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(fill="white", color = "black")) +
    facet_wrap(~Check)
  
  ggsave(paste(out.path, "SuccessfulModels", mod.criteria, "_", mod.cut, ".jpg", sep = ""), plot.out, width = 18, height = 12, units = "in")

  # End function
}

landings_top_spp_func<- function(df, rank.var, top.n, spp.type){
  ## Details
  # This function reads in a fisheries landings dataset and calculates the top species for a given port
  
  # Args:
  # input.data = Input dataset
  # top.n = Integer for number of top species to return
  # rank.var = Character indicating which variable (column) to use in deciding top species.
  # spp.type = Character string, either spp or spp.alt, where spp.alt contains aggregated species groupings for species managed collectively (e.g., multispecies groundfish)
  
  # Returns: NA
  
  ## Start function
  # Install libraries
  library_check(c("raster", "tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    landings.file<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/Processed Summaries/Comm.Sppsummary.csv"
    rank.var<- "Meansppvalue"
    top.n<- 10
    spp.type<- "spp"
  }

  # First, determine what file is being used for landings CFDERS "FocalComm.Spp.Gear" summary file or "Comm.Spp" summary file or GAR summary file
  file.use<- basename(landings.file)
  
  # Alternative species groupings
  cfders.nemulti<- c("FLOUNDER, AM. PLAICE", "COD", "HALIBUT, ATLANTIC", "POLLOCK", "WOLFFISH,ATLANTIC", "HADDOCK", "POUT, OCEAN", "REDFISH", "HAKE, WHITE", "FLOUNDER, SAND-DAB", "FLOUNDER, WINTER", "FLOUNDER, WITCH", "FLOUNDER, YELLOWTAIL") 
  
  if(file.use == "Comm.Sppsummary.csv"){
    # Read in landings data, create new spp.alt column for alternative species groupings
    dat<- read_csv(landings.file) %>%
      mutate(., spp.alt = case_when(spp_common_name %in% cfders.nemulti ~ "N.E. Multispecies",
                                    !spp_common_name %in% cfders.nemulti ~ as.character(spp_common_name)))
    
    # Now, figure out the top species by community using rank.var and top.n
    if(spp.type == "spp.alt"){
      top.species<- dat %>% 
        dplyr::group_by(jgs, spp.alt) %>% 
        dplyr::summarize(mean.value = sum(Meansppvalue, na.rm = TRUE)) %>% 
        arrange(jgs, desc(mean.value)) %>% 
        group_by(jgs) %>% 
        top_n(top.n, mean.value) %>%  
        arrange(desc(mean.value))
    } else {
      top.species<- dat %>% 
        group_by(jgs, spp_common_name) %>% 
        dplyr::summarize(mean.value = sum(Meansppvalue, na.rm = TRUE)) %>% 
        arrange(jgs, desc(mean.value)) %>% 
        group_by(jgs) %>% 
        top_n(top.n, mean.value) %>%  
        arrange(desc(mean.value))
    }
    
    # Return top species data set
    return(top.species)
  }
  
  # End function
}


