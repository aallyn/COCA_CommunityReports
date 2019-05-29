#####
## Treemap functions
#####
# CFDERS Top Species Function ---------------------------------------------
cfders_top_spp_func<- function(input.data, top.n, port.name) {
  # Deatils: This function calculates the top species for CFDERS data based on the number specified in top.n.

  # Args:
    # input.data = input CFDERS dataset
    # top_n = top 'x' species
    # port.name = Focal port name
   
  # Returns: Data frame of top species landings information
  
  ## Start function
  # Libraries
  library_check(c("tidyverse"))
  
  # For debugging
  if(FALSE){
    input.data = cfders.by.spp
    top.n = 6
    port.name = jgs.ports[2]
  }
  
  # Read in dataset and then calculate the sum (sum of Mean??) landed value over the 2011-2015 baseline
  total.landings<- input.data %>%
    filter(., jgs == port.name) %>%
    # Group by alternative species grouping, with groundfish all together, then calculate the communty-species group total mean spp value
    group_by(., jgs, spp.alt) %>%
    summarise(TotalMeanValue = sum(Meansppvalue)) 
  
  # Get data for only the top.n species
  top.spp.landings<- total.landings %>%
    top_n(., top.n, TotalMeanValue) %>%
    arrange(desc(TotalMeanValue))
  
  # Now, bring back over some of the other info contained in the cfders.by.spp dataset and nice names
  out<- top.spp.landings %>%
    left_join(., input.data, by = c("jgs" = "jgs", "spp.alt" = "spp.alt")) %>%
    left_join(., cfders.nice.names, by = c("jgs" = "jgs", "spp.alt" = "spp.top")) %>%
    mutate(., port.tidy = gsub(" ", "", jgs))
  return(out)
}

# Econ Modeling Top Species Function ---------------------------------------------
econ_top_spp_func<- function(input.data, top.n, port.name) {
  # Deatils: This function calculates the top species for Economics model
  
  # Args:
    # input.data = input CFDERS dataset
    # top_n = top 'x' species
    # port.name = Focal port name
  
  # Returns: A nested dataframe, where for each scenario, there is a column "TopSpeciesData" containing the information for the top species given the specific scenario. 
  ## Start function
  # Libraries
  library_check(c("tidyverse"))
  
  # For debugging
  if(FALSE){
    input.data = econ.results.new
    top.n = 10
    port.name = jgs.ports[1]
  }
  
  # Read in dataset and then calculate the sum (sum of land_Value) landed value for each scenario across all gear types. For total landings, we need to do the baseline and calibration test DIFFERENTLY as these cannot include the extended footprint fishing landings.
  total.landings.basecalib<- input.data %>%
    filter(., jgs == port.name & footprint == "base") %>%
    filter(., scenario == "baseline" | scenario == "calibration_test" ) %>%
    # Group by alternative species grouping, with groundfish all together, then calculate the communty-species group total mean spp value
    group_by(., jgs, scenario, spp.alt) %>%
    summarise(TotalLandedValue = sum(land_value)) 
  
  total.landings.other<- input.data %>%
    filter(., jgs == port.name & scenario != "baseline" & scenario != "calibration_test") %>%
    # Group by alternative species grouping, with groundfish all together, then calculate the communty-species group total mean spp value
    group_by(., jgs, scenario, spp.alt) %>%
    summarise(TotalLandedValue = sum(land_value)) 
  
  total.landings<- bind_rows(total.landings.basecalib, total.landings.other)
  
  # Get data for only the top.n species and return it in nested data frame (by scenario)
  out<- total.landings %>%
    group_by(., jgs, scenario) %>%
    top_n(., top.n, TotalLandedValue) %>%
    left_join(., cfders.nice.names, by = c("jgs" = "jgs", "spp.alt" = "spp.top")) %>%
    group_by(., jgs, scenario) %>%
    nest(., .key = "TopSpeciesData")
  
  return(out)
}

# Econ Modeling Top Species Function ---------------------------------------------
econ_top_spp_gear_func<- function(input.data, top.n, port.name) {
  # Deatils: This function calculates the top species for Economics model
  
  # Args:
  # input.data = input CFDERS dataset
  # top_n = top 'x' species
  # port.name = Focal port name
  
  # Returns: A nested dataframe, where for each scenario, there is a column "TopSpeciesData" containing the information for the top species given the specific scenario. 
  ## Start function
  # Libraries
  library_check(c("tidyverse"))
  
  # For debugging
  if(FALSE){
    input.data = econ.results
    top.n = 10
    port.name = jgs.ports[1]
  }
  
  # Read in dataset and then calculate the sum (sum of land_Value) landed value for each scenario across all gear types
  total.landings<- input.data %>%
    filter(., jgs == port.name) %>%
    # Group by alternative species grouping, with groundfish all together, then calculate the communty-species group total mean spp value
    group_by(., jgs, scenario, footprint, gear, spp.alt) %>%
    summarise(TotalLandedValue = sum(land_value)) 
  
  # Get data for only the top.n species and return it in nested data frame (by footprint, gear, scenario)
  out<- total.landings %>%
    group_by(., jgs, scenario, footprint, gear) %>%
    top_n(., top.n, TotalLandedValue) %>%
    left_join(., cfders.nice.names, by = c("jgs" = "jgs", "spp.alt" = "spp.top")) %>%
    group_by(., jgs, scenario, footprint, gear) %>%
    nest(., .key = "TopSpeciesData")
  
  return(out)
}

# Input missing species info for SDM and Econ results ---------------------
sdm_fill_func<-  function(model.dat, top.spp.dat, port.name, scenario.name){
  # Deatils: This function joins sdm model output data with full CFDERS landing dataset and arranges it as a suitable input dataset for treemaps. For aggregated categories (e.g., N.E. Multispecies and "Other"), aggregated valyes are calcualted by summing average values and averaging across species using weighted mean by average value. (Needs better explanation)
  
  # Args:
  # model.dat = species distribution model results 
  # top.spp.dat = Top species data (independent of sdm results)
  # port.name = the port of interest  
  # scenario.name = climate scenario one would want to choose  
  
  # Returns
  
  ## Start function
  # Libraries
  library_check(c("tidyverse", "treemapify"))
  
  # For debugging
  if(FALSE){
    model.dat<- cfders.spp.impact
    top.spp.dat<- sdm.top
    port.name<- jgs.ports[1]
    scenario.name<-  "Future_mean_percdiff.combo.b"
  }
  
  # Reduce data set selecting by port of interest and selecting only variable of interest and recording that the species was modeled
  input.dat.sub<- model.dat %>% 
    filter(Community == port.name & ProjectionScenario == scenario.name) %>%
    drop_na(CFDERSCommonName) %>% 
    dplyr::select(Community, CFDERSCommonName, ProjectionScenario, ProjectionValue) %>% 
    mutate(projection.status = "yes") # was i modeled or not
  
  # Join in landings data
  temp.input<- left_join(cfders.by.spp, input.dat.sub, by = c("jgs" = "Community", "spp_common_name" = "CFDERSCommonName")) %>% 
    arrange(ProjectionScenario, spp_common_name) %>% 
    filter(jgs == port.name)
  
  # Now, need to add in information for species that we were unable to model
  # Filtering dataframe to 'missing species'
  missing.spp <- temp.input %>% filter(is.na(ProjectionValue))
  no.projection.spp <- unique(missing.spp$spp_common_name)
  
  # Defining distinct scenarios -- not clear on this
  distinct.scenarios <- temp.input %>% 
    distinct(ProjectionScenario) %>% 
    drop_na() 
  projection.scenarios <- unique(distinct.scenarios$ProjectionScenario)
  
  # Expanded grid of scenarios and gears
  null.dat<- expand.grid(no.projection.spp, projection.scenarios)
  colnames(null.dat)<- c("CFDERSCommonName", "ProjectionScenario")
  null.dat$ProjectionValue<- 0
  null.dat$projection.status<-  "no"
  null.dat$Community<- port.name
  null.dat<- null.dat %>%
    dplyr::select(., Community, CFDERSCommonName, ProjectionScenario, ProjectionValue, projection.status)

  # Now bind the dataframe of missing data w/ the one of modeled species
  df.full<- bind_rows(input.dat.sub, null.dat)
  
  # Join landings data with the impact dataset
  cfders.by.spp.sub<- cfders.by.spp %>%
    filter(., jgs == port.name)
  
  organizing.with.missing.data<- left_join(cfders.by.spp.sub, df.full, by = c("jgs" = "Community", "spp_common_name" = "CFDERSCommonName")) %>% 
    group_by(jgs, spp.alt, ProjectionScenario) %>% 
    summarise(
      mean.value = sum(Meansppvalue),
      mean.projection.value = weighted.mean(ProjectionValue, Meansppvalue)) 
  
  # Inputting display nanmes
  nice.names<- cfders.nice.names %>% 
    filter(jgs == port.name)
  
  # Independent from fill, get top species from sdm.top
  temp.top.spp.list<- unique(sdm.top$spp.alt)
  
  # Using top species, get the fill and landings information, creating the dataset that will eventually be used in the treemap function
  out<- organizing.with.missing.data %>% 
    ungroup() %>% 
    mutate(., spp.group = ifelse(spp.alt %in% c(temp.top.spp.list), as.character(spp.alt), "Other")) %>% 
    group_by(spp.group, ProjectionScenario) %>% 
    summarise(
      mean.value = sum(mean.value),
      mean.projection.value = weighted.mean(mean.projection.value)) %>% 
    ungroup() %>% 
    mutate(., mean.projection.value = ifelse(spp.group %in% c(no.projection.spp), NA, as.numeric(mean.projection.value))) %>% 
    left_join(., nice.names, by = c("spp.group" = "spp.top"))
}

# Treemap function: Value only, no independent fill variable --------------
treemap_value_plot <- function(data){
  # Deatils: This function joins model output data (either from the sdm or from the econ results) with full CFDERS landing dataset and arranges it as a suitable input dataset for treemaps. For aggregated categories (e.g., N.E. Multispecies and "Other"), aggregated valyes are calcualted by summing average values and averaging across species using weighted mean by average value. (Needs better explanation)
  
  # Args:
  # input.data = dataset to use, either the species distribution model results or the econ model results
  # port.name = the port of interest  
  # scenario.name = climate scenario one would want to choose  
  # num.top = how many species should be included in the tree map (i.e. naming the top and grouping the others into other)  
  
  # Returns
  
  ## Start function
  # Libraries
  library_check(c("tidyverse", "treemapify"))
  
  # For debugging
  if(FALSE){
    input.data<- cfders.spp.impact
    port.name<- jgs.ports[4]
    scenario.name<-  "Future_mean_percdiff.combo.b"
    num.top<- 10
  }
  
  ggplot(data, aes(area =mean.value, fill = mean.value, label = spp.nice)) + 
    geom_treemap(alpha = .8, colour = "white", size = 2) +
    geom_treemap_text(colour = "black", place = "topleft", reflow = T, alpha = .6,
                      grow = FALSE, family = font.family, size = font.size, min.size = 3) +
    theme_tufte() + 
    scale_fill_gradientn(colors = pal.seq.dark, guide_legend(title = ""),
                         breaks = c(min(data$mean.value), max(data$mean.value) / 2,  max(data$mean.value)), 
                         labels = scales::dollar) +                                                            
    theme(legend.position = "top", 
          legend.key.width = unit(.5, "in"),
          legend.key.height = unit(.1, "in"),
          legend.title.align = .5,
          text = element_text(family = font.family, size = font.size),
          aspect.ratio = .5,
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(-5,-5,-5,-5))
}

# Treemap function: Value and independent fill variable -------------------
treemap_fill_plot <- function(data, type, scenarios = c("baseline", "calibration_test", "no_adaptation", "area", "gear", "species", "area+gear", "area+species", "area+gear", "area+gear+species"), spp.modeled = spp.modeled){
  # Deatils: This function takes in a dataset and then creates a treemap(s).
  
  # Args:
    # data = Data type to use, sdm.top/econ.top/econ.gear.top
    # type = Character string, one of either "sdm", "econ", or "econ.gear"
    # scenarios = Character string defining which scenarios to plot -- relevant for econ plots and comparing baseline to different adapatiation scenarios. Options are "baseline", "calibration_test", "no_adaptation", "area", "gear", "species", "area+gear", "area+species", "area+gear", "area+gear+species")
  
  # Returns
    # treemap figure
  
  ## Start function
  # Libraries
  library_check(c("tidyverse", "treemapify"))
  
  # Debugging
  if(FALSE){
    data = econ.top
    type = "econ"
    scenarios = c("baseline", "calibration_test", "no_adaptation", "area+gear+species")
    scenarios = c("area+gear+species")
    spp.modeled = spp.modeled
  }
  
  if(type == "sdm"){
    plot.out<- ggplot(data, aes(area = mean.value, fill = mean.projection.value/100, label = spp.nice)) + 
      geom_treemap(alpha = .8, colour = "white", size = 2) +
      geom_treemap_text(colour = "black", place = "topleft", reflow = T, alpha = .6,
                        grow = FALSE, family = font.family, size = font.size, min.size = 3) +
      theme_tufte() + 
      scale_fill_gradient2(low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0,
                           na.value = gmri.gray,
                           labels = scales::percent) +
      theme(legend.position = "top", 
            legend.key.width = unit(.5, "in"),
            legend.key.height = unit(.1, "in"),
            legend.title = element_blank(),
            text = element_text(family = font.family, size = font.size),
            aspect.ratio = .5,
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5))
    return(plot.out)
  }
  
  if(type == "econ" & length(scenarios) > 1){
    plot.dat<- data %>%
      filter(., as.character(scenario) %in% scenarios) %>%
      unnest() %>%
      spread(., scenario, TotalLandedValue)
    
    # Some NA values -- how do we know if this is a species we can't model vs. a species with no landed value in the baseline period?
    # Modeled species, NA landed value == 0
    spp.mods<- plot.dat %>%
      filter(., spp.alt %in% spp.modeled$spp.alt) %>%
      replace(is.na(.), 0)
    
    # Missing species, NA landed value = NA
    spp.miss<- plot.dat %>%
      filter(., !spp.alt %in% spp.modeled$spp.alt) 
    
    # Join them back and move from wide to long format
    plot.dat<- bind_rows(spp.mods, spp.miss) %>%
      gather(., scenario, TotalLandedValue, -jgs, -spp.alt, -spp.nice, -port.nice)
    
    # Now, a few different options here...first, just produce one plot for each of the scenarios, save each and also store them in a list. Each plot uses the same min/max value
    plots.scenarios<- vector("list", length(scenarios))
    names(plots.scenarios)<- scenarios
    min.val<- min(plot.dat$TotalLandedValue, na.rm = TRUE)
    max.val<- max(plot.dat$TotalLandedValue, na.rm = TRUE)
    breaks.use<- c(min.val, max.val/4, max.val/2, (max.val/2)+(max.val/4), max.val)
    labels.use<- scales::dollar(breaks.use)
    
    for(j in seq_along(scenarios)){
      # Filter to just scenario of interest
      plot.dat.temp<- plot.dat %>%
        filter(., scenario == scenarios[j])
      
      # Some NA buisiness
      plot.dat.good<- plot.dat.temp %>%
        drop_na(TotalLandedValue)
      
      plot.dat.na<- plot.dat.temp %>%
        filter(., is.na(TotalLandedValue))
      
      # Plot
      plot.temp<- ggplot(plot.dat.good) + 
        geom_treemap(aes(area = TotalLandedValue, fill = TotalLandedValue), alpha = .8, colour = "white", size = 2) +
        scale_fill_gradientn(colors = pal.seq.dark, guide_legend(title = ""), breaks = breaks.use, na.value = "white", labels = labels.use, limits = c(min.val, max.val)) +              
        geom_treemap_text(aes(area = TotalLandedValue, fill = TotalLandedValue, label = spp.nice), colour = "black", place = "topleft", reflow = T, alpha = .6, grow = FALSE, family = font.family, size = font.size, min.size = 3) +
        theme_tufte() +     
        theme(legend.position = "top", 
              legend.key.width = unit(.5, "in"),
              legend.key.height = unit(.1, "in"),
              legend.title = element_blank(),
              text = element_text(family = font.family, size = font.size),
              aspect.ratio = .5,
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-5,-5,-5,-5),
              plot.margin = unit(c(1, 0, 2, 0), "cm"))
      
      # Add labels for missing species...
      labs.x<- rep(-0.05, nrow(plot.dat.na))
      labs.y<- seq(from = -0.1, to = -0.25, length.out = 5)[nrow(plot.dat.na)]
      labs.df<- data.frame("Species" = plot.dat.na$spp.nice, "x" = labs.x, "y" = labs.y)
      
      for(k in seq_along(labs.df$Species)){
        plot.temp<- plot.temp +
          annotation_custom(grob = textGrob(label = paste(labs.df$Species[k], " not modeled", sep = ""), hjust = 0, gp = gpar(fontfamily = font.family, fontsize = font.size-1)), ymin = labs.df$y[k], ymax = labs.df$y[k], xmin = labs.df$x[k], xmax = labs.df$x[k]) 
      }
      
      plot.temp<- plot.temp +
        annotation_custom(grob = textGrob(label = paste("Total Value = ", scales::dollar(sum(plot.dat.good$TotalLandedValue)), sep = ""), hjust = 0, gp = gpar(fontfamily = font.family, fontsize = font.size-1)), ymin = 1.1, ymax = 1.1, xmin = -0.05, xmax = -0.05)

      plot.out<- ggplot_gtable(ggplot_build(plot.temp))
      plot.out$layout$clip[plot.out$layout$name == "panel"]<- "off"
      plots.scenarios[[j]]<- plot.out
      
      # Some clean up
      dev.off()
      rm(plot.temp, plot.out)
    }
    return(plots.scenarios)
  }
  
  if(type == "econ" & length(scenarios) == 1){
    plot.dat<- data %>%
      filter(., as.character(scenario) %in% scenarios) %>%
      unnest() %>%
      spread(., scenario, TotalLandedValue)
    
    # Some NA values -- how do we know if this is a species we can't model vs. a species with no landed value in the baseline period?
    # Modeled species, NA landed value == 0
    spp.mods<- plot.dat %>%
      filter(., spp.alt %in% spp.modeled$spp.alt) %>%
      replace(is.na(.), 0)
    
    # Missing species, NA landed value = NA
    spp.miss<- plot.dat %>%
      filter(., !spp.alt %in% spp.modeled$spp.alt) 
    
    # Join them back and move from wide to long format
    plot.dat<- bind_rows(spp.mods, spp.miss) %>%
      gather(., scenario, TotalLandedValue, -jgs, -spp.alt, -spp.nice, -port.nice)
    
    # Now, a few different options here...first, just produce one plot for each of the scenarios, save each and also store them in a list. Each plot uses the same min/max value
    plots.scenarios<- vector("list", length(scenarios))
    names(plots.scenarios)<- scenarios
    min.val<- min(plot.dat$TotalLandedValue, na.rm = TRUE)
    max.val<- max(plot.dat$TotalLandedValue, na.rm = TRUE)
    breaks.use<- c(min.val, max.val/4, max.val/2, (max.val/2)+(max.val/4), max.val)
    labels.use<- scales::dollar(breaks.use)
    
    # Plotting
    # Filter to just scenario of interest
    plot.dat.temp<- plot.dat %>%
      filter(., scenario == scenarios[j])
    
    # Some NA buisiness
    plot.dat.good<- plot.dat.temp %>%
      drop_na(TotalLandedValue)
    
    plot.dat.na<- plot.dat.temp %>%
      filter(., is.na(TotalLandedValue))
    
    # Plot
    plot.temp<- ggplot(plot.dat.good) + 
      geom_treemap(aes(area = TotalLandedValue, fill = TotalLandedValue), alpha = .8, colour = "white", size = 2) +
      scale_fill_gradientn(colors = pal.seq.dark, guide_legend(title = ""), breaks = breaks.use[c(1,3,5)], na.value = "white", labels = labels.use[c(1,3,5)], limits = c(min.val, max.val)) +              
      geom_treemap_text(aes(area = TotalLandedValue, fill = TotalLandedValue, label = spp.nice), colour = "black", place = "topleft", reflow = T, alpha = .6, grow = FALSE, family = font.family, size = font.size, min.size = 3) +
      theme_tufte() +     
      theme(legend.position = "top", 
            legend.key.width = unit(.5, "in"),
            legend.key.height = unit(.1, "in"),
            legend.title = element_blank(),
            text = element_text(family = font.family, size = font.size),
            aspect.ratio = .5,
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-5,-5,-5,-5),
            plot.margin = unit(c(1, 0, 2, 0), "cm"))
    
    # Add labels for missing species if necessary
    if(nrow(plot.dat.na) > 0){
      labs.x<- rep(-0.05, nrow(plot.dat.na))
      labs.y<- seq(from = -0.1, to = -0.25, length.out = 5)[nrow(plot.dat.na)]
      labs.df<- data.frame("Species" = plot.dat.na$spp.nice, "x" = labs.x, "y" = labs.y)
      
      for(k in seq_along(labs.df$Species)){
        plot.temp<- plot.temp +
          annotation_custom(grob = textGrob(label = paste(labs.df$Species[k], " not modeled", sep = ""), hjust = 0, gp = gpar(fontfamily = font.family, fontsize = font.size-1)), ymin = labs.df$y[k], ymax = labs.df$y[k], xmin = labs.df$x[k], xmax = labs.df$x[k]) 
      }
      
      plot.temp<- plot.temp +
        annotation_custom(grob = textGrob(label = paste("Total Value = ", scales::dollar(sum(plot.dat.good$TotalLandedValue)), sep = ""), hjust = 0, gp = gpar(fontfamily = font.family, fontsize = font.size-1)), ymin = labs.df$y[1], ymax = labs.df$y[1], xmin = 0.5, xmax = 0.5)
    } else {
      plot.temp<- plot.temp +
        annotation_custom(grob = textGrob(label = paste("Total Value = ", scales::dollar(sum(plot.dat.good$TotalLandedValue)), sep = ""), hjust = 0, gp = gpar(fontfamily = font.family, fontsize = font.size-1)), ymin = -0.1, ymax = -0.1, xmin = 0.5, xmax = 0.5)
    }
    
   
    
    plot.out<- ggplot_gtable(ggplot_build(plot.temp))
    plot.out$layout$clip[plot.out$layout$name == "panel"]<- "off"
    
    return(plot.out)
    
    rm(plot.temp, plot.out)
  }
  
  if(type == "econ.gear"){
    res<- vector("list", nrow(data))
    
    for(j in 1:nrow(data)){
      plot.dat<- data[j,]
      
      # Check for null
      dat.check<- is.null(plot.dat$TopSpeciesData[[1]])
      
      if(dat.check){
        res[[j]]<- plot.out
        names(res)[j]<- plot.title
        print(paste(plot.title, " row ", j, " is done!"))
        next()
      }
      
      plot.dat<- plot.dat %>%
        unnest()
      
      plot.title<- paste(unique(as.character(plot.dat$jgs)), unique(as.character(plot.dat$scenario)), unique(as.character(plot.dat$footprint)), unique(as.character(plot.dat$gear)), sep = "_")
      
      plot.out<- ggplot(plot.dat, aes(area = TotalLandedValue, fill = TotalLandedValue, label = spp.nice)) + 
        geom_treemap(alpha = .8, colour = "white", size = 2) +
        geom_treemap_text(colour = "black", place = "center", reflow = T, alpha = .6,
                          grow = FALSE, family = font.family, size = font.size, min.size = 1) +
        theme_tufte() + 
        scale_fill_gradientn(colors = pal.seq.dark, guide_legend(title = ""),
                             breaks = c(min(plot.dat$TotalLandedValue), max(plot.dat$TotalLandedValue) / 2,  max(plot.dat$TotalLandedValue)), 
                             labels = scales::dollar) +      
        theme(legend.position = "top", 
              legend.key.width = unit(.5, "in"),
              legend.key.height = unit(.1, "in"),
              legend.title = element_blank(),
              text = element_text(family = font.family, size = font.size),
              aspect.ratio = .5,
              legend.margin=margin(0,0,0,0),
              legend.box.margin=margin(-5,-5,-5,-5))
      
      res[[j]]<- plot.out
      names(res)[j]<- plot.title
      print(paste(plot.title, " row ", j, " is done!"))
    }
    return(res)
  }
}
