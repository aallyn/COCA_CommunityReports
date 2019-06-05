
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

# Load libraries
library_check(c("tidyverse", "ggmap", "akima", "sf", "cowplot", "here", "rgeos"))

# Stem directory to shared files
stem.dir<- "/Volumes/Shared/Research/COCA-conf/SDM and CFDERS Integration/"

# Read in prediction and footprints
preds.file<- paste(stem.dir, "Data/SDM Projections/SDMPredictions.rds", sep = "")
preds<- read_rds(preds.file)

foots.file<- paste(stem.dir, "Data/VTR/All VTR safe fishing footprints by community and gear type 2011-2015.grd", sep = "")
foots<- raster::stack(foots.file)

# Port locations
port.locals<- data.frame("Community" = c("POINT.JUDITH_RI", "STONINGTON_ME", "NEW.BEDFORD_MA", "PORTLAND_ME"), "Port_Tidy" = c("Point Judith, RI", "Stonington, ME", "New Bedford, MA", "Portland, ME"), "Long" = c(-71.48672, -68.66669, -70.95104, -70.26239), "Lat" = c(41.36538, 44.15619, 41.63342, 43.65878))

# Plot function -----------------------------------------------------------
comm_report_plot_func<- function(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-76, -65), ylim = c(35, 45), labels = TRUE){
  ## Details
  # This function produces sample figures to accompany the community report vulnerability projections methods text. The function reads in the model predictions, as well as the VTR footprint data and then makes a few potential maps for the community and species defined: baseline and projected, baseline and projected with gear footprints, baseline and projected with gear extents. 
  
  # Args:
  # pred.path = SDM projection results
  # foot.path = VTR fishing footprints
  # comm = Focal community to produce maps 
  # spp = Vector of species names to produce maps for
  # season = Either "SPRING", "FALL" or "both"
  # xlim = Mapping dimensions
  # ylim = Mapping dimensions
  
  ## Start function
  # Install libraries
  library_check(c("tidyverse"))
  
  # Set arguments for debugging -- this will NOT run when you call the function. Though, you can run each line inside the {} and then you will have everything you need to walk through the rest of the function.
  if(FALSE){
    preds = preds
    foots = foots
    comm = "STONINGTON_ME"
    spp = c("AMERICAN LOBSTER")
    season = "both"
    xlim = c(-76, -65) 
    ylim = c(35, 45)
    labels = TRUE
  }
  
  # Spatial stuff
  proj.wgs84<- "+init=epsg:4326" #WGS84
  proj.utm<- "+init=epsg:2960" #UTM 19
  
  states <- c("Maine", "New Hampshire", "Massachusetts", "Vermont", "New York", "Rhode Island", "Connecticut", "Delaware", "New Jersey", "Maryland", "Pennsylvania", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia", "West Virgina")
  provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")
  
  us <- raster::getData("GADM",country="USA",level=1)
  us.states <- us[us$NAME_1 %in% states,]
  us.states <- gSimplify(us.states, tol = 0.025, topologyPreserve = TRUE)
  canada <- raster::getData("GADM",country="CAN",level=1)
  ca.provinces <- canada[canada$NAME_1 %in% provinces,]
  ca.provinces <- gSimplify(ca.provinces, tol = 0.025, topologyPreserve = TRUE)
  
  us.states.f<- fortify(us.states, NAME_1)
  ca.provinces.f<- fortify(ca.provinces, NAME_1)
  
  # Figure plotting over each of the species
  for(i in seq_along(spp)){
    
    # Extract projections
    spp.use<- spp[i]
    proj.vals.base<- preds %>%
      filter(., COMNAME == spp.use, Proj.Class == "Baseline.combo.b") %>%
      unnest() %>%
      group_by(., COMNAME, Proj.Class, x, y) %>%
      summarize(., "Projection" = mean(Projection, na.rm = T))
    
    proj.vals.future<- preds %>%
      filter(., COMNAME == spp.use, Proj.Class == "Future_mean.combo.b") %>%
      unnest() %>%
      group_by(., COMNAME, Proj.Class, x, y) %>%
      summarize(., "Projection" = mean(Projection, na.rm = T))
    
    proj.diff<- preds %>%
      filter(., COMNAME == spp.use, Proj.Class == "Future_mean_diff.combo.b") %>%
      unnest() %>%
      group_by(., COMNAME, Proj.Class, x, y) %>%
      summarize(., "Projection" = mean(Projection, na.rm = T))
    
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
    data.use<- proj.vals.future
    pred.df.fut<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
    pred.df.interp<- interp(pred.df.fut[,1], pred.df.fut[,2], pred.df.fut[,3], duplicate = "mean", extrap = TRUE,
                            xo=seq(-87.99457, -57.4307, length = 115),
                            yo=seq(22.27352, 48.11657, length = 133))
    pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
    pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
    
    # Clip to nelme
    pred.df.temp.fut<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
    coords.keep<- as.data.frame(st_coordinates(pred.df.temp.fut))
    row.names(coords.keep)<- NULL
    pred.df.diff<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.fut$z)))
    names(pred.df.fut)<- c("long", "lat", "z")
    
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
    
    # Now, looping over the different footprints
    gears<- c("Pot.Trap", "Trawl", "Gillnet", "Other", "Dredge", "Gillnet", "Longline", "Purse.Seine", "All")
    foot.names<- paste(comm, ".", gears, ".", "JGS.SAFE.PROPORTION", sep = "")
    
    for(j in seq_along(gears)){
      gear.use<- foot.names[j]
      foot.use<- foots[[which(names(foots) == gear.use)]]
      
      # Break out if no data
      if(raster::nlayers(foot.use) == 0) {
        next()
      }
      
      # Reclassify
      m <- c(0, Inf, 1,  -Inf, 0, 0)
      rclmat <- matrix(m, ncol=3, byrow=TRUE)
      lay.bin<- raster::reclassify(foot.use, rclmat)
      
      # Get coordinates of footprint
      foot.pts <- raster::as.data.frame(lay.bin, xy = TRUE)
      foot.pts$layer<- ifelse(foot.pts$layer == 1, "1", NA)
      
      # Make the plots -- which points are within the footprint
      keep.preds<- raster::extract(lay.bin, pred.df.temp.diff)
      pred.df.base.tile<- pred.df.base[which(keep.preds == 1),]
      pred.df.diff.tile<- pred.df.diff[which(keep.preds == 1),]
      
      # Footprint stuff
      # foot.pt.sf<- sf::st_as_sf(pred.df.diff.tile, coords = c("long","lat"))
      # st_crs(foot.pt.sf)<- proj.wgs84
      # foot.poly.sf<- st_make_grid(foot.pt.sf)
      # 
      # foot.poly.keep<- sf::st_contains(foot.poly.sf, foot.pt.sf)
      # foot.poly.sub<- foot.poly.sf[which(lengths(foot.poly.keep) != 0)]
      # foot.poly.union<- smooth(st_union(foot.poly.sub), method = "ksmooth")
      
      # Arbitrary labeling
      pred.df.base<- pred.df.base %>%
        drop_na(z)
      base.breaks.use<- c(0, as.numeric(quantile(pred.df.base$z)))
      base.breaks.use<- base.breaks.use[-c(2,4)]
      base.labels.use<- c("Absent", "Low", "Average", "High")
      
      pred.df.fut<- pred.df.fut %>%
        drop_na(z)
      fut.breaks.use<- c(0, as.numeric(quantile(pred.df.fut$z)))
      fut.labels.use<- c("Absent", "Low", "Fair", "Average", "Moderate", "High")
      
      pred.df.diff<- pred.df.diff %>%
        drop_na(z)
      diff.max.abs<- max(abs(min(pred.df.diff$z)), abs(max(pred.df.diff$z)))
      diff.lims<- c(-1*diff.max.abs, 1*diff.max.abs)
      diff.breaks.use<- c(-1*diff.max.abs, -0.001, 0.001, 1*diff.max.abs)
      diff.labels.use<- c("Decreasing", "", "Neutral", "Increasing")
      
      # Community labels
      port.labs<- port.locals %>%
        filter(., Community == comm)
      
      port.labs.full<- switch(comm,
                              "POINT.JUDITH_RI" = c(NA, gmri.orange, NA, NA),
                              "NEW.BEDFORD_MA" = c(gmri.orange, NA, NA, NA),
                              "PORTLAND_ME" = c(NA, NA, gmri.orange, NA),
                              "STONINGTON_ME" = c(NA, NA, NA, gmri.orange))
      
      if(labels){
        plot.out.base<- ggplot() +
          # Here you'd make adjustments to the point colors...
          geom_tile(data = pred.df.base, aes(x = long, y = lat, fill = z)) +
          #scale_fill_gradient(name = "2011-2015\nRelative biomass", low = gmri.light.gray, high = "#797979", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
          #scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#ccece6", high = "#006d2c", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
          scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#dde199", high = "#868d00", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
          geom_tile(data = pred.df.diff.tile, aes(x = long, y = lat), fill = NA, color = "#464646", size = 0.25) +
          # Update "fill" and "color" to change map color
          geom_map(data = us.states.f, map = us.states.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          geom_map(data = ca.provinces.f, map = ca.provinces.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          geom_point(data = port.labs, aes(x = Long, y = Lat), size = 0.5, color = gmri.gray, show.legend = FALSE) +
          ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy), fill = "white", color = "#3a3a3a", direction = "both", hjust = 1, nudge_x = -1.25, nudge_y = 0.5, size = 6, label.size = 0.1, segment.size = .3, family = font.family) +
          coord_sf() +
          xlim(xlim) + 
          ylim(ylim) +
          xlab("") +
          ylab("") +
          theme(legend.position = c(0.65, 0.25), 
                text = element_text(family = font.family, size = font.size),
                panel.grid.major=element_line(colour="transparent"),
                axis.line.x = element_line(size = 0.3, color = gmri.gray),
                axis.line.y = element_line(size = 0.3, color = gmri.gray),
                axis.text.x = element_text(family = font.family, color = gmri.gray, size = font.size-1),
                axis.text.y = element_text(family = font.family, color = gmri.gray, size = font.size-1),
                axis.ticks = element_line(color = gmri.gray, size = 0.5))
        
        plot.out.diff<- ggplot() +
          geom_tile(data = pred.df.diff, aes(x = long, y = lat, fill = z)) +
          #scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = diff.lims) +
          scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = diff.lims) +
          geom_tile(data = pred.df.diff.tile, aes(x = long, y = lat), fill = NA, color = "#464646", size = 0.25) +
          # Update "fill" and "color" to change map color
          geom_map(data = us.states.f, map = us.states.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          geom_map(data = ca.provinces.f, map = ca.provinces.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          geom_point(data = port.labs, aes(x = Long, y = Lat), size = 0.5, color = gmri.gray, show.legend = FALSE) +
          ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy), fill = "white", color = "#3a3a3a", direction = "both", hjust = 1, nudge_x = -1.25, nudge_y = 0.5, size = 6, label.size = 0.1, segment.size = .3, family = font.family) +
          coord_sf() +
          xlim(xlim) + 
          ylim(ylim) +
          xlab("") +
          ylab("") +
          theme(legend.position = c(0.65, 0.25), 
                text = element_text(family = font.family, size = font.size),
                panel.grid.major=element_line(colour="transparent"),
                axis.line.x = element_line(size = 0.3, color = gmri.gray),
                axis.line.y = element_line(size = 0.3, color = gmri.gray),
                axis.text.x = element_text(family = font.family, color = gmri.gray, size = font.size-1),
                axis.text.y = element_text(family = font.family, color = gmri.gray, size = font.size-1),
                axis.ticks = element_line(color = gmri.gray, size = 0.5))
        
        ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste(spp.use, gear.use, "GMRI.Alabeled.png", sep = "")), plot.out.base)
        ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste(spp.use, gear.use, "GMRI.Blabeled.png", sep = "")), plot.out.diff)
        
        dev.off()
      } else {
        plot.out.base<- ggplot() +
          # Here you'd make adjustments to the point colors...
          geom_tile(data = pred.df.base, aes(x = long, y = lat, fill = z)) +
          #scale_fill_gradient(name = "2011-2015\nRelative biomass", low = gmri.light.gray, high = "#797979", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
          #scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#ccece6", high = "#006d2c", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
          scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#dde199", high = "#868d00", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
          geom_tile(data = pred.df.diff.tile, aes(x = long, y = lat), fill = NA, color = "#464646", size = 0.15) +
          # Update "fill" and "color" to change map color
          geom_map(data = us.states.f, map = us.states.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          geom_map(data = ca.provinces.f, map = ca.provinces.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          # geom_point(data = port.labs, aes(x = Long, y = Lat), size = 2, color = gmri.gray, show.legend = FALSE) +
          # ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
          #                           fill = "white",
          #                           color = "#3a3a3a",
          #                           direction = "both",
          #                           hjust = 1,
          #                           nudge_x = -1.25,
          #                           nudge_y = 0.15,
          #                           size = 2.75,
          #                           label.size = 0.1,
          #                           segment.size = .3) +
          coord_sf(datum = proj.wgs84) +
          xlim(xlim) + 
          ylim(ylim) +
          xlab("") +
          ylab("") +
          theme(legend.position = "right", 
                text = element_text(family = font.family, size = font.size),
                panel.grid.major=element_line(colour="transparent")) 
        
        plot.out.diff<- ggplot() +
          geom_tile(data = pred.df.diff, aes(x = long, y = lat, fill = z)) +
          #scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = diff.lims) +
          scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = diff.lims) +
          geom_tile(data = pred.df.diff.tile, aes(x = long, y = lat), fill = NA, color = "#464646", size = 0.15) +
          # Update "fill" and "color" to change map color
          geom_map(data = us.states.f, map = us.states.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          geom_map(data = ca.provinces.f, map = ca.provinces.f,
                   aes(map_id = id, group = group),
                   fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
          # geom_point(data = port.labs, aes(x = Long, y = Lat), size = 2, color = gmri.gray, show.legend = FALSE) +
          # ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
          #                           fill = "white",
          #                           color = "#3a3a3a",
          #                           direction = "both",
          #                           hjust = 1,
          #                           nudge_x = -1.25,
          #                           nudge_y = 0.15,
          #                           size = 2.75,
          #                           label.size = 0.1,
          #                           segment.size = .3) +
          coord_sf(datum = proj.wgs84) +
          xlim(xlim) + 
          ylim(ylim) +
          xlab("") +
          ylab("") +
          theme(legend.position = "right", 
                text = element_text(family = font.family, size = font.size),
                panel.grid.major=element_line(colour="transparent"))
        
        
        plot.out<- plot_grid(plot.out.base, plot.out.diff, nrow = 1, align = "hv")
        ggsave(paste(out.path.use, spp.use, gear.use, "GMRI.jpg", sep = ""), plot.out, width = 7, height = 5, units = "in")
        
        dev.off()
      }

      # Overview plot
      plot.out.overview<- ggplot() +
        geom_map(data = us.states.f, map = us.states.f,
                 aes(map_id = id, group = group),
                 fill = gmri.light.gray, color = "#797979", alpha = 0.4, size = 0.15) +
        geom_map(data = ca.provinces.f, map = ca.provinces.f,
                 aes(map_id = id, group = group),
                 fill = gmri.light.gray, color = "#797979", alpha = 0.4, size = 0.15) +
        geom_point(data = port.locals, aes(x = Long, y = Lat, color = gmri.orange), size = 0.5, show.legend = FALSE) +
        ggrepel::geom_text_repel(data = port.locals, aes(x=Long, y=Lat, label=Port_Tidy),
                                 direction = "both",
                                 hjust = 1,
                                 nudge_x = 3.5,
                                 nudge_y = -1.2,
                                 size = 1.2,
                                 segment.size = .1) +
      coord_sf(datum = proj.wgs84) +
        xlim(xlim) + 
        ylim(ylim) +
        xlab("") +
        ylab("") +
        theme(legend.position = "right", 
              text = element_text(family = font.family, size = 1),
              panel.grid.major=element_line(colour="transparent"),
              axis.line.x = element_line(size = 0.15, color = gmri.gray),
              axis.line.y = element_line(size = 0.15, color = gmri.gray),
              axis.text.x = element_text(family = font.family, color = gmri.gray, size = 3),
              axis.text.y = element_text(family = font.family, color = gmri.gray, size = 3),
              axis.ticks = element_line(color = gmri.gray, size = 0.1))
      
      ggsave(here(paste("Results/", jgs.ports[i], sep = ""), paste("GMRI.png", sep = "")), plot.out.overview, width = 3.5, height = 2.25, units = "in")
      
    }

  }
  
  # End function
}

# Executing plot function... ----------------------------------------------
# Community and species vector -- these would be pulled from looking at the ecology treemap figure. Also, could work to automate this by having the tree map figure save a dataframe. 
comm.use<- "STONINGTON_ME"
spp.use<- c("AMERICAN LOBSTER")
comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-76, -65), ylim = c(38, 46), labels = TRUE)

comm.use<- "PORTLAND_ME"
spp.use<- c("ATLANTIC HERRING", "AMERICAN LOBSTER", "HAGFISH")
spp.use<- c("LONGFIN SQUID")

comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-75, -66.5), ylim = c(38, 47.5))

comm.use<- "NEW.BEDFORD_MA"
spp.use<- c("SEA SCALLOP", "AMERICAN LOBSTER")
comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-74, -66.5), ylim = c(40, 47.5))

comm.use<- "POINT.JUDITH_RI"
spp.use<- c("LONGFIN SQUID")
comm_report_plot_func(preds = preds, foots = foots, comm = comm.use, spp = spp.use, season = "both", xlim = c(-76, -65), ylim = c(38, 46), labels = TRUE)






######
## MESSSSSSSSSSSSS
######

# Species plots for Kathy
comm<- "STONINGTON_ME"
preds = preds
foots = foots
season = "both"
xlim = c(-76, -65) 
ylim = c(35, 45)


# Spatial stuff
proj.wgs84<- "+init=epsg:4326" #WGS84
proj.utm<- "+init=epsg:2960" #UTM 19

states <- c("Maine", "New Hampshire", "Massachusetts", "Vermont", "New York", "Rhode Island", "Connecticut", "Delaware", "New Jersey", "Maryland", "Pennsylvania", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia", "West Virgina")
provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")

us <- raster::getData("GADM",country="USA",level=1)
us.states <- us[us$NAME_1 %in% states,]
us.states <- gSimplify(us.states, tol = 0.025, topologyPreserve = TRUE)
canada <- raster::getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
ca.provinces <- gSimplify(ca.provinces, tol = 0.025, topologyPreserve = TRUE)

us.states.f<- fortify(us.states, NAME_1)
ca.provinces.f<- fortify(ca.provinces, NAME_1)

# Figure plotting over each of the species
for(i in seq_along(unique(preds$COMNAME))){

  # Extract projections
  spp.use<- unique(preds$COMNAME)[i]
  proj.vals.base<- preds %>%
    filter(., COMNAME == spp.use, Proj.Class == "Baseline.combo.b") %>%
    unnest() %>%
    group_by(., COMNAME, Proj.Class, x, y) %>%
    summarize(., "Projection" = mean(Projection, na.rm = T))
  
  proj.vals.future<- preds %>%
    filter(., COMNAME == spp.use, Proj.Class == "Future_mean.combo.b") %>%
    unnest() %>%
    group_by(., COMNAME, Proj.Class, x, y) %>%
    summarize(., "Projection" = mean(Projection, na.rm = T))
  
  proj.diff<- preds %>%
    filter(., COMNAME == spp.use, Proj.Class == "Future_mean_diff.combo.b") %>%
    unnest() %>%
    group_by(., COMNAME, Proj.Class, x, y) %>%
    summarize(., "Projection" = mean(Projection, na.rm = T))
  
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
  
  # Future
  data.use<- proj.vals.future
  pred.df.fut<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
  pred.df.interp<- interp(pred.df.fut[,1], pred.df.fut[,2], pred.df.fut[,3], duplicate = "mean", extrap = TRUE,
                          xo=seq(-87.99457, -57.4307, length = 115),
                          yo=seq(22.27352, 48.11657, length = 133))
  pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
  pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
  
  # Clip to nelme
  pred.df.temp.fut<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
  coords.keep<- as.data.frame(st_coordinates(pred.df.temp.fut))
  row.names(coords.keep)<- NULL
  pred.df.fut<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.fut$z)))
  names(pred.df.fut)<- c("long", "lat", "z")
  
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
  
  # Arbitrary labeling
  pred.df.base<- pred.df.base %>%
    drop_na(z)
  # base.breaks.use<- c(0, as.numeric(quantile(pred.df.base$z)))
  # base.breaks.use<- base.breaks.use[-c(2,4)]
  # base.labels.use<- c("Absent", "Low", "Average", "High")
  
  pred.df.fut<- pred.df.fut %>%
    drop_na(z)
  # fut.breaks.use<- c(0, as.numeric(quantile(pred.df.fut$z)))
  # fut.labels.use<- c("Absent", "Low", "Fair", "Average", "Moderate", "High")
  
  pred.df.diff<- pred.df.diff %>%
    drop_na(z)
  # diff.max.abs<- max(abs(min(pred.df.diff$z)), abs(max(pred.df.diff$z)))
  # diff.lims<- c(-1*diff.max.abs, 1*diff.max.abs)
  # diff.breaks.use<- c(-1*diff.max.abs, -0.001, 0.001, 1*diff.max.abs)
  # diff.labels.use<- c("Decreasing", "", "Neutral", "Increasing")
  
  # Community labels
  port.labs<- port.locals %>%
    filter(., Community == gsub(" ", ".", comm))
  
  port.labs.full<- switch(comm,
                          "POINT.JUDITH_RI" = c(NA, gmri.orange, NA, NA),
                          "NEW.BEDFORD_MA" = c(gmri.orange, NA, NA, NA),
                          "PORTLAND_ME" = c(NA, NA, gmri.orange, NA),
                          "STONINGTON_ME" = c(NA, NA, NA, gmri.orange))
  
  plot.out.base<- ggplot() +
    # Here you'd make adjustments to the point colors...
    geom_tile(data = pred.df.base, aes(x = long, y = lat, fill = z)) +
    #scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#dde199", high = "#868d00", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
    scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#dde199", high = "#868d00", limits = c(0, max(max(pred.df.base$z), max(pred.df.fut$z)))) +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_point(data = port.labs, aes(x = Long, y = Lat), size = 1, color = gmri.gray, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
                              fill = "white",
                              color = "#3a3a3a",
                              direction = "both",
                              hjust = 1,
                              nudge_x = -1.25,
                              nudge_y = 2.25,
                              size = 2.5,
                              label.size = 0.1,
                              segment.size = .3) +
    coord_sf(datum = proj.wgs84) +
    xlim(xlim) + 
    ylim(ylim) +
    xlab("") +
    ylab("") +
    theme(legend.position = "right", 
          text = element_text(family = font.family, size = 6),
          panel.grid.major=element_line(colour="transparent"),
          axis.line.x = element_line(size = 0.15, color = gmri.gray),
          axis.line.y = element_line(size = 0.15, color = gmri.gray),
          axis.text.x = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.text.y = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.ticks = element_line(color = gmri.gray, size = 0.1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  plot.out.fut<- ggplot() +
    # Here you'd make adjustments to the point colors...
    geom_tile(data = pred.df.fut, aes(x = long, y = lat, fill = z)) +
    #scale_fill_gradient(name = "2055 Projected\nRelative biomass", low = "#dde199", high = "#868d00", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
    scale_fill_gradient(name = "2055 Projected\nRelative biomass", low = "#dde199", high = "#868d00", limits = c(0, max(max(pred.df.base$z), max(pred.df.fut$z)))) +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_point(data = port.labs, aes(x = Long, y = Lat), size = 1, color = gmri.gray, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
                              fill = "white",
                              color = "#3a3a3a",
                              direction = "both",
                              hjust = 1,
                              nudge_x = -1.25,
                              nudge_y = 2.25,
                              size = 2.5,
                              label.size = 0.1,
                              segment.size = .3) +
    coord_sf(datum = proj.wgs84) +
    xlim(xlim) + 
    ylim(ylim) +
    xlab("") +
    ylab("") +
    theme(legend.position = "right", 
          text = element_text(family = font.family, size = 6),
          panel.grid.major=element_line(colour="transparent"),
          axis.line.x = element_line(size = 0.15, color = gmri.gray),
          axis.line.y = element_line(size = 0.15, color = gmri.gray),
          axis.text.x = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.text.y = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.ticks = element_line(color = gmri.gray, size = 0.1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  diff.max.abs<- max(abs(min(pred.df.diff$z)), abs(max(pred.df.diff$z)))
  diff.lims<- c(-1*diff.max.abs, 1*diff.max.abs)
  
  plot.out.diff<- ggplot() +
    geom_tile(data = pred.df.diff, aes(x = long, y = lat, fill = z)) +
    #scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = diff.lims) +
    scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, limits = diff.lims) +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_point(data = port.labs, aes(x = Long, y = Lat), size = 1, color = gmri.gray, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
                              fill = "white",
                              color = "#3a3a3a",
                              direction = "both",
                              hjust = 1,
                              nudge_x = -1.25,
                              nudge_y = 2.25,
                              size = 2.5,
                              label.size = 0.1,
                              segment.size = .3) +
    coord_sf(datum = proj.wgs84) +
    xlim(xlim) + 
    ylim(ylim) +
    xlab("") +
    ylab("") +
    theme(legend.position = "right", 
          text = element_text(family = font.family, size = 6),
          panel.grid.major=element_line(colour="transparent"),
          axis.line.x = element_line(size = 0.15, color = gmri.gray),
          axis.line.y = element_line(size = 0.15, color = gmri.gray),
          axis.text.x = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.text.y = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.ticks = element_line(color = gmri.gray, size = 0.1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  
  plot.out<- plot_grid(plot.out.base, plot.out.fut, plot.out.diff, nrow = 1, align = "hv")
  ggsave(here(paste("Results/", comm, sep = ""), paste(spp.use, "Projections.pdf", sep = "")), plot.out)
}

  
########
## MESSSSSSSSS
#########

# Footprints for Kathy
comm<- "STONINGTON_ME"
foots = foots
xlim = c(-76, -65) 
ylim = c(35, 45)

# Spatial stuff
proj.wgs84<- "+init=epsg:4326" #WGS84
proj.utm<- "+init=epsg:2960" #UTM 19

states <- c("Maine", "New Hampshire", "Massachusetts", "Vermont", "New York", "Rhode Island", "Connecticut", "Delaware", "New Jersey", "Maryland", "Pennsylvania", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia", "West Virgina")
provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")

us <- raster::getData("GADM",country="USA",level=1)
us.states <- us[us$NAME_1 %in% states,]
us.states <- gSimplify(us.states, tol = 0.025, topologyPreserve = TRUE)
canada <- raster::getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
ca.provinces <- gSimplify(ca.provinces, tol = 0.025, topologyPreserve = TRUE)

us.states.f<- fortify(us.states, NAME_1)
ca.provinces.f<- fortify(ca.provinces, NAME_1)

# Figure plotting over each of the gear types
gears<- c("Pot.Trap", "Trawl", "Gillnet", "Other", "Dredge", "Gillnet", "Longline", "Purse.Seine", "All")
foot.names.reg<- paste(comm, ".", gears, ".", "JGS.SAFE.PROPORTION", sep = "")
foot.names.ext<- paste(foot.names.reg, "MaxD.JGS.SAFE.PROPORTION", sep = "")
foot.names.1.5ext<- paste(foot.names.reg, "1.5xMaxD.JGS.SAFE.PROPORTION", sep = "")
foot.names.all<- c(foot.names.reg, foot.names.ext, foot.names.1.5ext)

for (i in seq_along(foot.names.all)){
  foot.use<- foots[[which(names(foots) == foot.names.all[i])]]
  
  # Break out if no data
  if(raster::nlayers(foot.use) == 0) {
    next()
  }
  
  # Reclassify
  m <- c(0, Inf, 1,  -Inf, 0, 0)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  lay.bin<- raster::reclassify(foot.use, rclmat)
  
  # Get coordinates of footprint
  foot.pts <- raster::as.data.frame(lay.bin, xy = TRUE)
  foot.pts$layer<- ifelse(foot.pts$layer == 1, "1", NA)
  
  # Make the plots 
  # Community labels
  port.labs<- port.locals %>%
    filter(., Community == comm)
  
  port.labs.full<- switch(comm,
                          "POINT.JUDITH_RI" = c(NA, gmri.orange, NA, NA),
                          "NEW.BEDFORD_MA" = c(gmri.orange, NA, NA, NA),
                          "PORTLAND_ME" = c(NA, NA, gmri.orange, NA),
                          "STONINGTON_ME" = c(NA, NA, NA, gmri.orange))
  
  plot.out<- ggplot() +
    # Update "fill" and "color" to change map color
    geom_tile(data = foot.pts, aes(x = x, y = y, fill = layer), color = NA, size = 0.15, show.legend = FALSE) +
    scale_fill_manual(values = c(gmri.blue, ""), na.value = NA) +
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", size = 0.15) +
    geom_point(data = port.labs, aes(x = Long, y = Lat), size = 2, color = gmri.gray, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
                              fill = "white",
                              color = "#3a3a3a",
                              direction = "both",
                              hjust = 1,
                              nudge_x = -1.25,
                              nudge_y = 0.15,
                              size = 2.75,
                              label.size = 0.1,
                              segment.size = .3) +
    coord_sf(datum = proj.wgs84) +
    xlim(xlim) + 
    ylim(ylim) +
    xlab("") +
    ylab("") +
    theme(legend.position = "right", 
          text = element_text(family = font.family, size = 6),
          panel.grid.major=element_line(colour="transparent"),
          axis.line.x = element_line(size = 0.15, color = gmri.gray),
          axis.line.y = element_line(size = 0.15, color = gmri.gray),
          axis.text.x = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.text.y = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.ticks = element_line(color = gmri.gray, size = 0.1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  ggsave(here(paste("Results/", comm, sep = ""), paste(foot.names.all[i], "Footprint.png", sep = "")), plot.out)
}



########
## MESS One species
########
# Species plots for Kathy
comm<- "POINT JUDITH_RI"
preds = preds
foots = foots
season = "both"
xlim = c(-76, -65) 
ylim = c(35, 45)
spp = "SPANISH MACKEREL"


# Spatial stuff
proj.wgs84<- "+init=epsg:4326" #WGS84
proj.utm<- "+init=epsg:2960" #UTM 19

states <- c("Maine", "New Hampshire", "Massachusetts", "Vermont", "New York", "Rhode Island", "Connecticut", "Delaware", "New Jersey", "Maryland", "Pennsylvania", "Virginia", "North Carolina", "South Carolina", "Georgia", "Florida", "District of Columbia", "West Virgina")
provinces <- c("Ontario", "Québec", "Nova Scotia", "New Brunswick")

us <- raster::getData("GADM",country="USA",level=1)
us.states <- us[us$NAME_1 %in% states,]
us.states <- gSimplify(us.states, tol = 0.025, topologyPreserve = TRUE)
canada <- raster::getData("GADM",country="CAN",level=1)
ca.provinces <- canada[canada$NAME_1 %in% provinces,]
ca.provinces <- gSimplify(ca.provinces, tol = 0.025, topologyPreserve = TRUE)

us.states.f<- fortify(us.states, NAME_1)
ca.provinces.f<- fortify(ca.provinces, NAME_1)

out.path.use<- "~/Box/Mills Lab/Projects/COCA15_ClimVuln/Ecology/SDMProjectionMaps/"

# Figure plotting over each of the species
for(i in seq_along(unique(preds$COMNAME))){
  
  # Extract projections
  spp.use<- unique(preds$COMNAME)[i]
  proj.vals.base<- preds %>%
    filter(., COMNAME == spp.use, Proj.Class == "Baseline.combo.b") %>%
    unnest() %>%
    group_by(., COMNAME, Proj.Class, x, y) %>%
    summarize(., "Projection" = mean(Projection, na.rm = T))
  
  proj.vals.future<- preds %>%
    filter(., COMNAME == spp.use, Proj.Class == "Future_mean.combo.b") %>%
    unnest() %>%
    group_by(., COMNAME, Proj.Class, x, y) %>%
    summarize(., "Projection" = mean(Projection, na.rm = T))
  
  proj.diff<- preds %>%
    filter(., COMNAME == spp.use, Proj.Class == "Future_mean_diff.combo.b") %>%
    unnest() %>%
    group_by(., COMNAME, Proj.Class, x, y) %>%
    summarize(., "Projection" = mean(Projection, na.rm = T))
  
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
  
  # Future
  data.use<- proj.vals.future
  pred.df.fut<- na.omit(data.frame("x" = data.use$x, "y" = data.use$y, "layer" = data.use$Projection))
  pred.df.interp<- interp(pred.df.fut[,1], pred.df.fut[,2], pred.df.fut[,3], duplicate = "mean", extrap = TRUE,
                          xo=seq(-87.99457, -57.4307, length = 115),
                          yo=seq(22.27352, 48.11657, length = 133))
  pred.df.interp.final<- data.frame(expand.grid(x = pred.df.interp$x, y = pred.df.interp$y), z = c(round(pred.df.interp$z, 2)))
  pred.sp<- st_as_sf(pred.df.interp.final, coords = c("x", "y"), crs = proj.wgs84)
  
  # Clip to nelme
  pred.df.temp.fut<- pred.sp[which(st_intersects(pred.sp, nelme, sparse = FALSE) == TRUE),]
  coords.keep<- as.data.frame(st_coordinates(pred.df.temp.fut))
  row.names(coords.keep)<- NULL
  pred.df.fut<- data.frame(cbind(coords.keep, "z" = as.numeric(pred.df.temp.fut$z)))
  names(pred.df.fut)<- c("long", "lat", "z")
  
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
  
  # Arbitrary labeling
  pred.df.base<- pred.df.base %>%
    drop_na(z)
  # base.breaks.use<- c(0, as.numeric(quantile(pred.df.base$z)))
  # base.breaks.use<- base.breaks.use[-c(2,4)]
  # base.labels.use<- c("Absent", "Low", "Average", "High")
  
  pred.df.fut<- pred.df.fut %>%
    drop_na(z)
  # fut.breaks.use<- c(0, as.numeric(quantile(pred.df.fut$z)))
  # fut.labels.use<- c("Absent", "Low", "Fair", "Average", "Moderate", "High")
  
  pred.df.diff<- pred.df.diff %>%
    drop_na(z)
  # diff.max.abs<- max(abs(min(pred.df.diff$z)), abs(max(pred.df.diff$z)))
  # diff.lims<- c(-1*diff.max.abs, 1*diff.max.abs)
  # diff.breaks.use<- c(-1*diff.max.abs, -0.001, 0.001, 1*diff.max.abs)
  # diff.labels.use<- c("Decreasing", "", "Neutral", "Increasing")
  
  # Community labels
  port.labs<- port.locals %>%
    filter(., Community == gsub(" ", ".", comm))
  
  port.labs.full<- switch(comm,
                          "POINT.JUDITH_RI" = c(NA, gmri.orange, NA, NA),
                          "NEW.BEDFORD_MA" = c(gmri.orange, NA, NA, NA),
                          "PORTLAND_ME" = c(NA, NA, gmri.orange, NA),
                          "STONINGTON_ME" = c(NA, NA, NA, gmri.orange))
  
  plot.out.base<- ggplot() +
    # Here you'd make adjustments to the point colors...
    geom_tile(data = pred.df.base, aes(x = long, y = lat, fill = z)) +
    #scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#dde199", high = "#868d00", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
    scale_fill_gradient(name = "2011-2015\nRelative biomass", low = "#dde199", high = "#868d00", limits = c(0, max(max(pred.df.base$z), max(pred.df.fut$z)))) +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_point(data = port.labs, aes(x = Long, y = Lat), size = 1, color = gmri.gray, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
                              fill = "white",
                              color = "#3a3a3a",
                              direction = "both",
                              hjust = 1,
                              nudge_x = -1.25,
                              nudge_y = 2.25,
                              size = 2.5,
                              label.size = 0.1,
                              segment.size = .3) +
    coord_sf(datum = proj.wgs84) +
    xlim(xlim) + 
    ylim(ylim) +
    xlab("") +
    ylab("") +
    theme(legend.position = "right", 
          text = element_text(family = font.family, size = 6),
          panel.grid.major=element_line(colour="transparent"),
          axis.line.x = element_line(size = 0.15, color = gmri.gray),
          axis.line.y = element_line(size = 0.15, color = gmri.gray),
          axis.text.x = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.text.y = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.ticks = element_line(color = gmri.gray, size = 0.1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  plot.out.fut<- ggplot() +
    # Here you'd make adjustments to the point colors...
    geom_tile(data = pred.df.fut, aes(x = long, y = lat, fill = z)) +
    #scale_fill_gradient(name = "2055 Projected\nRelative biomass", low = "#dde199", high = "#868d00", breaks = base.breaks.use, labels = base.labels.use, limits = c(0, max(pred.df.base$z))) +
    scale_fill_gradient(name = "2055 Projected\nRelative biomass", low = "#dde199", high = "#868d00", limits = c(0, max(max(pred.df.base$z), max(pred.df.fut$z)))) +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_point(data = port.labs, aes(x = Long, y = Lat), size = 1, color = gmri.gray, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
                              fill = "white",
                              color = "#3a3a3a",
                              direction = "both",
                              hjust = 1,
                              nudge_x = -1.25,
                              nudge_y = 2.25,
                              size = 2.5,
                              label.size = 0.1,
                              segment.size = .3) +
    coord_sf(datum = proj.wgs84) +
    xlim(xlim) + 
    ylim(ylim) +
    xlab("") +
    ylab("") +
    theme(legend.position = "right", 
          text = element_text(family = font.family, size = 6),
          panel.grid.major=element_line(colour="transparent"),
          axis.line.x = element_line(size = 0.15, color = gmri.gray),
          axis.line.y = element_line(size = 0.15, color = gmri.gray),
          axis.text.x = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.text.y = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.ticks = element_line(color = gmri.gray, size = 0.1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  diff.max.abs<- max(abs(min(pred.df.diff$z)), abs(max(pred.df.diff$z)))
  diff.lims<- c(-1*diff.max.abs, 1*diff.max.abs)
  
  plot.out.diff<- ggplot() +
    geom_tile(data = pred.df.diff, aes(x = long, y = lat, fill = z)) +
    #scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, breaks = diff.breaks.use, labels = diff.labels.use, limits = diff.lims) +
    scale_fill_gradient2(name = "Projected\nRelative biomass changes", low = gmri.blue, mid = gmri.light.gray, high = gmri.orange, midpoint = 0, limits = diff.lims) +
    # Update "fill" and "color" to change map color
    geom_map(data = us.states.f, map = us.states.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_map(data = ca.provinces.f, map = ca.provinces.f,
             aes(map_id = id, group = group),
             fill = "white", color = "#797979", alpha = 0.4, size = 0.15) +
    geom_point(data = port.labs, aes(x = Long, y = Lat), size = 1, color = gmri.gray, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = port.labs, aes(x=Long, y=Lat, label=Port_Tidy),
                              fill = "white",
                              color = "#3a3a3a",
                              direction = "both",
                              hjust = 1,
                              nudge_x = -1.25,
                              nudge_y = 2.25,
                              size = 2.5,
                              label.size = 0.1,
                              segment.size = .3) +
    coord_sf(datum = proj.wgs84) +
    xlim(xlim) + 
    ylim(ylim) +
    xlab("") +
    ylab("") +
    theme(legend.position = "right", 
          text = element_text(family = font.family, size = 6),
          panel.grid.major=element_line(colour="transparent"),
          axis.line.x = element_line(size = 0.15, color = gmri.gray),
          axis.line.y = element_line(size = 0.15, color = gmri.gray),
          axis.text.x = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.text.y = element_text(family = font.family, color = gmri.gray, size = 5.5),
          axis.ticks = element_line(color = gmri.gray, size = 0.1),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  
  plot.out<- plot_grid(plot.out.base, plot.out.fut, plot.out.diff, nrow = 1, align = "hv")
  ggsave(paste(out.path.use, spp.use, "Projections.pdf", sep = ""), plot.out)
}



