# Script to plot Appendix Figure 3
# 01.07.2024
# Nils Rietze: nils.rietze@uzh.ch

library(dplyr)
library(cowplot)
library(tidyverse)
library(motif)
library(tidyterra)
library(class)
library(caret)
library(terra)

# 1. LOAD DATA ----
FIG_PATH <- "./figures/"
DATA_PATH <- "./data/"
# load site table
coords <- read.csv(paste0(DATA_PATH,"tables/biomass_cover_reformated_drone_sites_only.csv")) %>% 
  vect(geom = c("Long","Lat"),crs = "epsg:4326") %>% 
  terra::project("epsg:32643")

sites <- c("Vize", "Uedineniya", "Pioneer")

# function to load raster data
loadRast <- function(site, variable){
  
  # extract first 4 letters of site name
  site_prefix <- substr(site, 1, 4)
  
  # find file for site and variable
  fname <- list.files(DATA_PATH,
                      pattern = paste0(site_prefix,".*.",variable,".tif$"),
                      recursive = TRUE,
                      full.names = TRUE)
  
  #  load raster
  raster <-  rast(fname)
  
  # add site name
  varnames(raster) <- site
  
  return(raster)
}

rlist_class <- lapply(sites,loadRast,variable =  "cut")

# 2. CONFIGURE POINT BUFFERING ----

buffer_radius <- 5 # in crs units (meters)

# get focal fcover in shifted windows around the original plot location?
shifted_extract <- TRUE

# Buffer the points for the pixel extraction
if (shifted_extract){
  # Create shifted plot locations for uncertainty estimation of fcover in plots
  dxs <- -3:3
  dys <- -3:3
  
  shift_points <- function(points, dxs,dys){
    sp <- list()
    i <- 1
    
    for (dx in dxs){
      for (dy in dys){
        sp[[i]] <- shift(coords,dx = dx, dy = dy)
        i <- i+1
      }
      
    }
    return( vect(sp) )
  }
  
  coords_shifted <-  shift_points(coords,dxs,dys)
  
  # Buffer shifted plot locations
  buffered_points <- buffer(coords_shifted, width = buffer_radius,
                            capstyle = 'square')
  
} else{
  # Buffer original plot locations only
  buffered_points <- buffer(coords, width = buffer_radius,
                            capstyle = 'square')
}

# Function to compute fcover
get_composition <- function(rast_class,window){
    
  cat(sprintf("Computing focal cover in %s ... \n",varnames(rast_class)) )
  
  window_rpj <- project(window, crs(rast_class)) %>% 
    mutate(window_id = 1:nrow(.))
  
  # Get plots that are in the raster's extent
  intersect <- terra::intersect(window_rpj,ext(rast_class)) 
  
  # copy as dataframe
  intersect_df <- as.data.frame(intersect)
  
  # extract only the unique polygons for faster terra::extract
  intersect_unique <- intersect %>% 
    filter(grepl("*-B1", Subplot )) %>% 
    mutate(filtered_id = 1:nrow(.))
  
  # Compute landscape composition in windows around points
  fcover_fun <- function(x) {
    table_result <- table(x, useNA = "ifany")
    fraction_result <- table_result / sum(table_result, na.rm = TRUE)
    return(fraction_result)
  }
  
  # extract raster cover
  lc_composition <- terra::extract(rast_class,
                                   intersect_unique['filtered_id'],
                                   fun = fcover_fun) 
  
  if (varnames(rast_class) != "Vize"){
    lc_composition <- as.data.frame(lc_composition)
    lc_composition$prop_NA <- NA
  } else {
    lc_composition <- lc_composition %>% 
      unnest_wider(2) 
  }
  
  colnames(lc_composition) <- c("filtered_id", "fcover_vegetation", "fcover_substrate","prop_NA")
  
  cat("joining data ... \n")
  
  # Convert fractions to percent, and join with Plot-level df
  lc_composition_unique <- lc_composition %>% 
    mutate(across(c(fcover_substrate, fcover_vegetation,prop_NA), ~ . * 100)) %>% 
    right_join(as.data.frame(intersect_unique),.,'filtered_id') 
  
  # Join with final dataframe format and fill empty rows
  lc_composition_full <- left_join(intersect_df,lc_composition_unique) %>% 
    fill(c(fcover_substrate,fcover_vegetation,prop_NA),.direction = "down") %>% 
    mutate(across(c(fcover_substrate,fcover_vegetation,prop_NA),as.numeric)) %>% 
    tibble()
  
  cat("done. \n")
  
  return(lc_composition_full)
}

# Apply function to all sites
df_list <- lapply(rlist_class, get_composition, window = buffered_points)
# df_1 <- get_composition(rlist_class[[1]], window = buffered_points)

df_composition_points <- bind_rows(df_list)

custom_palette <- c(
   # viridis::viridis(n = 6),
   # viridis::mako(n = 3),   # for site 2
   # viridis::inferno(n = 3)   # for site 3
  RColorBrewer::brewer.pal(6,"Reds")[4:6],
  RColorBrewer::brewer.pal(6,"Blues")[4:6],
  RColorBrewer::brewer.pal(6,"Greens")
)

# Plot vegetation cover per "plot"
p <- df_composition_points %>%
  group_by(Plot) %>%
  summarise(
    Cover_live_mean = mean(Cover_live),
    fcover_vegetation_mean = mean(fcover_vegetation),
    Cover_live_min = min(Cover_live),
    Cover_live_max = max(Cover_live),
    fcover_vegetation_min = min(fcover_vegetation),
    fcover_vegetation_max = max(fcover_vegetation)
  ) %>% 
  ggplot(aes(x = Cover_live_mean, y = fcover_vegetation_mean, color = Plot)) + 
    # dashed 1:1 line
    geom_abline(intercept = 0, slope = 1, 
                linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(ymin = fcover_vegetation_min, 
                        ymax = fcover_vegetation_max),
                    size = 1) +
    geom_errorbarh(aes(xmin = Cover_live_min, 
                       xmax = Cover_live_max),
                   height = 0) +
    # scale_color_viridis_d(option = "viridis") +
    scale_color_manual(values = custom_palette) +
    xlab('Cover observed (mean in-situ subplot cover), %') +
    ylab('Fractional vegetation cover (drone estimate), %') +
    theme_cowplot(15) +
    # background_grid() +
    xlim(0, 100) + 
    ylim(0, 100) +
    theme(aspect.ratio = 1); p

# export PNG
ggsave(p,filename = paste0(FIG_PATH,"Fig_A3.png"),
        bg = 'white',width = 8, height = 8)
