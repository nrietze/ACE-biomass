library(dplyr)
library(cowplot)
library(tidyverse)
library(motif)
library(tidyterra)
library(class)
library(caret)
library(terra)

# 1. LOAD DATA ----

path_data <- "./data/"

# c('OctRev','Vize','Pioneer','Uedineniya')
site <- 'Vize' 

# Load class rasters
rast_fn <- paste0(path_data,'raster/cropped_',site,'_standard.tif')
rast_class <- rast(rast_fn)

cat("\nThis raster has a spatial resolution of ",res(rast_class), " m \n")

# Load plot coordinates
plot_fn <- paste0(path_data,'tables/biomass_cover_reformated_drone_sites_only.csv')
plot_data <- read.csv(plot_fn) %>% 
  mutate(.,ID = 1:nrow(.)) 

# Convert to Spatvector
plot_locations <- plot_data%>% 
  vect(geom=c("Long", "Lat"),crs = 'EPSG:4326') %>% 
  project('EPSG:32643') 
  
# Plot data
plot(rast_class)
plot(plot_locations,add = T)


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
        sp[[i]] <- shift(plot_locations,dx = dx, dy = dy)
        i <- i+1
      }
      
    }
    return( vect(sp) )
  }
  
  plot_locations_shifted <-  shift_points(plot_locations,dxs,dys)
  
  # Buffer shifted plot locations
  buffered_points <- buffer(plot_locations_shifted, width = buffer_radius,
                            capstyle = 'square')
  
} else{
  # Buffer original plot locations only
  buffered_points <- buffer(plot_locations, width = buffer_radius,
                            capstyle = 'square')
}

# Function to compute fcover
get_composition <- function(rast_class,window){
  
  # Check if window is a number or simple feature
  if (is.numeric(window) ){
    # Compute number of raster cells for a window of size "window_size"
    ncells <- ceiling(window/res(rast_class)[1])
    
    # Compute landscape composition in moving windows
    lc_composition <- lsp_signature(rast_class,
                                    type = "composition", 
                                    window = ncells)
    
    lc_composition <- lsp_restructure(lc_composition) %>% 
      mutate(fcover_substrate = X1 * 100,
             fcover_vegetation = X2 * 100) %>% 
      select(-c("X1","X2"))
    
    # Plot aggregated cover
    lc_composition$signature <- NULL
    rast_composition <- lsp_add_terra(lc_composition)
    
    return(rast_composition)
    
  } else {
    # Get plots that are in the raster's extent
    intersect <- intersect(window,ext(rast_class)) %>% 
      filter(Plot != "") %>% 
      mutate(window_id = 1:nrow(.)) 
    
    intersect_unique <- intersect %>% 
      filter(grepl("*-B1", Subplot )) %>% 
      mutate(filtered_id = 1:nrow(.))
    
    # copy as dataframe
    intersect_df <- as.data.frame(intersect)
    
    # Compute landscape composition in windows around points
    fcover_fun <- function(x) {
      table_result <- table(x, useNA = "ifany")
      fraction_result <- table_result / sum(table_result, na.rm = TRUE)
      return(fraction_result)
    }
    
    lc_composition <- terra::extract(rast_class,
                                     intersect_unique['filtered_id'],
                                     fun = fcover_fun) %>% 
      unnest_wider(col = 2) 
    colnames(lc_composition) <- c("filtered_id", "fcover_substrate", "fcover_vegetation","prop_NA")
    
    # Convert fractions to percent, add ID for full join later
    lc_composition_unique <- lc_composition %>% 
      mutate(across(c(fcover_substrate, fcover_vegetation,prop_NA), ~ . * 100)) %>% 
      right_join(as.data.frame(intersect_unique),.,'filtered_id') 
      
    lc_composition_full <- left_join(intersect_df,lc_composition_unique) %>% 
      fill(c(fcover_substrate,fcover_vegetation,prop_NA),.direction = "down") %>% 
      tibble()
    
    return(lc_composition_full)
  }
}

# 3. COMPUTE FCOVER OVER BUFFERED PLOT LOCATIONS ----

# Get Fcovers in buffered points
df_composition_points <- get_composition(rast_class,buffered_points)

ggplot(data = df_composition_points,aes(x = Cover_live, y = fcover_vegetation,color = Subplot)) + 
  geom_point()

ggplot(data = df_composition_points,aes(x = Cover_live, y = fcover_vegetation,color = Subplot)) + 
  geom_line() +
  stat_summary(fun = median,fun.max = max,fun.min = min, geom = "point", size = 3) + 
  xlim (0,100) + 
  ylim(0,100) +
  xlab('FCover observed') +
  ylab('FCover drone') +
  theme(aspect.ratio=1) + 
  theme_cowplot()
  
# Plot errors with mean standard error
ggplot(data = df_composition_points) +
  geom_pointrange(mapping = aes(x = Cover_live, y = fcover_vegetation,color = Subplot),
                  stat = "summary",
                  fun.ymin = min,
                  fun.ymax = max,
                  fun.y = median)

# Validate join on Vize by looking at plots
plot(rast_class,xlim = c(533700,533850),ylim = c(8837300,8837400),
     plg=list(size=0.01, cex=.01))
plot(intersect[seq(0, 12, by=3)],'window_id',
     add=T,
     col = NA,border = intersect[seq(0, 12, by=3)]$window_id,
     legend = T)


# 4. COMPUTE LANDSCAPE-WIDE FCOVER  ----

# Define window size (in metres)
window_size <- 10 

# Get Fcovers in fixed window size
rast_composition <- get_composition(rast_class,window_size)

# Plot result
plot(rast_composition$fcover_substrate,
     main = 'Fraction of substrate cover')
plot(plot_locations,add = T)

plot(rast_composition$fcover_vegetation,
     main = 'Fraction of vegetation cover')
plot(plot_locations,add = T)


# 5. COMPUTE LANDSCAPE-WIDE FCOVER USING DIFFERENT RESOLUTIONS ----

# Apply over different window sizes
windows <- seq(1:10)
rast_list <- lapply(windows, get_composition, rast_class = rast_class)

# Extract values at buffered plot locations from all rasters within a buffer of side length 2 * radius
raster_vals_list <- lapply(rast_list, terra::extract, y = buffered_points)

# Join and rename fcover_vegetation column
for (i in seq_along(raster_vals_list)) {
  col_name <- paste0("fcover_vegetation_", i)
  
  cat(col_name,'\n')
  
  plot_data <- left_join(plot_data, select(raster_vals_list[[i]], ID, fcover_vegetation = fcover_vegetation) %>%
                             rename(!!col_name := fcover_vegetation), by = "ID")
}

# Reshape for plotting
obs_cover_name <- "Cover_Green"
obs_biomass_name <- "Cover_Total_biomass"

selected_columns <- c("ID",
                      grep("fcover_vegetation_", names(plot_data), value = TRUE),
                      obs_cover_name,obs_biomass_name)
selected_df <- select(plot_data, all_of(selected_columns)) %>% 
  na.omit() %>% 
  pivot_longer(
    cols = starts_with("fcover_vegetation_") | c(obs_cover_name),
    names_to = 'Variable',
    values_to = 'Cover'
  ) 

# Plot drone cover from different windows vs. biomass (incl. observed cover)
ggplot(selected_df) +
  geom_point(aes(x = Cover,y = Cover_Total_biomass,color = Variable)) +
  labs(title = "Scatter plot of fcover_vegetation against Cover_Total_biomass")


