# Script to plot Appendix Figure 1
# 31.05.2024
# Nils Rietze: nils.rietze@uzh.ch

library(tidyverse)
library(tidyterra)
library(terra)
library(grid)
library(cowplot)
library(RColorBrewer)

# 1. Load data ----
FIG_PATH <- "./figures/"
DATA_PATH <- "./data/"

subplot_name <- "Vize-R1-B1" # random subplot only for plotting Fig A1

# Load plot coordinates
plot_fn <- paste0(DATA_PATH,"./tables/biomass_cover_reformated_drone_sites_only.csv")
plot_data <- read.csv(plot_fn) %>% 
  mutate(.,ID = 1:nrow(.)) 

# Convert to Spatvector
plot_locations <- plot_data %>% 
  filter(Subplot == subplot_name) %>% 
  vect(geom=c("Long", "Lat"),crs = "EPSG:4326") %>% 
  project("EPSG:32643") 

# 2. Run point buffering and shifting ----

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
                            capstyle = "square")
  
} else{
  # Buffer original plot locations only
  buffered_points <- buffer(plot_locations, width = buffer_radius,
                            capstyle = "square")
}
# 3. Plot figure ----

min_x <- 533832.9
min_y <- 8837356

# Data frame for scale bar
rect_data_h <- data.frame(
  xmin = min_x,
  xmax = min_x + 10,
  ymin = min_y, 
  ymax = min_y + .8,
  ytext = min_y + .4,
  xtext = min_x +5
)

rect_data_v <- data.frame(
  xmin = min_x + 13,
  xmax = min_x + 13.8,
  ymin = min_y + 3, 
  ymax = min_y + 3 + 10,
  ytext = min_y + 8,
  xtext = min_x + 13.4
)

p <- ggplot() +
  geom_spatvector(data = buffered_points,color = "gray", fill = NA) +
  geom_spatvector(data = buffered_points[20,],color = "brown3", fill = NA, 
                  linewidth = 2) + 
  geom_spatvector(data = buffered_points[30,],color = "#00008B", fill = NA, 
                  linewidth = 2) + 
  geom_spatvector(data = plot_locations_shifted, color = "gray") +
  geom_spatvector(data = plot_locations, color = "black", 
                  size = 2) +
  geom_spatvector(data = plot_locations, color = "black", 
                  shape = 4, size = 5) +
  geom_spatvector(data = plot_locations_shifted[20,], color = "brown3",
                  size = 2) +
  geom_spatvector(data = plot_locations_shifted[30,], color = "#00008B", 
                  size = 2) +
  # horizontal scale bar
  geom_rect(data = rect_data_h, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = '#00008B') +
  geom_text(data = rect_data_h, aes(x = xtext, y = ytext),
            label = "10 m", fontface = "bold", color = 'white') +
  # vertical scale bar
  geom_rect(data = rect_data_v, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = '#00008B') +
  geom_text(data = rect_data_v, aes(x = xtext, y = ytext),
            label = "10 m", color = 'white', fontface = "bold",
            angle = 90) +
  theme_map()

# export PNG
ggsave(p,filename = paste0(FIG_PATH,"Fig_A1.png"),
        bg = 'white',width = 8, height = 8)
