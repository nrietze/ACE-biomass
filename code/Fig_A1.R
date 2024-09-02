# Script to plot Appendix Figure 1
# 31.05.2024
# Nils Rietze: nils.rietze@uzh.ch

library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
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

## a) artificial 10 m plots ----
min_x <- 533831.9
min_y <- 8837358

# Data frame for scale bar
rect_data_h <- data.frame(
  xmin = min_x,
  xmax = min_x + 10,
  ymin = min_y - 1, 
  ymax = min_y - 1.8,
  ytext = min_y - 1.4,
  xtext = min_x +5
)

rect_data_v <- data.frame(
  xmin = min_x + 14,
  xmax = min_x + 14.8,
  ymin = min_y + 3, 
  ymax = min_y + 3 + 10,
  ytext = min_y + 8,
  xtext = min_x + 14.4
)

pos_cent <- 25
pos1 <- 20
pos2 <- 37

p1 <- ggplot() +
  # draw square boxes
  geom_spatvector(data = buffered_points,color = "gray", fill = NA) +
  geom_spatvector(data = buffered_points[pos_cent,],color = "black", fill = NA, 
                  linewidth = 2) + 
  geom_spatvector(data = buffered_points[pos1,],color = "brown3", fill = NA, 
                  linewidth = 1) + 
  geom_spatvector(data = buffered_points[pos2,],color = "#00008B", fill = NA, 
                  linewidth = 1) + 
  geom_spatvector(data = plot_locations_shifted, color = "gray") +
  # draw points
  geom_spatvector(data = plot_locations, color = "black", 
                  size = 2) +
  geom_spatvector(data = plot_locations, color = "black", 
                  shape = 4, size = 5) +
  geom_spatvector(data = plot_locations_shifted[pos1,], color = "brown3",
                  size = 2) +
  geom_spatvector(data = plot_locations_shifted[pos2,], color = "#00008B", 
                  size = 2) +
  # horizontal scale bar
  geom_rect(data = rect_data_h, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = 'black') +
  geom_text(data = rect_data_h, aes(x = xtext, y = ytext),
            label = "10 m", fontface = "bold", color = 'white') +
  # vertical scale bar
  geom_rect(data = rect_data_v, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = 'black') +
  geom_text(data = rect_data_v, aes(x = xtext, y = ytext),
            label = "10 m", color = 'white', fontface = "bold",
            angle = 90) +
  theme_map();p1

## b) Subplot figure ----

# Function to create random squares
create_random_squares <- function(num_squares, side_length, area_extent) {
  squares <- list()
  
  for (i in 1:num_squares) {
    # Random center within the area
    x_center <- runif(1, min = area_extent$xmin, max = area_extent$xmax)
    y_center <- runif(1, min = area_extent$ymin, max = area_extent$ymax)
    
    # Random angle
    angle <- runif(1, min = 0, max = 2 * pi)
    
    # Define vertices of the square
    half_side <- side_length / 2
    corners <- matrix(c(
      -half_side, -half_side,
      half_side, -half_side,
      half_side,  half_side,
      -half_side,  half_side
    ), ncol = 2, byrow = TRUE)
    
    # Rotate the corners
    rotation_matrix <- matrix(c(
      cos(angle), -sin(angle),
      sin(angle),  cos(angle)
    ), ncol = 2, byrow = TRUE)
    
    rotated_corners <- t(rotation_matrix %*% t(corners))
    
    # Translate the corners to the center
    rotated_corners <- rotated_corners + c(x_center, y_center)
    
    x_values <- c(rotated_corners[1, ], rotated_corners[3, ])
    y_values <- c(rotated_corners[2, ], rotated_corners[4, ])
    
    # Create a polygon (SpatVector)
    df <- data.frame(id = i,
               x = x_values,
               y = y_values) 
    df <- rbind(df, df[1, ])
    
    sf_polygon <- st_as_sf(
      st_sfc(st_polygon(list(as.matrix(df[, c("x", "y")])))),
      crs = 32643  # Specify the CRS if needed
    )
    
    polygon <- vect(sf_polygon)
    
    squares[[i]] <- polygon
  }
  
  # Combine into a single SpatVector
  combined_squares <- do.call(rbind, squares)
  return(combined_squares)
}

# Define the extent of the area to place squares within
area_extent <- ext(buffered_points[pos_cent,])

# Create 3 random squares of side length 25 cm
set.seed(1)
random_squares <- create_random_squares(3, 0.25, area_extent)

# Data frame for scale bar
rect_data_h <- data.frame(
  xmin = min_x,
  xmax = min_x + 10,
  ymin = min_y + 1, 
  ymax = min_y + 1.8,
  ytext = min_y + 1.4,
  xtext = min_x +5
)

rect_data_v <- data.frame(
  xmin = min_x + 11,
  xmax = min_x + 11.8,
  ymin = min_y + 3, 
  ymax = min_y + 3 + 10,
  ytext = min_y + 8,
  xtext = min_x + 11.4
)

p2 <- ggplot() +
  # draw square boxes
  geom_spatvector(data = buffered_points[pos_cent,],color = "black", fill = NA, 
                  linewidth = 2) +  
  geom_spatvector(data = random_squares,color = "blue", fill = NA, 
                  linewidth = 1) +
  # draw points
  geom_spatvector(data = plot_locations, color = "black", 
                  size = 2) +
  geom_spatvector(data = plot_locations, color = "black", 
                  shape = 4, size = 5) +
  # horizontal scale bar
  geom_rect(data = rect_data_h, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
          fill = 'black') +
  geom_text(data = rect_data_h, aes(x = xtext, y = ytext),
            label = "10 m", fontface = "bold", color = 'white') +
  # vertical scale bar
  geom_rect(data = rect_data_v, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = 'black') +
  geom_text(data = rect_data_v, aes(x = xtext, y = ytext),
            label = "10 m", color = 'white', fontface = "bold",
            angle = 90) +
  theme_map();p2


## c) Create final figure ----
pg <- cowplot::plot_grid(p1,p2, nrow = 2,
                         labels = c("a)","b)") # add subplot labels
                         )

# export PNG
ggsave(pg,filename = paste0(FIG_PATH,"Fig_A1.png"),
       bg = 'white',width = 6, height = 12)
