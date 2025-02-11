# Script to extract Sentinel-2 data
# 11.02.2025
# Nils Rietze: nils.rietze@uzh.ch

library(terra)
library(tidyterra)
library(tidyverse)
library(cowplot)
library(patchwork)
library(ggpubr)
library(stringr)

# 0. Configure functions ----

# Function to read raster data and calculate NDVI from (downloaded from Copernicus Dataspace Browser and covnerted to TIF with GDAL)
load_data <- function(fname, bbox, index_name){
  s2_rast <- rast(fname)
  
  # Reproject bounding box to raster UTM zone (add 50 m for the extent)
  bbox_repr <- project(bbox,from = "epsg:4326", to = crs(s2_rast)) + 50
  
  # Clip raster to bounding box and scale
  s2_rast_clipped <- crop(s2_rast,bbox_repr) / 1e4
  
  # Rename bands
  names(s2_rast_clipped) <- c("RED","GREEN","BLUE","NIR")

  if (index_name == "NDVI"){
    out_rast <- (s2_rast_clipped$NIR - s2_rast_clipped$RED) / (s2_rast_clipped$NIR + s2_rast_clipped$RED)
  } else if (index_name == "SAVI"){
    L <- 0.5
    out_rast <- ((s2_rast_clipped$NIR - s2_rast_clipped$RED) / (s2_rast_clipped$NIR + s2_rast_clipped$RED + L)) * (1.0 + L)
  }
  
  names(out_rast) <- index_name
  
  return(out_rast)
}

extract_S2_metrics <- function(s2_rast, plot_points, metric){
  crs_rast <- crs(s2_rast)
  
  # Create buffer around points (15 meters)
  plots_buffered <- plot_points %>% 
    project(crs_rast) %>% 
    buffer(15)
  
  s2_metric <- zonal(s2_rast, plots_buffered, 
                     fun=metric, na.rm=FALSE)
  
  return(s2_metric)
}

# 1. Load data ----
sites <- c('OctRev','Vize','Pioneer','Uedineniya')
S2_DIR <- "data/raster/sentinel2/"

# plot locations
plot_locations <- read.csv("data/tables/biomass_cover_reformated_drone_sites_only.csv") %>% 
  vect(geom = c("Long","Lat"),crs = "epsg:4326")

# Biomass data table
data <- read.csv("data/tables/biomass_cover_reformated_drone_sites_only.csv")

# 2. Extract Sentinel-2 data for each plot ----

df_out <- data.frame()

for (site in sites){
  cat(sprintf("Processing site: %s ... \n",site))
  
  # Filter plot_locations
  plot_location_filtered <- plot_locations %>% 
    filter(grepl(site,Site))
  
  bbox_wgs84 <- ext(plot_location_filtered)
  
  # Read raster
  S2_FNAME <- paste0(S2_DIR,sprintf("%s_10m.tif",site))
  s2_ndvi <- load_data(S2_FNAME, bbox_wgs84, index_name = "NDVI")
  s2_savi <- load_data(S2_FNAME, bbox_wgs84, index_name = "SAVI")
  s2_evi <- load_data(S2_FNAME, bbox_wgs84, index_name = "EVI2")
  
  # Get zonal statistics for NDVI
  s2_mean_ndvi <- extract_S2_metrics(s2_ndvi, plot_location_filtered, metric = "mean")
  s2_sd_ndvi <- extract_S2_metrics(s2_ndvi, plot_location_filtered, metric = "sd")
  
  # And for SAVI
  s2_mean_savi <- extract_S2_metrics(s2_savi, plot_location_filtered, metric = "mean")
  s2_sd_savi <- extract_S2_metrics(s2_savi, plot_location_filtered, metric = "sd")
  
  df_temp <- data.frame(s2_mean_ndvi,s2_sd_ndvi,
                        s2_mean_savi,s2_sd_savi,
                        plot_location_filtered$Subplot)
  colnames(df_temp) <- c("s2_mean_ndvi","s2_sd_ndvi",
                         "s2_mean_savi","s2_sd_savi",
                         "Subplot")
  
  df_out <- rbind(df_out, df_temp)
}

data_out <- data %>% left_join(df_out, by = "Subplot")

write.csv(data_out,file = "data/tables/biomass_cover_reformated_drone_sites_only_newS2data.csv")

# 3. Plot Figure A5 ----
# Load data frame
data <- read.csv("data/tables/biomass_cover_reformated_drone_sites_only_newS2data.csv")
data$Site <- as.factor(data$Site)

# Load sample geodata for inset map
site <- "Uedineniya"

# Filter plot_locations
plot_location_filtered <- plot_locations %>% 
  filter(grepl(site,Site))

bbox_wgs84 <- ext(plot_location_filtered)

S2_FNAME <- paste0(S2_DIR,sprintf("%s_10m.tif",site))
s2_ndvi <- load_data(S2_FNAME, bbox_wgs84, index_name = "NDVI")

ext_rast <- ext(s2_ndvi)

plots_buffered <- plot_location_filtered %>% 
  project(s2_ndvi) %>% 
  buffer(15)

# Set up colors for boxplot fills
custom_palette <- c(
  "Pioneer" = RColorBrewer::brewer.pal(6,"Reds")[4],
  "Uedineniya" = RColorBrewer::brewer.pal(6,"Blues")[4],
  "OctRevCentre" = RColorBrewer::brewer.pal(6,"Purples")[4],
  "Vize" = RColorBrewer::brewer.pal(6,"Greens")[4]
)

site_proper_names <- names(custom_palette)
site_proper_names[3] <- "October Revolution Inland"

s2_dates <- c("Pioneer" = "23-08-2021",
              "Uedineniya" = "08-09-2021",
              "October Revolution Inland" = "25-08-2021",
              "Vize" = "04-08-2022")
xlabs <- paste(site_proper_names,s2_dates,sep = "\n")
xlabs <- str_wrap(xlabs, width = 10)
names(xlabs) <- names(custom_palette)

FONT_SIZE <- 18

#  Boxplot of mean Sentinel-2 NDVI
p1 <- ggplot(data) + 
  geom_boxplot(aes(x = Site, y = s2_mean_ndvi, 
                   fill = Site), alpha = 0.8) +
  geom_point(aes(x = Site,y = s2_mean_ndvi),
             size = 2, alpha = 1) +
  scale_fill_manual(values = custom_palette) +
  scale_x_discrete(labels = xlabs) +
  labs(y = "Sentinel-2 mean NDVI",
       x = "") +
  ylim(c(0.05,0.2)) + 
  theme_cowplot(FONT_SIZE) +
  theme(legend.position = "none");p1

inset <- ggplot() + 
  geom_spatraster(data = s2_ndvi, show.legend = F) +
  geom_spatvector(data = plots_buffered, 
                  fill = NA, color = "white", linewidth = 1) +
  # add scale bar
  geom_rect(aes(xmin = ext_rast[1] + 30, xmax = ext_rast[1] + 60, 
                ymin = ext_rast[3] + 5, ymax = ext_rast[3] + 10),
            fill = 'white') + 
  geom_text(aes(x = ext_rast[1] + 45, y = ext_rast[3] + 20,
                label = '30 m',fontface = 'bold'),
            size = 4,
            colour = 'white') +
  scale_fill_viridis_c(option = "mako") +
  labs(fill = "Sentinel-2 NDVI") +
  theme_map(FONT_SIZE) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1));inset

# Combine figures
pa <- ggdraw(p1) + draw_plot(inset, .15, .6, .4, .4)

# Boxplot of std.dev Sentinel-2 NDVI
p2 <- ggplot(data) + 
  geom_boxplot(aes(x = Site, y = s2_sd_ndvi, 
                   fill = Site), alpha = 0.8) +
  geom_point(aes(x = Site,y = s2_sd_ndvi),
             size = 2, alpha = 1) +
  scale_fill_manual(values = custom_palette) +
  scale_x_discrete(labels = xlabs) + 
  labs(y = "Sentinel-2 std. deviation NDVI",
       x = "") +
  theme_cowplot(FONT_SIZE) +
  theme(legend.position = "none");p2

plot_grid(pa, p2,
          labels = c("a)","b)"),
          label_size = FONT_SIZE)

ggsave2("figures/Fig_A5.png",
        bg = "white",width = 14, height = 8)

# 4. Plot Figure A6 ----
#  Boxplot of std.dev Sentinel-2 SAVI
p1 <- ggplot(data) + 
  geom_boxplot(aes(x = Site, y = s2_mean_savi, 
                   fill = Site), alpha = 0.8) +
  geom_point(aes(x = Site,y = s2_mean_savi),
             size = 2, alpha = 1) +
  scale_fill_manual(values = custom_palette) +
  scale_x_discrete(labels = xlabs) +
  labs(y = "Sentinel-2 mean SAVI",
       x = "") +
  theme_cowplot(FONT_SIZE) +
  theme(legend.position = "none");p1

inset <- ggplot() + 
  geom_spatraster(data = s2_savi, show.legend = F) +
  geom_spatvector(data = plots_buffered, 
                  fill = NA, color = "white", linewidth = 1) +
  # add scale bar
  geom_rect(aes(xmin = ext_rast[1] + 30, xmax = ext_rast[1] + 60, 
                ymin = ext_rast[3] + 5, ymax = ext_rast[3] + 10),
            fill = 'white') + 
  geom_text(aes(x = ext_rast[1] + 45, y = ext_rast[3] + 20,
                label = '30 m',fontface = 'bold'),
            size = 4,
            colour = 'white') +
  scale_fill_viridis_c(option = "mako") +
  labs(fill = "Sentinel-2 SAVI") +
  theme_map(FONT_SIZE) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1));inset

# Combine figures
pa <- ggdraw(p1) + draw_plot(inset, .15, .6, .4, .4)

# Boxplot of new Sentinel-2 SAVI
p2 <- ggplot(data) + 
  geom_boxplot(aes(x = Site, y = s2_sd_savi, 
                   fill = Site), alpha = 0.8) +
  geom_point(aes(x = Site,y = s2_sd_savi),
             size = 2, alpha = 1) +
  scale_fill_manual(values = custom_palette) +
  scale_x_discrete(labels = xlabs) + 
  labs(y = "Sentinel-2 std. deviation SAVI",
       x = "") +
  theme_cowplot(FONT_SIZE) +
  theme(legend.position = "none");p2

plot_grid(pa, p2,
          labels = c("a)","b)"),
          label_size = FONT_SIZE)

ggsave2("figures/Fig_A6.png",
        bg = "white",width = 14, height = 8)

# Boxplot of old Sentinel-2 imagery
p2 <- ggplot(data) + 
  geom_boxplot(aes(x = Site, y = Sentinel2_mean, 
                   fill = Site)) +
  labs(y = "Sen-2 mean NDVI (larger time gaps in data)") +
  theme_cowplot()

ggarrange(p1, p2, ncol=2, common.legend = TRUE, legend="bottom")




ggsave2("figures/Boxplot_new_vs_old_S2_meanNDVI.png",
        bg = "white",width = 10, height = 8)
