# Script to plot Figure 3
# 31.05.2024
# Nils Rietze: nils.rietze@uzh.ch

library(tidyverse)
library(tidyterra)
library(terra)
library(grid)
library(cowplot)
library(colorspace)
library(extrafont)
library(ggtext)
loadfonts(device = "win")

# 1. Load and configure stuff ----

FIG_PATH <- "./figures/"
DATA_PATH <- "./data/"
# load site table
coords <- read.csv(paste0(DATA_PATH,"tables/biomass_cover_reformated_drone_sites_only.csv")) %>% 
  vect(geom = c("Long","Lat"),crs = "epsg:4326") %>% 
  terra::project("epsg:32643")

sites <- c("Vize", "Uedineniya", "Pioneer")
vars <- c("cut","fcover","biomass","uncertainty")

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
  
  if (variable == "fcover"){
    raster <- raster$fcover_vegetation
  } 
  
  # add site name
  varnames(raster) <- site
  
  return(raster)
}

# load binary maps
r_class <- lapply(sites,loadRast,variable =  vars[1])

# load fcover maps
r_fcov <- lapply(sites,loadRast,variable =  vars[2])

# load biomass maps
r_biom <- lapply(sites,loadRast,variable =  vars[3])

# load uncertainty maps
r_unc <- lapply(sites,loadRast,variable =  vars[4])

# 2. Make Figure 3 ----
font_size <- 22

# Function to plot individual raster
PlotRasters <- function(raster, variable){
  
  # extract plots in site
  points <- coords %>% 
    project(crs(raster)) %>% 
    terra::intersect(ext(raster))
  
  n <- 9
  pal <- brewer.pal(n,name = "BrBG")
  
  colors <- c(pal[3], pal[n],"steelblue") # Greens colors
  # colors <- c("#D6D6D6", "#6E8B3D","steelblue") # custom colors
  
  if (variable == "cut"){
    raster <- as.factor(raster)
    
    if (varnames(raster) == "Vize"){
      xtext <- 533730
      ytext <- 8837410
    } else if (varnames(raster) == "Uedineniya"){
      xtext <- 531030
      ytext <- 8603870
    } else if (varnames(raster) == "Pioneer"){
      xtext <- 478270
      ytext <- 8889320
    }
    
    p <- ggplot() + 
      geom_spatraster(data = raster, show.legend = FALSE) + 
      geom_spatvector(data = points, show.legend = FALSE,
                      color = "white", size = 3) +
      scale_fill_manual(values = c("1" = colors[2], "2" = colors[1]),
                        na.value = colors[3],
                        labels = c("Vegetation","Background"),
                        guide = guide_legend(title.position = "bottom")) +
      # scale bar
      geom_rect(aes(xmin = xtext - 120, xmax = xtext - 20, 
                    ymin = ytext - 10, ymax = ytext - 20),
                fill = 'white') +
      geom_text(aes(x = xtext -70, y = ytext + 15,
                    label = '100 m',fontface = 'bold'),
                size = 6,colour = 'white') +
      # north arrow
      geom_text(aes(x = xtext, y = ytext,
                    label = 'N',fontface = 'bold'),
                size = 6, colour = 'white') +
      geom_text(aes(x = xtext + 2, y = ytext + 20,
                    label = '$', angle = 90,
                    family = 'ESRI arrowhead',
                    fontface = 'bold'),
                size = 6, colour = 'white') +
      # plot styling
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_map(font_size) +
      theme(plot.subtitle = element_text(hjust = 0.5),
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.position = "right",
            legend.box = "vertical",
            legend.justification = "center",
            panel.border = element_rect(colour = "black", fill=NA, linewidth=1))
  } else {
    
    # define label and color palettes
    if(variable == "fcover"){
      # lab_var <- "Vegetation fCover (%)"
      lab_var <- "(%)"
      pal <- sequential_hcl(99, palette = "Viridis")
      vmax <- 100
    } else if (variable == "biomass"){
      # lab_var <- expr(Biomass~estimation~(g/m^2))
      lab_var <- "(g/m²)"
      pal <- sequential_hcl(99, palette = "Inferno")
      vmax <- 1300
    } else {
      # lab_var <- expr(Uncertainty~(g/m^2))
      lab_var <- "(g/m²)"
      pal <- sequential_hcl(99, palette = "Reds3",rev = T)
      vmax <- 1300
    }
    
    # identify max value for legend
    # vmax <- terra::global(raster,'max',na.rm = TRUE)[[1]]
    # vmax <- plyr::round_any(vmax,f = ceiling,accuracy = 100)
    
    p <- ggplot() + 
      geom_spatraster(data = raster) + 
      geom_spatvector(data = points, show.legend = FALSE,
                      color = ifelse(variable == "uncertainty","black","white"), size = 3) +
      scale_fill_gradientn(colors = pal,
                        limits = c(0,vmax),
                        breaks = seq(0,vmax,length.out = 5),
                        labels = seq(0,vmax,length.out = 5),
                        na.value = colors[3],
                        guide = guide_colourbar(title.position = "bottom")) +
      # plot styling
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_map(font_size) +
      theme(plot.subtitle = element_text(hjust = 0.5),
            plot.margin = unit(c(0,0,0,0), "cm"),
            legend.position = "right",
            legend.box = "vertical", 
            legend.justification = "center",
            panel.border = element_rect(colour = "black",
                                        fill=NA,
                                        linewidth=1)) +
      labs(fill = lab_var)
  }
    
  return(p)
}

# plot all rasters and store in list
class_plots <- lapply(r_class,PlotRasters, variable = vars[1])
fcov_plots <- lapply(r_fcov,PlotRasters, variable = vars[2])
biom_plots <- lapply(r_biom,PlotRasters, variable = vars[3])
unc_plots <- lapply(r_unc,PlotRasters, variable = vars[4])

## Arrange site plots ----
varnames <- c("Land Cover Class",
              "Fractional Vegetation Cover",
              "Biomass Estimate",
              "Biomass Uncertainty")

vize_id <- 1
vjust_sub <- 7
vize_row <- plot_grid(
   class_plots[[vize_id]] +
     labs(subtitle = varnames[1]) + 
     theme(plot.subtitle = element_text(vjust = vjust_sub,
                                        size = font_size,
                                        face = "bold")), 
   fcov_plots[[vize_id]] +
     # labs(subtitle = " ") + 
     labs(subtitle = varnames[2]) + 
    theme(legend.position="none",
          plot.subtitle = element_text(vjust = vjust_sub,
                                       size = font_size,
                                       face = "bold")),
  biom_plots[[vize_id]] +
    # labs(subtitle = " ") + 
    labs(subtitle = varnames[3]) + 
    theme(legend.position="none",
          plot.subtitle = element_text(vjust = vjust_sub,
                                       size = font_size,
                                       face = "bold")),
  unc_plots[[vize_id]] +
    # labs(subtitle = " ") + 
    labs(subtitle = varnames[4]) + 
    theme(legend.position="none",
          plot.subtitle = element_text(vjust = vjust_sub,
                                       size = font_size,
                                       face = "bold")),
  # labels = c("a)","b)","c)","d)"), label_size = font_size,
  ncol = 4
  )

vize_row <- ggdraw() +
  draw_plot(vize_row, x = 0, y = 0, width = 1, height = 0.95) +
  # draw_text(varnames, x = c(0.05,0.3,0.55,0.85), y = 1,  
  #           vjust = 0.95,hjust = 0,
  #           size = font_size,fontface = "bold")
  # add site label on left
  # draw_text("Vize", x = 0.01, y = 0.5, angle = 90, vjust = 1, 
  #           size = font_size,fontface = "bold") +
  draw_text("(mean biomass: 916 g/m², CI: 673-1162 g/m²; median biomass: 1100 g/m²)", 
            x = 0.1, y = 0.965,  
            vjust = 1.5,hjust = 0,
            size = font_size)

ued_id <- 2
ued_row <- plot_grid(
  class_plots[[ued_id]] +
    labs(subtitle = " ") + 
    theme(legend.position="none"),
  fcov_plots[[ued_id]] +
    labs(subtitle = " ") + 
    theme(legend.position="none"),
  biom_plots[[ued_id]] +
    labs(subtitle = " ") + 
    theme(legend.position="none"),
  unc_plots[[ued_id]] +
    labs(subtitle = " ") + 
    theme(legend.position="none"),
  # labels = c("i)","j)","k)","l)"), label_size = font_size,
  ncol = 4
  )

ued_row <- ggdraw() +
  draw_plot(ued_row, x = 0, y = 0, width = 1, height = 0.95) +
  draw_text("(mean biomass: 39 g/m², CI: 28-94 g/m² ; median biomass: 0 g/m²)", 
            x = 0.2, y = 0.965,  
            vjust = 1.5,hjust = 0,
            size = font_size)
  # add site label on left
  # draw_plot(ued_row, x = 0.05, y = 0, width = 0.95, height = 1) +
  # draw_text("Uedineniya", x = 0.01, y = 0.5, angle = 90, vjust = 1, 
  #           size = font_size,fontface = "bold")

pioneer_id <- 3
pioneer_row <- plot_grid(
  class_plots[[pioneer_id]] +
    labs(subtitle = " ") +
    theme(legend.position="none"),
  fcov_plots[[pioneer_id]] +
    labs(subtitle = " ") +
    theme(legend.position="none"),
  biom_plots[[pioneer_id]] +
    labs(subtitle = " ") +
    theme(legend.position="none"),
  unc_plots[[pioneer_id]] +
    labs(subtitle = " ") +
    theme(legend.position="none"),
  # labels = c("e)","f)","g)","h)"), label_size = font_size,
  ncol = 4
  )

pioneer_row <- ggdraw() +
  draw_plot(pioneer_row, x = 0, y = 0, width = 1, height = 0.95) +
  draw_text("(mean biomass: 138 g/m², CI: 100-203 g/m²; median biomass: 46 g/m²)", 
            x = 0.15, y = 0.965,  
            vjust = 1.5,hjust = 0,
            size = font_size)
  # add site label on left
  # draw_plot(pioneer_row, x = 0.05, y = 0, width = 0.95, height = 1) +
  # draw_text("Pioneer", x = 0.01, y = 0.5, angle = 90, vjust = 1, 
  #           size = font_size,fontface = "bold")

## Get legends and plot them ----
legend_fcov <- get_legend(
  fcov_plots[[pioneer_id]] + 
    # guides(color = guide_colourbar(barwidth=30,label.position="bottom")) +
    theme(legend.position = "bottom",
          legend.key.width=unit(35,"points"),
          legend.title = element_text(hjust = 0.5),
          legend.box.margin = unit(c(0.4,0,.5,0), "cm"))
)
legend_biom <- get_legend(
  biom_plots[[pioneer_id]] + 
    # guides(color = guide_colourbar(barwidth=30,label.position="bottom")) +
    theme(legend.position = "bottom",
          legend.key.width=unit(35,"points"),
          legend.title = element_text(hjust = 0.5),
          legend.box.margin = unit(c(0.4,0,.5,0), "cm"))
)
legend_unc <- get_legend(
  unc_plots[[pioneer_id]] + 
    # guides(color = guide_colourbar(barwidth=30,label.position="bottom")) +
    theme(legend.position = "bottom",
          legend.key.width=unit(35,"points"),
          legend.title = element_text(hjust = 0.5),
          legend.box.margin = unit(c(0.4,0,.5,0), "cm"))
)

n <- 9
pal <- brewer.pal(n,name = "BrBG")

colors <- c(pal[3], pal[n],"steelblue") # Greens colors

# construct legend row
leg_row <- plot_grid(ggplot() + 
                       labs(
                         subtitle = paste(
                           "<span style='color:", colors[2], "'>**Vegetation**</span>","<br>",
                           "<span style='color:",  colors[1], "'>**Substrate**</span>","<br>",
                           "<span style='color:",  colors[3], "'>**Water**</span>")
                       ) +
                       theme_map() +
                       theme(plot.subtitle = element_markdown(size = font_size)),
                     legend_fcov, legend_biom, legend_unc,
                     ncol = 4)

leg_row <- ggdraw() +
  # draw_plot(leg_row, x = 0.05, y = 0, width = 0.95, height = 1)
  draw_plot(leg_row, x = 0, y = 0, width = 1, height = 0.95)

## Build final composition ----
pg <- plot_grid(vize_row,pioneer_row,ued_row,leg_row,
                nrow = 4, 
                label_x = 0.01,
                hjust = 0,vjust = 2.5,
                labels = c("a) Vize",
                           "b) Pioneer",
                           "c) Uedineniya"),
                label_size = font_size,
                rel_heights = c(1,1,1,.2))

# export PNG
ggsave2(pg,
        filename = paste0(FIG_PATH,"Fig_3.png"),
        device = png, type = "cairo",
          bg = 'white',width = 12.5, height = 22.5)
