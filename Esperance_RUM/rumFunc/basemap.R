# libraries
library(tidyverse)
library(ggplot2)
library(rgeos)
library(rgdal)
library(sf)
library(ggspatial)

# directories
w.dir <- "/Users/23088313/Documents/git_repos/Analysis-Hamre-Bioeconomic" # set working directory
d.dir <- paste(w.dir, "Esperance_RUM/rumIgnore", sep='/') # set data directory
f.dir <- paste(w.dir, "Esperance_RUM/rumFunc", sep = '/') # set functions directory
s.dir <- paste(w.dir, "spIgnore/gpkg", sep='/') # set gpkg directory
r.dir <- paste(w.dir, "spIgnore/raster", sep='/') # set raster directory
rumPlots <- paste(w.dir, "Esperance_RUM/rumPlots", sep='/') # set plots directory

# spatial data
coast <- st_read(paste(s.dir, "Esp_coast.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)

# might need adjust limits of bbox when you get new data
bbox <- st_bbox(c(xmin = 121, xmax = 123, ymax = -33.4, ymin = -34.5), crs = st_crs(4283))
coast <- st_crop(coast, bbox)

basemap <- 
  ggplot() +
  geom_sf(data = coast, lwd = 0.07) +
  annotation_scale(location = "tr", pad_x=unit(1, "cm"), pad_y = unit(1, "cm"), height = unit(1, "mm"), text_cex = 0.6, bar_cols = c(" dark grey", "white"), line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(1, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.5, text_size = 6, line_col = "dark grey",
                                                                fill = c("white", "dark grey"), text_col = "dark grey"),
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  theme_void()

basemap
# ggsave(paste(rumPlots, "esp_basemap.png", sep='/'), width = 4, height = 6)

rm(espCoast)

