# libraries
library(tidyverse)
library(ggplot2)
library(rgeos)
library(rgdal)
library(sf)
library(ggspatial)
library(ggrepel)

# directories
w.dir <- "/Users/23088313/Documents/git_repos/Analysis-Hamre-Bioeconomic" # set working directory
d.dir <- paste(w.dir, "Esperance_RUM/rumIgnore", sep='/') # set data directory
f.dir <- paste(w.dir, "Esperance_RUM/rumFunc", sep = '/') # set functions directory
# s.dir <- paste(w.dir, "spIgnore/shp", sep='/') # set shp directory
gpkg.dir <- paste(w.dir, "spIgnore/gpkg", sep='/') # set gpkg directory
r.dir <- paste(w.dir, "spIgnore/raster", sep='/') # set raster directory
rumPlots <- paste(w.dir, "Esperance_RUM/rumPlots", sep='/') # set plots directory

# spatial data
coast <- st_read(paste(gpkg.dir, "EspCoast.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
BR <- st_read(paste(gpkg.dir, "Esp_BR.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)

# might need adjust limits of bbox when you get new data
bbox <- st_bbox(c(xmin = 121, xmax = 123, ymax = -33.4, ymin = -34.5), crs = st_crs(4283))
coast <- st_crop(coast, bbox)

basemap <-
  ggplot() +
  geom_sf(data = coast, lwd = 0.07) +
  # geom_sf(data = port, fill = NA, col = "red") +
  # geom_sf(data = port_secure, fill = NA, col = "blue") +
  annotation_scale(location = "tr", pad_x=unit(1, "cm"), pad_y = unit(1, "cm"), height = unit(1, "mm"), text_cex = 0.6, bar_cols = c(" dark grey", "white"), line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(1, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.5, text_size = 6, line_col = "dark grey",
                                                                fill = c("white", "dark grey"), text_col = "dark grey"),
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  theme_void()

basemap
  
# port map 
bbox_port <- st_bbox(c(xmin = 121.7, xmax = 122.2, ymax = -33.8, ymin = -33.96), crs = st_crs(4283))
coast_port <- st_crop(coast, bbox_port)

port <- st_read(paste(gpkg.dir, "Esp_Port.gpkg", sep = '/')) %>%
  st_transform(crs = 4283) 
# port <- st_crop(port, bbox_port)
# st_write(port, paste(gpkg.dir, "Esp_Port.gpkg", sep = '/'), append = FALSE)

port_secure <- st_read(paste(gpkg.dir, "Esp_PortSecure.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)

basemap_port <-
  ggplot() +
  geom_sf(data = coast_port, lwd = 0.07) +
  geom_sf(data = port, fill = NA) +
  geom_sf(data = port_secure, fill = NA) +
  annotation_scale(location = "tr", pad_x=unit(1, "cm"), pad_y = unit(1, "cm"), height = unit(1, "mm"), text_cex = 0.6, bar_cols = c(" dark grey", "white"), line_width = 0.5) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(1, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.5, text_size = 6, line_col = "dark grey",
                                                                fill = c("white", "dark grey"), text_col = "dark grey"),
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  theme_void()

basemap_port

# ggsave(paste(rumPlots, "esp_basemap.png", sep='/'), width = 4, height = 6)

# rm(espCoast)


# ggplot() +
#   geom_sf(data = port, fill = alpha("red", 0.2)) +
#   geom_sf(data = port_secure, fill = alpha("blue", 0.2))
# 
# ggplot() +
#   geom_sf(data = port, fill = NA, col = "red") +
#   geom_sf(data = port_secure, fill = NA, col = "blue")
# 
# 
# ggplot(data = BR, aes(label = Label)) +
#   geom_sf() +
#   geom_sf_label(fill = "white",  # override the fill from aes()
#                 fun.geometry = sf::st_centroid) 
