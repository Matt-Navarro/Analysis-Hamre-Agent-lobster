# libraries
library(tidyverse)
library(ggplot2)
library(rgeos)
library(rgdal)
library(sf)

# directories
w.dir <- "/Users/23088313/Documents/git_repos/Analysis-Hamre-Bioeconomic"
d.dir <- paste(w.dir, "Ningaloo_RUM/rumIgnore", sep='/')
s.dir <- paste(w.dir, "spIgnore/gpkg", sep='/')
f.dir <- paste(w.dir, "Ningaloo_RUM/rumFunc", sep = '/')
rumPlots <- paste(w.dir, "Ningaloo_RUM/rumPlots", sep='/')

# spatail data
coast <- st_read(paste(s.dir, "NWScoast.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp1 <- st_read(paste(s.dir, "nmpConserve.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp2 <- st_read(paste(s.dir, "nmpGeneralUse.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp3 <- st_read(paste(s.dir, "nmpManage.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp4 <- st_read(paste(s.dir, "nmpOutline.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp5 <- st_read(paste(s.dir, "nmpRec.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp6 <- st_read(paste(s.dir, "nmpSanc.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp7 <- st_read(paste(s.dir, "nmpShoreFish.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp8 <- st_read(paste(s.dir, "nmpSpPurposeBenthic.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
nmp9 <- st_read(paste(s.dir, "nmpSpPurposeShore.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
ncmp1 <- st_read(paste(s.dir, "ncmpSplit.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)
ncmp2 <- st_read(paste(s.dir, "ncmpSZ.gpkg", sep = '/')) %>%
  st_transform(crs = 4283)

# base map
basemap <- 
  ggplot() +
  geom_sf(data = coast, lwd = 0.07) +
  geom_sf(data = ncmp2, lwd = 0.1, fill = NA) +
  geom_sf(data = nmp1, lwd = 0.1, fill = NA) +
  geom_sf(data = nmp2, lwd = 0.1, fill = NA) +
  # geom_sf(data = nmp3) +
  # geom_sf(data = nmp4) +
  geom_sf(data = nmp5, lwd = 0.1, fill = NA) +
  geom_sf(data = nmp6, lwd = 0.1, fill = NA) +
  geom_sf(data = nmp8, lwd = 0.1, fill = NA) +
  geom_sf(data = nmp9, lwd = 0.1, fill = NA) +
  annotation_scale(location = "br", pad_x=unit(0.7, "cm"), pad_y = unit(1, "cm"), height = unit(1, "mm"), text_cex = 0.6, bar_cols = c(" dark grey", "white"), line_width = 0.5) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.7, "cm"), pad_y = unit(1.5, "cm"),
                         style = north_arrow_fancy_orienteering(line_width = 1.5, text_size = 6, line_col = "dark grey",
                                                                fill = c("white", "dark grey"), text_col = "dark grey"),
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +
  theme_void() +
  theme(legend.title = element_text(size = 8, color = "#595959"),
        legend.text = element_text(size = 8, color = "#595959"),
        axis.text.x = element_text(size = 7, colour = "dark grey", angle = 90), 
        axis.text.y = element_text(size = 7, colour = "dark grey"))

# ning_base
# ggsave(paste(rumPlots, "basemap.png", sep='/'), width = 4, height = 6)

rm(nmp1, nmp2, nmp3, nmp4, nmp5, nmp6, nmp7, nmp8, nmp9, ncmp1, ncmp2)
