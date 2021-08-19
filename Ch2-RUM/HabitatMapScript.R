# Main unprojected crs = 4283 (GDA94), units = m 

## Reading habitat layers
Pelagic <- st_read(paste(s.dir, "PelagicHabitat.gpkg", sep = '/')) %>%
   st_transform(crs = 4283)

Rockyreef <- st_read(paste(s.dir, "RockReefHabitat.gpkg", sep = '/')) %>%
    st_transform(crs = 4283)
Rockyreef <- st_make_valid(Rockyreef) %>%
   st_crop(StudySite)

Reef <- st_read(paste(s.dir, "ReefHabitat.gpkg", sep = '/')) %>%
   st_transform(crs = 4283)

Lagoon <- st_read(paste(s.dir, "LagoonHabitat.gpkg", sep = '/')) %>%
   st_transform(crs = 4283) %>%
   st_crop(StudySite)

## Making habitat grids - use this to manipulate size of grids within habitat polygons

PelagicGrid <- Pelagic %>%
  st_make_grid(cellsize = 0.05, square = FALSE, crs=4283) %>%
  st_intersection(Pelagic)
 st_write(PelagicGrid, paste(s.dir, "PelagicGrid.gpkg", sep = '/'), append = FALSE)

 RockyreefGrid <- Rockyreef %>%
   st_make_grid(cellsize = 0.03, square = FALSE, crs=4283) %>%
   st_intersection(Rockyreef)
 st_write(RockyreefGrid, paste(s.dir, "RockyreefGrid.gpkg", sep = '/'), append = FALSE)

 ReefGrid <- Reef %>%
   st_make_grid(cellsize = 0.03, square = FALSE, crs=4283) %>%
   st_intersection(Reef)
 st_write(ReefGrid, paste(s.dir, "ReefGrid.gpkg", sep = '/'), append = FALSE)

LagoonGrid <- Lagoon %>%
  st_make_grid(cellsize = 0.03, square = FALSE, crs=4283) %>%
  st_intersection(Lagoon)
st_write(LagoonGrid, paste(s.dir, "LagoonGrid.gpkg", sep = '/'), append = FALSE)

## Adding habitat variable to Grids
PelagicGrid$Habitat <- c("Pelagic")
RockyreefGrid$Habitat <- c("Rockyreef")
ReefGrid$Habitat <- c("Reef")
LagoonGrid$Habitat <- c("Lagoon")

## Joining Grids
Grid <- rbind(PelagicGrid, RockyreefGrid)
Grid <- rbind(Grid, ReefGrid)
Grid <- rbind(Grid, LagoonGrid)

## Giving each grid individual ID
Grid <- Grid %>%
  mutate(GridID = row_number())

st_write(Grid, paste(s.dir, "Grid.gpkg", sep = '/'), append = FALSE)