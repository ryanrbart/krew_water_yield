# KREW NDVI Pre-Processing 

# This script imports raw (tif) Landsat NDVI files of the Kings River watershed
# obtained from Safeeq, crops separately to Bull and Providence locations in the
# Krew watersheds and stacks the rasters.

# Output: Two rasters with 1027 layers

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import GIS data

prov <- st_read("data/GIS/krew_watersheds/Providence_Watersheds.shp")
prov <- prov %>% 
  st_transform(crs = proj_landsat)
prov_e <- extent(prov)

bull <- st_read("data/GIS/krew_watersheds/Bull_Watersheds.shp")
bull <- bull %>% 
  st_transform(crs = proj_landsat)
bull_e <- extent(bull)

kings <- st_read("GIS/Kings.shp")
kings <- kings %>% 
  st_transform(crs = proj_landsat)
kings_e <- extent(kings)


b202 <- st_read("data/GIS/b202.kml")
b202 <- b202 %>% 
  st_transform(crs = proj_landsat)
b202_e <- extent(b202)


# ---------------------------------------------------------------------
# Pull in NDVI layers and determine which are required for analysis 

# Create table of NDVI files and then parse so it can be filtered by scenes.
ndvi_files <- list.files("/Volumes/BART_ECOHYD/NDVI/",full.names = T)

ndvi_table <- tibble(ndvi_files)
ndvi_table$short_name <- stringr::str_sub(ndvi_table$ndvi_files, 28, 54)
ndvi_table$sensor <- stringr::str_sub(ndvi_table$ndvi_files, 28, 31)
ndvi_table$tile <- stringr::str_sub(ndvi_table$ndvi_files, 32, 37)
ndvi_table$yms <- stringr::str_sub(ndvi_table$ndvi_files, 38, 44)
ndvi_table$year <- as.integer(stringr::str_sub(ndvi_table$ndvi_files, 38, 41))   # Year
ndvi_table$month <- as.integer(stringr::str_sub(ndvi_table$ndvi_files, 42, 43))   # Month
ndvi_table$day <- as.integer(stringr::str_sub(ndvi_table$ndvi_files, 44, 45))   # Day
ndvi_table$tier <- (stringr::str_sub(ndvi_table$ndvi_files, 46, 49))   # Tier (1 is better than 2)  https://www.usgs.gov/land-resources/nli/landsat/landsat-collection-1       
ndvi_table <- dplyr::arrange(ndvi_table, yms)

# Add a row number to ndvi_table
ndvi_table <- dplyr::mutate(ndvi_table, row_num = row_number())

# ---------------------------------------------------------------------
# Process Landsat NDVI data for prov

# Import NDVI data, subset, and stack  
ndvi_prov <- ndvi_table$ndvi_files %>% 
  map(raster::raster) %>%                             # Import rasters
  map(function(x)raster::crop(x,prov_e)) %>%          # Subset rasters
  raster::stack()

# Turn prov vector boundries into raster so that areas outside of watersheds can be nulled
prov_rast <- rasterize(prov, ndvi_prov[[1]])

# Null out raster areas outside of prov watersheds
for (aa in seq_len(nlayers(ndvi_prov))){
  ndvi_prov[[aa]][is.na(prov_rast) == TRUE] <- NA
}

beep(1)

# ---------------------------------------------------------------------
# Process Landsat NDVI data for bull

# Import NDVI data, subset, and stack  
ndvi_bull <- ndvi_table$ndvi_files %>% 
  map(raster::raster) %>%                             # Import rasters
  map(function(x)raster::crop(x,bull_e)) %>%          # Subset rasters
  raster::stack()

# Turn bull vector boundries into raster so that areas outside of watersheds can be nulled
bull_rast <- rasterize(bull, ndvi_bull[[1]])

# Null out raster areas outside of bull watersheds
for (aa in seq_len(nlayers(ndvi_bull))){
  ndvi_bull[[aa]][is.na(bull_rast) == TRUE] <- NA
}

beep(1)

# ---------------------------------------------------------------------
# Save landsat NDVI data for KREW

# KREW watersheds: Vector 
write_rds(prov, "output/1.1/prov.rds")
write_rds(bull, "output/1.1/bull.rds")

# KREW watersheds: Raster 
write_rds(prov_rast, "output/1.1/prov_rast.rds")
write_rds(bull_rast, "output/1.1/bull_rast.rds")

# NDVI table and data
write_rds(ndvi_table, "output/1.1/ndvi_table.rds")
write_rds(ndvi_prov, "output/1.1/ndvi_prov.rds")
write_rds(ndvi_bull, "output/1.1/ndvi_bull.rds")



