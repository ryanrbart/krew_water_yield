# KREW NDVI QC usign annual Maximum-value composites (MVC)

# This script imports KREW Landsat NDVI and then QCs layers by the maximum value
# per pixel per year.

# Output: QCed rasters and maps of selected scenes

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import KREW NDVI data

ndvi_table <- read_rds("output/1.1/ndvi_table.rds")
ndvi_prov <- read_rds("output/1.1/ndvi_prov.rds")
ndvi_bull <- read_rds("output/1.1/ndvi_bull.rds")


# ---------------------------------------------------------------------
# Identify the min and max NDVI in every layer to get rid of scenes with NDVIs greater than 1

# Create vectors of showing max and min ndvi values for a layer
ndvi_max_prov <- vector()
ndvi_min_prov <- vector()
ndvi_max_bull <- vector()
ndvi_min_bull <- vector()
for (aa in seq_len(nlayers(ndvi_prov))){
  ndvi_max_prov[aa] <- max(values(ndvi_prov[[aa]]), na.rm=TRUE)
  ndvi_min_prov[aa] <- min(values(ndvi_prov[[aa]]), na.rm=TRUE)
  ndvi_max_bull[aa] <- max(values(ndvi_bull[[aa]]), na.rm=TRUE)
  ndvi_min_bull[aa] <- min(values(ndvi_bull[[aa]]), na.rm=TRUE)
}

# Add vectors to ndvi_table and create new columns showing whether layer is within limits
ndvi_table_qc <- ndvi_table %>% 
  dplyr::mutate(ndvi_max_prov = ndvi_max_prov,
                ndvi_min_prov = ndvi_min_prov,
                ndvi_max_bull = ndvi_max_bull,
                ndvi_min_bull = ndvi_min_bull)

# ---
# Remove layers that do not meet all requirements
ndvi_table_qc <- ndvi_table_qc %>% 
  dplyr::filter(sensor == "LE07") %>% 
  dplyr::filter(month %in% c(5,6,7,8,9)) %>% 
  dplyr::filter(ndvi_max_prov > 0.0 & ndvi_max_prov < 1.0 &
                  ndvi_max_bull > 0.0 & ndvi_max_bull < 1.0) %>%
  dplyr::select(-c(ndvi_max_prov,ndvi_min_prov,ndvi_max_bull,ndvi_min_bull))
#ndvi_table_qc

# ---
# Consolidate table by years to relabel ndvi_prov_qc and ndvi_bull_qc below 
ndvi_table_label <- ndvi_table_qc %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(short_name = first(short_name))


# ---------------------------------------------------------------------
# MVC

for (aa in seq_along(unique(ndvi_table_qc$year))){
  print(paste("Year", aa - 1 + min(ndvi_table_qc$year)))
  
  # Subset by year
  ndvi_table_sub <- dplyr::filter(ndvi_table_qc, year == (aa - 1 + min(ndvi_table_qc$year)))
  print(paste("Scenes", nrow(ndvi_table_sub)))
  ndvi_prov_sub <- raster::subset(ndvi_prov, ndvi_table_sub$row_num)
  ndvi_bull_sub <- raster::subset(ndvi_bull, ndvi_table_sub$row_num)
  
  # Find max value per pixel per year and stack
  if (aa == 1){
    ndvi_prov_qc <- max(ndvi_prov_sub, na.rm = TRUE)
    ndvi_bull_qc <- max(ndvi_bull_sub, na.rm = TRUE)
  } else {
    ndvi_prov_qc <- raster::stack(ndvi_prov_qc, max(ndvi_prov_sub, na.rm = TRUE))
    ndvi_bull_qc <- raster::stack(ndvi_bull_qc, max(ndvi_bull_sub, na.rm = TRUE))
  }
}
# ndvi_prov_qc
# ndvi_bull_qc

# Relabel the layers
names(ndvi_prov_qc) <- ndvi_table_label$short_name
names(ndvi_bull_qc) <- ndvi_table_label$short_name

# ---------------------------------------------------------------------
# Save landsat NDVI data for KREW

write_rds(ndvi_table_qc, "output/1.2/ndvi_table_qc.rds")
write_rds(ndvi_prov_qc, "output/1.2/ndvi_prov_qc.rds")
write_rds(ndvi_bull_qc, "output/1.2/ndvi_bull_qc.rds")




