# Code for processing detailed thinning data
# Note: This is incomplete since Safeeq completed this analysis

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Import data

# Treatment azimuth data
treat_azimuth <- read_csv("data/treatments_azimuth.csv")

# Treatment coordinate data
treat_prov_grid <- read_csv("data/treatments_prov_2006_veg_grid_coordinates.csv")
treat_bull_grid <- read_csv("data/treatments_bull_2006_veg_grid_coordinates.csv")

# Treatment basal raw
basal_data_raw <- read_csv("data/treatments_basal_data_raw.csv")
# Treatment basal summary
basal_data_sum <- read_csv("data/treatments_basal_data_summary.csv")


# ---------------------------------------------------------------------
# Clean up datasets

# Azimuth - Get rid of NAs and duplicates
treat_azimuth <- treat_azimuth %>% 
  distinct() %>% 
  filter(!is.na(Plot)) 

# Coordinate - Change watershed value
treat_grid <- bind_rows(treat_prov_grid, treat_bull_grid)   # Combine coordinate data
treat_grid <- mutate(treat_grid, Watershed = if_else(`WATERSHED CODE` == "PROV 301","P301",
                                            if_else(`WATERSHED CODE` == "DUFF 102","D102",
                                            if_else(`WATERSHED CODE` == "PROV 303","P303",
                                            if_else(`WATERSHED CODE` == "PROV 304","P304",`WATERSHED CODE`)))))

# ---------------------------------------------------------------------

# Join basal with azimuth data
basal_coord_azimuth <- inner_join(basal_data_raw, treat_azimuth, by=c("Plot" = "Plot", "Catchment" = "Watershed"))

# Join basal with coordinate data (basal and coordinate have only 53 points in common, instead of 98)
# basal_coord_azimuth <- inner_join(basal_coord_azimuth, treat_grid, by=c("Plot" = "GRID CODE", "Catchment" = "Watershed"))

# Notes about datasets The table with basal area has 98 sites. In the azimuth
# data, there are 174 sites but it also contains the 98 basal sites. The azimuth
# table contains both azimuth and coordinate data. The coordinate tables contain
# 265 sites, but only 53 correspond to basal sites. Consequently, the coordinate
# data from the azimuth table is used instead of the coordinates from the
# coordinate table.

# ---------------------------------------------------------------------
# Plot spatial data

# Basal data
basal_summary <- basal_coord_azimuth %>% 
  dplyr::select(Catchment, Plot, Azimuth, GISEasting, GISNorthing) 
coordinates(basal_summary) <- c("GISEasting","GISNorthing")
proj4string(basal_summary) <- CRS("+init=epsg:32611") 

# Watershed data
prov_sheds <- readOGR(dsn = "data/gis/krew_watersheds/", layer="Providence_Watersheds")
prov_p300 <- readOGR(dsn = "data/gis/krew_watersheds/", layer="Providence_Integrated")
bull_sheds <- readOGR(dsn = "data/gis/krew_watersheds/", layer="Bull_Watersheds")
bull_b200 <- readOGR(dsn = "data/gis/krew_watersheds/", layer="Bull_Integrated")


# Plot Providence
plot(prov_sheds)
plot(prov_p300, add=TRUE)
plot(basal_summary, pch=20, add=TRUE)

# Plot Bull
plot(bull_sheds)
plot(bull_b200, add=TRUE)
plot(basal_summary, pch=20, add=TRUE)






# ---------------------------------------------------------------------
# Looking at KREW.gdb


happy <- readOGR(dsn = "GIS/KREW.gdb")

# The input file geodatabase
fgdb <- "GIS/KREW.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="providence_fire_lines_dozer1")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)


