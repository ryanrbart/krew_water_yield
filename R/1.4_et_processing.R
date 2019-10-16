# NDVI to ET Processing 

# This script converts spatially distributed NDVI to ET.

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import processed NDVI data

ndvi_prov_final <- read_rds("output/1.3/ndvi_prov_final.rds")
ndvi_bull_final <- read_rds("output/1.3/ndvi_bull_final.rds")

# ---------------------------------------------------------------------
# Change NDVI to ET

# ndvi_to_et <- function(ndvi){
#   # Calculate ET
#   et = 123.8243*exp(2.5456*ndvi)                   # Roche2018 for Landsat
#   # et = 3.7129*exp(3.0748*ndvi)*satVPweighted     # Bales2018 for MODIS
#   # et = 3.7129*exp(3.0748*ndvi)*23.52738          # Bales2018 for MODIS (satvp value from average satvp value on 'ET regression' tab)
#   # et = 10.3247*exp(2.8599*ndvi)                  # Goulden2014 for MODIS
#   # et = 101.49*exp(2.6853*ndvi)                   # Goulden2012 for MODIS
#   return(et)
# }
# 
# et_prov <- raster::calc(ndvi_prov, fun = ndvi_to_et)
# et_bull <- raster::calc(ndvi_bull, fun = ndvi_to_et)
# Note: calc function is removing the layer names. The below approach seems to work better.

# Names sometimes does not get passed. setNames does it explicitly
et_prov_final <- setNames(123.8243*exp(2.5456*ndvi_prov_final), names(ndvi_prov_final))
et_bull_final <- setNames(123.8243*exp(2.5456*ndvi_bull_final), names(ndvi_bull_final))

# ---------------------------------------------------------------------
# Save processed ET data

write_rds(et_prov_final, "output/1.4/et_prov_final.rds")
write_rds(et_bull_final, "output/1.4/et_bull_final.rds")




