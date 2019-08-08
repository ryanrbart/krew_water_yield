# KREW NDVI Filtering 

# This script allows the filtering of the QCed NDVI data for further criteria (e.g. month).


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import KREW NDVI QC data

ndvi_table_qc <- read_rds("output/3.2/ndvi_table_qc.rds")
ndvi_prov_qc <- read_rds("output/3.2/ndvi_prov_qc.rds")
ndvi_bull_qc <- read_rds("output/3.2/ndvi_bull_qc.rds")

# ---------------------------------------------------------------------
# This file may be obsolete (retained for the time-being)

ndvi_table_final <- ndvi_table_qc
ndvi_prov_final <- ndvi_prov_qc
ndvi_bull_final <- ndvi_bull_qc

# ---------------------------------------------------------------------
# Select other variables that data should be filtered by (e.g. month)

# Add a row number to ndvi_table
# ndvi_table_final <- ndvi_table_qc %>% 
#   dplyr::mutate(row_num_qc = row_number())

# Select final filtering
# ndvi_table_final <- ndvi_table_final %>%
#   dplyr::filter(sensor == "LE07")

# ndvi_table_final <- ndvi_table_final %>% 
#   dplyr::filter(month %in% c(8))

# ---
# ndvi_table_final <- ndvi_table_final %>% 
#   dplyr::slice(-c(14,20,21,23,26,27,39,52,57))
# ---

#ndvi_table_final

# ---------------------------------------------------------------------
# Process Landsat NDVI data

# ndvi_prov_final <- raster::subset(ndvi_prov_qc, ndvi_table_final$row_num_qc)
# ndvi_bull_final <- raster::subset(ndvi_bull_qc, ndvi_table_final$row_num_qc)
#ndvi_prov_final
#ndvi_bull_final

# ---------------------------------------------------------------------
# Save processed NDVI data

write_rds(ndvi_table_final, "output/3.3/ndvi_table_final.rds")
write_rds(ndvi_prov_final, "output/3.3/ndvi_prov_final.rds")
write_rds(ndvi_bull_final, "output/3.3/ndvi_bull_final.rds")


# ---------------------------------------------------------------------
# Plot scenes

par(mfrow=c(1,2))

for (aa in seq_len(nlayers(ndvi_prov_qc))){
  
  plot(ndvi_prov_qc[[aa]])
  title(main=paste(aa,"YearMonthScene", ndvi_table_qc$yms[aa]))
  
  plot(ndvi_bull_qc[[aa]])
  title(main=paste(aa,"YearMonthScene", ndvi_table_qc$yms[aa]))
  
  readline(prompt="Press [enter] to continue")
}

par(mfrow=c(1,1))




