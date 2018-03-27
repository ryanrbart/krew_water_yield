# Code for prepping gridded climate data


# ---------------------------------------------------------------------
# Test data for p301

# readin patch, generate clim_ID

patch_ID=scan(file="ws_p301/auxdata/patch_p301.asc", skip=6, na.strings="*")
LAI=scan(file="ws_p301/auxdata/lai_p301.asc", skip=6, na.strings="*") # LAI used as placeholder for DEM

tmp = as_tibble(cbind(clim_ID = patch_ID,dem_ID = LAI))
tmp2 = subset(tmp, is.na(tmp$clim_ID)==F)


# Create ascii input table


# Make gridded climate base station file

input_clim_base <- as_tibble(matrix(nrow=4,ncol=length(tmp2$clim_ID)))
input_clim_base[1,] <- c(as.character(length(tmp2$clim_ID)), rep("", length(tmp2$clim_ID)-1))
input_clim_base[2,] <- c(as.character(1941), "1", "1", "1", rep("", length(tmp2$clim_ID)-4))
input_clim_base[3,] <- tmp2$clim_ID
input_clim_base[4,] <- tmp2$dem_ID



input_clim_base[4,] <- c(as.character(length(tmp2$clim_ID)), "", "", "")
                              
input_clim_base_list[1,] <- c(c1=length(clim_ID), c2="", c3="", c4="",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][2,] <- data.frame(c1=100.0, c2="x_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][3,] <- data.frame(c1=100.0, c2="y_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][4,] <- data.frame(c1=1748, c2="z_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][5,] <- data.frame(c1=3.5, c2="effective_lai",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][6,] <- data.frame(c1=2, c2="screen_height",stringsAsFactors=FALSE)





