# Code for creating year with repeating precipitation

# Average annual precipitation in P301 is 1308 mm
# Repeating 1943, which has annual precipitation of 1284.

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------


# Read in met data
happy <- read_rhessys_met("ws_p301/clim/Grove_lowprov_clim_1942_2453")


# Analyze precipitation data
plot(happy$rain[365:730], type="l")
sum(happy$rain[365:730])

happy2 <- happy %>% 
  dplyr::group_by(wy) %>% 
  summarize(precip = sum(rain))


# -------------

# Write out new climate data
RHESSysIOinR::write_sample_clim("ws_p301/clim/Grove_lowprov_clim_1943_repeat",
                                clim = happy,
                                samplewyrs = 1943,
                                reps = 20)

# Optional check to see if new clim data is set up correctly
happy3 <- read_rhessys_met("ws_p301/clim/Grove_lowprov_clim_1943_repeat")




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Add patch-level base_station_id to world file (for dated sequences)

world_name_in <- "ws_p301/worldfiles/p301.world"
world_name_out <- "ws_p301/worldfiles/p301_mod.world"

newrow <- data.frame(a=101, b="p_base_station_ID", stringsAsFactors = FALSE)

# Read in worldfile
worldfile <- read.table(world_name_in, header = FALSE, stringsAsFactors = FALSE)

for (aa in seq(2,nrow(worldfile))){         # Note that this should be a while loop since worldfile is extended. (see other examples)
  if (aa%%1000 == 0 ){print(paste(aa,"out of", nrow(worldfile)))} # Counter
  if ((worldfile[aa,2] == "num_canopy_strata" | worldfile[aa,2] == "num_stratum") && worldfile[aa-1,2] == "n_basestations"){
    # Change previous n_basestations to 1
    worldfile[aa-1,1] = 1
    
    # Add new line containing p_base_station_ID
    worldfile[seq(aa+1,nrow(worldfile)+1),] <- worldfile[seq(aa,nrow(worldfile)),]
    worldfile[aa,] <- newrow[1,]
  }
}

# Write new file
worldfile$V1 <- format(worldfile$V1, scientific = FALSE)
write.table(worldfile, file = world_name_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="  ")




