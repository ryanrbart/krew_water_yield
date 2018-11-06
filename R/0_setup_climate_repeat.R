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


