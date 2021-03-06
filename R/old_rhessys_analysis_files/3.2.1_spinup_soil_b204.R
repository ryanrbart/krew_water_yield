# Spinup of soil C and N pools for B204
#
# This code with a worldfile with no vegetation and carbon pools from a previous
# P301 worldfile. The watershed is spun up for ** years.

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Model inputs

# Processing options
parameter_method <- "all_combinations"


# RHESSys Inputs
input_rhessys <- list()
input_rhessys$rhessys_version <- "bin/rhessys5.20.1"
input_rhessys$tec_file <- "ws_b204/tecfiles/b204_3.2_spinup.tec"
input_rhessys$world_file <- "ws_b204/worldfiles/b204.world"
input_rhessys$world_hdr_prefix <- "b204_3.2_spinup"
input_rhessys$flow_file <- "ws_b204/flowtables/b204.flow"
input_rhessys$start_date <- "1941 10 1 1"
input_rhessys$end_date <- "1951 10 1 1"
input_rhessys$output_folder <- "ws_b204/out/3.1_b204_spinup_soils"
input_rhessys$output_filename <- "b204_spinup"
input_rhessys$command_options <- c("-b -g -c 1 649 31786 31786 -p 1 649 31786 31786 -tchange 0 0")
# Remember to switch on/off reference to spinup def file in input_hdr_list and extra line in worldfile
#input_rhessys$command_options <- c("-vegspinup ws_b204/tecfiles/spinup_thresholds.txt -b -g -tchange 0 0")    # Can't use -c or -p with -vegspinup


# HDR (header) file
input_hdr_list <- list()
input_hdr_list$basin_def <- c("ws_b204/defs/basin_b204.def")
input_hdr_list$hillslope_def <- c("ws_b204/defs/hill_b204.def")
input_hdr_list$zone_def <- c("ws_b204/defs/zone_b204.def")
input_hdr_list$soil_def <- c("ws_b204/defs/patch_b204.def")
input_hdr_list$landuse_def <- c("ws_b204/defs/lu_b204.def")
#input_hdr_list$stratum_def <- c("ws_b204/defs/veg_b204_conifer.def")
input_hdr_list$stratum_def <- c("ws_b204/defs/veg_b204_conifer.def", "ws_b204/defs/veg_b204_shrub.def")
#input_hdr_list$spinup_def <- c("ws_b204/defs/spinup_b204.def")
input_hdr_list$base_stations <- c("ws_b204/clim/Grove_lowprov_clim_1942_2453.base")


# Define path to a pre-selected df containing parameter sets
input_preexisting_table <- NULL


# Def file parameter changes
# List of lists containing def_file, parameter and parameters values
#input_def_list <- NULL
input_def_list <- list(
  # Lower canopy parameters (Note that parameters epc.leaf_turnover &
  # epc.branch_turnover differ from P301)
  list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(0.32)),
  list(input_hdr_list$stratum_def[2], "epc.livewood_turnover", c(0.1)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_frootc_leafc", c(1.4)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_crootc_stemc", c(0.4)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_stemc_leafc", c(0.2)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_livewoodc_woodc", c(0.9)),
  list(input_hdr_list$stratum_def[2], "epc.branch_turnover", c(0.015)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_exp", c(0.57)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_coef", c(4.0)),
  list(input_hdr_list$stratum_def[2], "epc.resprout_leaf_carbon", c(0.02)),
  # -----
  # Upper canopy parameters
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_exp", c(0.57)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(11.39)),
  list(input_hdr_list$stratum_def[1], "epc.resprout_leaf_carbon", c(0.02)),
  # -----
  # Patch level parameters
  list(input_hdr_list$soil_def[1], "soil_depth", c(2.0))
)

# Standard sub-surface parameters
# input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(1.792761),
  k = c(1.566492),
  m_v = c(1.792761),
  k_v = c(1.566492),
  pa = c(1),
  po = c(0.3),
  gw1 = c(0.001),
  gw2 = c(0.2)
)


# Make climate base station file
#input_clim_base_list <- NULL
input_clim_base_list <- list(
  list(core = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       annual = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       monthly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       daily = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       hourly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE)
  )
)
input_clim_base_list[[1]][[1]][1,] <- data.frame(c1=101, c2="base_station_id",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][2,] <- data.frame(c1=100.0, c2="x_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][3,] <- data.frame(c1=100.0, c2="y_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][4,] <- data.frame(c1=1748, c2="z_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][5,] <- data.frame(c1=3.5, c2="effective_lai",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][6,] <- data.frame(c1=2, c2="screen_height",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[2]][1,] <- data.frame(c1="annual", c2="annual_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[2]][2,] <- data.frame(c1=0, c2="number_non_critical_annual_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[3]][1,] <- data.frame(c1="monthly", c2="monthly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[3]][2,] <- data.frame(c1=0, c2="number_non_critical_monthly_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[4]][1,] <- data.frame(c1="ws_b204/clim/Grove_lowprov_clim_1942_2453", c2="daily_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[4]][2,] <- data.frame(c1=0, c2="number_non_critical_daily_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[5]][1,] <- data.frame(c1="hourly", c2="hourly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[5]][2,] <- data.frame(c1=0, c2="number_non_critical_hourly_sequences",stringsAsFactors=FALSE)


# Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
#input_dated_seq_list <- NULL
input_dated_seq_list = list()
input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="biomass_removal_percent",year=1941,month=10,day=3,hour=1,value=0.95,stringsAsFactors=FALSE)


# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)
#input_tec_data[3,] <- data.frame(2041, 9, 30, 1, "output_current_state", stringsAsFactors=FALSE)


# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
output_variables <- NULL
# output_variables <- list()
# output_variables[[1]] <- list("lai", "awks/output_var_bd_lai.awk","p301_simulation_basin.daily")
# output_variables[[2]] <- list("leafc", "awks/output_var_cdg_leafc.awk","p301_simulation_grow_stratum.daily")
# output_variables[[3]] <- list("stemc", "awks/output_var_cdg_stemc.awk","p301_simulation_grow_stratum.daily")
# output_variables[[4]] <- list("rootc", "awks/output_var_cdg_rootc.awk","p301_simulation_grow_stratum.daily")
# 
# output_variables[[5]] <- list("litrc", "awks/output_var_bd_litrc.awk","p301_simulation_basin.daily")
# output_variables[[6]] <- list("cwdc", "awks/output_var_cdg_cwdc.awk","p301_simulation_grow_stratum.daily")
# output_variables[[7]] <- list("soil1c", "awks/output_var_pdg_soil1c.awk","p301_simulation_grow_patch.daily")
# 
# output_variables[[8]] <- list("height", "awks/output_var_cd_height.awk","p301_simulation_stratum.daily")
# 
# output_variables[[9]] <- list("understory_leafc", "awks/output_var_bdg_understory_leafc.awk","p301_simulation_grow_basin.daily")
# output_variables[[10]] <- list("understory_stemc", "awks/output_var_bdg_understory_stemc.awk","p301_simulation_grow_basin.daily")
# output_variables[[11]] <- list("understory_biomassc", "awks/output_var_bdg_understory_biomassc.awk","p301_simulation_grow_basin.daily")
# output_variables[[12]] <- list("understory_height", "awks/output_var_bdg_understory_height.awk","p301_simulation_grow_basin.daily")
# output_variables[[13]] <- list("overstory_leafc", "awks/output_var_bdg_overstory_leafc.awk","p301_simulation_grow_basin.daily")
# output_variables[[14]] <- list("overstory_stemc", "awks/output_var_bdg_overstory_stemc.awk","p301_simulation_grow_basin.daily")
# output_variables[[15]] <- list("overstory_biomassc", "awks/output_var_bdg_overstory_biomassc.awk","p301_simulation_grow_basin.daily")
# output_variables[[16]] <- list("overstory_height", "awks/output_var_bdg_overstory_height.awk","p301_simulation_grow_basin.daily")


# ---------------------------------------------------------------------

system.time(
  run_rhessys(parameter_method = parameter_method,
              input_rhessys = input_rhessys,
              input_hdr_list = input_hdr_list,
              input_preexisting_table = input_preexisting_table,
              input_def_list = input_def_list,
              input_standard_par_list = input_standard_par_list,
              input_clim_base_list = input_clim_base_list,
              input_dated_seq_list = input_dated_seq_list,
              input_tec_data = input_tec_data,
              output_variables = output_variables)
)

beep(1)

