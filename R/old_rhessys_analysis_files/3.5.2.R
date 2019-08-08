# Watershed Simulation for P301
# 
# Repeats 1943 (Uses different clim than normal)


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Run the model from 3.5.1


# Soil depth 1m

# Change worldfile (spun-up with appropriate soil depth)
input_rhessys$world_file <- "ws_p301/worldfiles/p301_icrw.world.Y2041M9D30H1.state"

# Change soil depth
input_def_list[[12]][[3]] <- 1

# Change output file name
input_rhessys$output_folder <- "ws_p301/out/3.5.1_p301_soil1"


system.time(
  run_rhessys(parameter_method = parameter_method,
              output_method="awk",
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


# -----
# Soil depth 3m

# Change worldfile (spun-up with appropriate soil depth)
input_rhessys$world_file <- "ws_p301/worldfiles/p301_icrw.world.Y2041M9D30H2.state"

# Change soil depth
input_def_list[[12]][[3]] <- 3

# Change output file name
input_rhessys$output_folder <- "ws_p301/out/3.5.1_p301_soil3"


system.time(
  run_rhessys(parameter_method = parameter_method,
              output_method="awk",
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


# -----
# Soil depth 5m

# Change worldfile (spun-up with appropriate soil depth)
input_rhessys$world_file <- "ws_p301/worldfiles/p301_icrw.world.Y2041M9D30H3.state"

# Change soil depth
input_def_list[[12]][[3]] <- 5

# Change output file name
input_rhessys$output_folder <- "ws_p301/out/3.5.1_p301_soil5"


system.time(
  run_rhessys(parameter_method = parameter_method,
              output_method="awk",
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


