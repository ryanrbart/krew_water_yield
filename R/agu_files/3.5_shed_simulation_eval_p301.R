# Simulation Evaluation
#
# Contains scripts for evaluating VALUE of response variables

source("R/0_utilities.R")

theme_set(theme_bw(base_size = 12))

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Process differences in variables - Fluxes


process_shed_diff <- function(path, parameter_file){
  
  p301_shed <- readin_rhessys_output_cal(var_names = c("precip", "streamflow", "et", "trans", "evap"),
                                         path = path,
                                         initial_date = ymd("1942-10-01"),
                                         parameter_file = parameter_file,
                                         num_canopies = 1)
  
  p301_shed$wy <- as.character(y_to_wy(lubridate::year(p301_shed$dates),lubridate::month(p301_shed$dates)))
  p301_shed_sum <- p301_shed %>%
    group_by(dated_id, lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default, var_type, wy) %>%
    summarize(sum_value = sum(value))
  
  p301_shed_diff <- p301_shed_sum %>%
    tidyr::spread(dated_id, sum_value) %>%
    rename(biomass100='1', biomass90='2', biomass80='3', biomass70='4', biomass60='5',
           biomass50='6', biomass40='7', biomass30='8', biomass20='9', biomass10='10') %>% 
    dplyr::mutate(thin10 = biomass90 - biomass100,
                  thin20 = biomass80 - biomass100,
                  thin30 = biomass70 - biomass100,
                  thin40 = biomass60 - biomass100,
                  thin50 = biomass50 - biomass100,
                  thin60 = biomass40 - biomass100,
                  thin70 = biomass30 - biomass100,
                  thin80 = biomass20 - biomass100,
                  thin90 = biomass10 - biomass100) %>% 
    tidyr::gather(key="thinning", value="change", thin10, thin20, thin30, thin40, thin50, 
                  thin60, thin70, thin80, thin90)
  #View(p301_shed_diff)
  #rm(p300_patch)
  
  # Generate an easy display, mostly realistic, conversion for lapse_rate_precip_default
  #p301_shed_diff <- mutate(p301_shed_diff, annual_precip = (lapse_rate_precip_default*250000)+1250)
  p301_shed_diff <- mutate(p301_shed_diff, annual_precip = case_when(lapse_rate_precip_default==-0.00352 ~ 250,
                                                                     lapse_rate_precip_default==-0.00265 ~ 500,
                                                                     lapse_rate_precip_default==-0.00181 ~ 750,
                                                                     lapse_rate_precip_default==-0.00097 ~ 1000,
                                                                     lapse_rate_precip_default==-0.00013 ~ 1250,
                                                                     lapse_rate_precip_default==0.00071 ~ 1500,
                                                                     lapse_rate_precip_default==0.00155 ~ 1750,
                                                                     lapse_rate_precip_default==0.00239 ~ 2000,
                                                                     lapse_rate_precip_default==0.00323 ~ 2250,
                                                                     lapse_rate_precip_default==0.00407 ~ 2500,
                                                                     lapse_rate_precip_default==0.00491 ~ 2750,
                                                                     lapse_rate_precip_default==0.00575 ~ 3000
  ))
  
  return(p301_shed_diff)
}

# ----
# 1m soil
p301_shed_diff_1m <- process_shed_diff(RHESSYS_ALLSIM_DIR_21_P301_1,
                                       RHESSYS_ALL_OPTION_21_P301_1)

# 3m soil
p301_shed_diff_3m <- process_shed_diff(RHESSYS_ALLSIM_DIR_21_P301_2,
                                       RHESSYS_ALL_OPTION_21_P301_2)

# 5m soil
p301_shed_diff_5m <- process_shed_diff(RHESSYS_ALLSIM_DIR_21_P301_3,
                                       RHESSYS_ALL_OPTION_21_P301_3)

p301_shed_diff <- dplyr::bind_rows(p301_shed_diff_1m,
                                   p301_shed_diff_3m,
                                   p301_shed_diff_5m,
                                   .id = "soil_depth")

soil_depth_id <- c(
  `1` = "Soil depth-1m", `2` = "Soil depth - 3m", `3` = "Soil depth - 5m"
)
post_fire_year_id <- c(
  `1943` = "1st year",
  `1944` = "2nd year",
  `1945` = "3rd year",
  `1946` = "4th year" 
)

# ---------------------------------------------------------------------
# P301 Streamflow

# Comparison plot
x <- filter(p301_shed_diff, var_type == "streamflow") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.3) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Annual Streamflow (mm)") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_q2.jpg", plot=x, width = 6, height = 4)

# ---------------------------------------------------------------------
# P301 Basin ET

# Comparison plot
x <- filter(p301_shed_diff, var_type == "et") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Annual ET (mm)") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_et2.jpg", plot=x, width = 6, height = 4)


# ---------------------------------------------------------------------
# P301 Basin Transpiration

# Comparison plot
x <- filter(p301_shed_diff, var_type == "trans") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Annual Transpiration (mm)") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_trans2.jpg", plot=x, width = 6, height = 4)


# ---------------------------------------------------------------------
# P301 Basin Evaporation

# Comparison plot
x <- filter(p301_shed_diff, var_type == "evap") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Annual Evaporation (mm)") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_evap2.jpg", plot=x, width = 6, height = 4)







# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Process differences in variables - Storage


process_shed_diff_avg <- function(path, parameter_file){
  
  p301_shed <- readin_rhessys_output_cal(var_names = c("rz_storage", "sat_def", "unsat_stor"),
                                         path = path,
                                         initial_date = ymd("1942-10-01"),
                                         parameter_file = parameter_file,
                                         num_canopies = 1)
  
  p301_shed$wy <- as.character(y_to_wy(lubridate::year(p301_shed$dates),lubridate::month(p301_shed$dates)))
  p301_shed_sum <- p301_shed %>%
    group_by(dated_id, lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default, var_type, wy) %>%
    summarize(sum_value = mean(value))
  
  p301_shed_diff <- p301_shed_sum %>%
    tidyr::spread(dated_id, sum_value) %>%
    rename(biomass100='1', biomass90='2', biomass80='3', biomass70='4', biomass60='5',
           biomass50='6', biomass40='7', biomass30='8', biomass20='9', biomass10='10') %>% 
    dplyr::mutate(thin10 = biomass90 - biomass100,
                  thin20 = biomass80 - biomass100,
                  thin30 = biomass70 - biomass100,
                  thin40 = biomass60 - biomass100,
                  thin50 = biomass50 - biomass100,
                  thin60 = biomass40 - biomass100,
                  thin70 = biomass30 - biomass100,
                  thin80 = biomass20 - biomass100,
                  thin90 = biomass10 - biomass100) %>% 
    tidyr::gather(key="thinning", value="change", thin10, thin20, thin30, thin40, thin50, 
                  thin60, thin70, thin80, thin90)
  #View(p301_shed_diff)
  #rm(p300_patch)
  
  # Generate a easy display, mostly realistic, conversion for lapse_rate_precip_default
  #p301_shed_diff <- mutate(p301_shed_diff, annual_precip = (lapse_rate_precip_default*250000)+1250)
  p301_shed_diff <- mutate(p301_shed_diff, annual_precip = case_when(lapse_rate_precip_default==-0.00352 ~ 250,
                                                                     lapse_rate_precip_default==-0.00265 ~ 500,
                                                                     lapse_rate_precip_default==-0.00181 ~ 750,
                                                                     lapse_rate_precip_default==-0.00097 ~ 1000,
                                                                     lapse_rate_precip_default==-0.00013 ~ 1250,
                                                                     lapse_rate_precip_default==0.00071 ~ 1500,
                                                                     lapse_rate_precip_default==0.00155 ~ 1750,
                                                                     lapse_rate_precip_default==0.00239 ~ 2000,
                                                                     lapse_rate_precip_default==0.00323 ~ 2250,
                                                                     lapse_rate_precip_default==0.00407 ~ 2500,
                                                                     lapse_rate_precip_default==0.00491 ~ 2750,
                                                                     lapse_rate_precip_default==0.00575 ~ 3000
  ))
  
  return(p301_shed_diff)
}

# ----
# 1m soil
p301_shed_diff_1m <- process_shed_diff_avg(RHESSYS_ALLSIM_DIR_21_P301_1,
                                           RHESSYS_ALL_OPTION_21_P301_1)

# 3m soil
p301_shed_diff_3m <- process_shed_diff_avg(RHESSYS_ALLSIM_DIR_21_P301_2,
                                           RHESSYS_ALL_OPTION_21_P301_2)

# 5m soil
p301_shed_diff_5m <- process_shed_diff_avg(RHESSYS_ALLSIM_DIR_21_P301_3,
                                           RHESSYS_ALL_OPTION_21_P301_3)

p301_shed_diff_avg <- dplyr::bind_rows(p301_shed_diff_1m,
                                       p301_shed_diff_3m,
                                       p301_shed_diff_5m,
                                       .id = "soil_depth")

soil_depth_id <- c(
  `1` = "Soil depth-1m", `2` = "Soil depth - 3m", `3` = "Soil depth - 5m"
)
post_fire_year_id <- c(
  `1943` = "1st year",
  `1944` = "2nd year",
  `1945` = "3rd year",
  `1946` = "4th year"
)

# ---------------------------------------------------------------------
# P301 RZ Storage

# Comparison plot
x <- filter(p301_shed_diff_avg, var_type == "rz_storage") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Annual RZ Storage (mm)") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_rz_storage2.jpg", plot=x, width = 6, height = 4)

# ---------------------------------------------------------------------
# P301 Sat Def

# Comparison plot
x <- filter(p301_shed_diff_avg, var_type == "sat_def") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Saturated Deficit (mm)") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_sat_def2.jpg", plot=x, width = 6, height = 4)


# ---------------------------------------------------------------------
# P301 Unsat Storage

# Comparison plot
x <- filter(p301_shed_diff_avg, var_type == "unsat_stor") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Unsaturated Storage (mm)") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_unsat_stor2.jpg", plot=x, width = 6, height = 4)







# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Process differences in variables - Height


process_shed_diff <- function(path, parameter_file){
  
  p301_shed <- readin_rhessys_output_cal(var_names = c("height"),
                                         path = path,
                                         initial_date = ymd("1942-10-01"),
                                         parameter_file = parameter_file,
                                         num_canopies = 2)
  
  p301_shed$wy <- as.character(y_to_wy(lubridate::year(p301_shed$dates),lubridate::month(p301_shed$dates)))
  p301_shed_sum <- p301_shed %>%
    group_by(dated_id, lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default, var_type, wy, canopy_layer) %>%
    summarize(sum_value = mean(value))
  
  p301_shed_diff <- p301_shed_sum %>%
    tidyr::spread(dated_id, sum_value) %>%
    rename(biomass100='1', biomass90='2', biomass80='3', biomass70='4', biomass60='5',
           biomass50='6', biomass40='7', biomass30='8', biomass20='9', biomass10='10') %>% 
    dplyr::mutate(thin10 = biomass90 - biomass100,
                  thin20 = biomass80 - biomass100,
                  thin30 = biomass70 - biomass100,
                  thin40 = biomass60 - biomass100,
                  thin50 = biomass50 - biomass100,
                  thin60 = biomass40 - biomass100,
                  thin70 = biomass30 - biomass100,
                  thin80 = biomass20 - biomass100,
                  thin90 = biomass10 - biomass100) %>% 
    tidyr::gather(key="thinning", value="change", thin10, thin20, thin30, thin40, thin50, 
                  thin60, thin70, thin80, thin90)
  #View(p301_shed_diff)
  #rm(p300_patch)
  
  # Generate a easy display, mostly realistic, conversion for lapse_rate_precip_default
  #p301_shed_diff <- mutate(p301_shed_diff, annual_precip = (lapse_rate_precip_default*250000)+1250)
  p301_shed_diff <- mutate(p301_shed_diff, annual_precip = case_when(lapse_rate_precip_default==-0.00352 ~ 250,
                                                                     lapse_rate_precip_default==-0.00265 ~ 500,
                                                                     lapse_rate_precip_default==-0.00181 ~ 750,
                                                                     lapse_rate_precip_default==-0.00097 ~ 1000,
                                                                     lapse_rate_precip_default==-0.00013 ~ 1250,
                                                                     lapse_rate_precip_default==0.00071 ~ 1500,
                                                                     lapse_rate_precip_default==0.00155 ~ 1750,
                                                                     lapse_rate_precip_default==0.00239 ~ 2000,
                                                                     lapse_rate_precip_default==0.00323 ~ 2250,
                                                                     lapse_rate_precip_default==0.00407 ~ 2500,
                                                                     lapse_rate_precip_default==0.00491 ~ 2750,
                                                                     lapse_rate_precip_default==0.00575 ~ 3000
  ))
  
  return(p301_shed_diff)
}

# ----
# 1m soil
p301_shed_diff_1m <- process_shed_diff(path = RHESSYS_ALLSIM_DIR_21_P301_1,
                                       parameter_file = RHESSYS_ALL_OPTION_21_P301_1)

# 3m soil
p301_shed_diff_3m <- process_shed_diff(RHESSYS_ALLSIM_DIR_21_P301_2,
                                       RHESSYS_ALL_OPTION_21_P301_2)

# 5m soil
p301_shed_diff_5m <- process_shed_diff(RHESSYS_ALLSIM_DIR_21_P301_3,
                                       RHESSYS_ALL_OPTION_21_P301_3)

p301_shed_diff <- dplyr::bind_rows(p301_shed_diff_1m,
                                   p301_shed_diff_3m,
                                   p301_shed_diff_5m,
                                   .id = "soil_depth")

soil_depth_id <- c(
  `1` = "Soil depth-1m", `2` = "Soil depth - 3m", `3` = "Soil depth - 5m"
)
post_fire_year_id <- c(
  `1943` = "1st year",
  `1944` = "2nd year",
  `1945` = "3rd year",
  `1946` = "4th year" 
)

# ---------------------------------------------------------------------
# P301 Height

# Comparison plot
x <- filter(p301_shed_diff, var_type == "height") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.3) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Height") +
  facet_grid(wy~soil_depth, labeller = labeller(.rows=post_fire_year_id, .cols=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_height.jpg", plot=x, width = 6, height = 4)


















