# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

source("R/0_utilities.R")

theme_set(theme_bw(base_size = 20))

# ---------------------------------------------------------------------
# P300 Patch Simulation data processing


p301_shed <- readin_rhessys_output_cal(var_names = c("precip", "streamflow"),
                                        path = RHESSYS_ALLSIM_DIR_21_P301,
                                        initial_date = ymd("1941-10-01"),
                                        parameter_file = RHESSYS_ALL_OPTION_21_P301,
                                        num_canopies = 1)

p301_shed$wy <- y_to_wy(lubridate::year(p301_shed$dates),lubridate::month(p301_shed$dates))
p301_shed_sum <- p301_shed %>%
  group_by(dated_id, lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default, var_type) %>%
  summarize(sum_value = sum(value))

# -----

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
p301_shed_diff <- mutate(p301_shed_diff, annual_precip = (lapse_rate_precip_default*250000)+1250)

# Comparison plot
x <- filter(p301_shed_diff, var_type == "streamflow") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=change, shape=as.character(thinning)), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Forest\nThinned (%)", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Change in annual streamflow (mm)")
print(x)

# -----
# Percent change in streamflow

p301_shed_diff <- p301_shed_sum %>%
  tidyr::spread(dated_id, sum_value) %>%
  rename(biomass100='1', biomass90='2', biomass80='3', biomass70='4', biomass60='5',
         biomass50='6', biomass40='7', biomass30='8', biomass20='9', biomass10='10') %>% 
  dplyr::mutate(thin10 = (biomass90 / biomass100) * 100 -100,
                thin20 = (biomass80 / biomass100) * 100 -100,
                thin30 = (biomass70 / biomass100) * 100 -100,
                thin40 = (biomass60 / biomass100) * 100 -100,
                thin50 = (biomass50 / biomass100) * 100 -100,
                thin60 = (biomass40 / biomass100) * 100 -100,
                thin70 = (biomass30 / biomass100) * 100 -100,
                thin80 = (biomass20 / biomass100) * 100 -100,
                thin90 = (biomass10 / biomass100) * 100 -100) %>% 
  tidyr::gather(key="thinning", value="change", thin10, thin20, thin30, thin40, thin50, 
                thin60, thin70, thin80, thin90)
#View(p301_shed_diff)
#rm(p300_patch)

# Generate a easy display, mostly realistic, conversion for lapse_rate_precip_default
p301_shed_diff <- mutate(p301_shed_diff, annual_precip = (lapse_rate_precip_default*250000)+1250)

# Comparison plot
x <- filter(p301_shed_diff, var_type == "streamflow") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=change, shape=as.character(thinning)), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Forest\nThinned (%)", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(x = "Annual Precipitation (mm)", y = "Change in annual streamflow (%)")
print(x)





