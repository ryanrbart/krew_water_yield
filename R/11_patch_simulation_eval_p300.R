# Patch Simulation Evaluation
#
# Contains scripts for evaluating patch simulation

source("R/0_utilities.R")

theme_set(theme_bw(base_size = 11))

# ---------------------------------------------------------------------
# P300 Patch Simulation data processing


p300_patch <- readin_rhessys_output_cal(var_names = c("precip", "streamflow"),
                                               path = RHESSYS_ALLSIM_DIR_10_P300,
                                               initial_date = ymd("1941-10-01"),
                                               parameter_file = RHESSYS_ALL_OPTION_10_P300,
                                               num_canopies = 1)

p300_patch$wy <- y_to_wy(lubridate::year(p300_patch$dates),lubridate::month(p300_patch$dates))
p300_patch_sum <- p300_patch %>%
  group_by(dated_id, lapse_rate_precip_default = ws_p300.defs.zone_p300.def.lapse_rate_precip_default, var_type) %>%
  summarize(sum_value = sum(value))

p300_patch_diff <- p300_patch_sum %>%
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
#View(p300_patch_diff)
#rm(p300_patch)


# Comparison plot
x <- filter(p300_patch_diff, var_type == "streamflow") %>%
  ggplot() +
  geom_point(aes(x=lapse_rate_precip_default,y=change, shape=as.character(thinning)), size = 1.2)
print(x)










