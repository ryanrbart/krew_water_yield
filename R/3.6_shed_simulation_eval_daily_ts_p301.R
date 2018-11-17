# Code to look at daily timeseries of key variables


path <- RHESSYS_ALLSIM_DIR_21_P301_2
parameter_file <- RHESSYS_ALL_OPTION_21_P301_2


p301_shed <- readin_rhessys_output_cal(var_names = c("precip", "trans", "sat_def", "rz_storage", "unsat_stor"),
                                       path = path,
                                       initial_date = ymd("1942-10-01"),
                                       parameter_file = parameter_file,
                                       num_canopies = 1)

p301_shed$wy <- as.character(y_to_wy(lubridate::year(p301_shed$dates),lubridate::month(p301_shed$dates)))



p301_shed2 <- p301_shed %>%
  dplyr::select(dates, dated_id, lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default, var_type, wy, value) %>% 
  dplyr::filter(lapse_rate_precip_default == -0.00352, dated_id == 1)   # 250 mm precip
  # dplyr::filter(lapse_rate_precip_default == 0.00491, dated_id == 1)  # 2750 mm precip

#p301_shed2 <- p301_shed2 %>%
#  dplyr::filter(var_type == "trans")

ggplot(p301_shed2) +
  geom_line(aes(x=dates,y=value)) +
  facet_grid(var_type~.)



# ----------
# Two layers

path <- RHESSYS_ALLSIM_DIR_21_P301_3
parameter_file <- RHESSYS_ALL_OPTION_21_P301_3


p301_shed <- readin_rhessys_output_cal(var_names = c("height"),
                                       path = path,
                                       initial_date = ymd("1942-10-01"),
                                       parameter_file = parameter_file,
                                       num_canopies = 2)

p301_shed$wy <- as.character(y_to_wy(lubridate::year(p301_shed$dates),lubridate::month(p301_shed$dates)))



p301_shed2 <- p301_shed %>%
  dplyr::select(dates, dated_id, lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default, var_type, wy, value, canopy_layer) %>% 
  # dplyr::filter(lapse_rate_precip_default == -0.00352, dated_id == 1)
  dplyr::filter(lapse_rate_precip_default == 0.00491, dated_id == 1)


ggplot(p301_shed2) +
  geom_line(aes(x=dates,y=value, group=canopy_layer)) +
  facet_grid(var_type~.)




