# Misc tasks


# ---------------------------------------------------------------------
# Look at actual precipitation values

p301_shed <- readin_rhessys_output_cal(var_names = c("precip"),
                                       path = RHESSYS_ALLSIM_DIR_21_P301_1,
                                       initial_date = ymd("1942-10-01"),
                                       parameter_file = RHESSYS_ALL_OPTION_21_P301_1,
                                       num_canopies = 1)

p301_shed$wy <- y_to_wy(lubridate::year(p301_shed$dates),lubridate::month(p301_shed$dates))
p301_shed_sum <- p301_shed %>%
  group_by(dated_id, lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default, var_type, wy) %>%
  summarize(sum_value = sum(value))


dplyr::filter(p301_shed_sum, wy==1943)

diff(dplyr::filter(p301_shed_sum, wy==1943)$sum_value)

# difference is 296.13

0.001/296.13
296.13/0.001

# ---------------------------------------------------------------------
# Compare hydrograph between different lapse years


ls(p301_shed)

happy <- p301_shed %>% 
  dplyr::select(dates, 
                wy, 
                dated_id,
                lapse_rate_precip_default = ws_p301.defs.zone_p301.def.lapse_rate_precip_default,
                var_type,
                value) %>% 
  dplyr::filter(var_type == 'precip', dated_id == 1, wy==1943)


ggplot(data=happy) +
  geom_line(aes(x=dates, y=value)) + 
  facet_wrap(.~lapse_rate_precip_default)


# ---------------------------------------------------------------------
# Calculate the appropriate values of lapse_rate_precip_default

# -0.005 277
# -0.004 581
# -0.003 1587
# -0.002 2771
# -0.001 3955


1587-581
2771-1587
3955-2771




