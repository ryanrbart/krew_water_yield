
# Run top of 3.4 prior to this code

# Comparison plot
x <- filter(p301_shed_diff, var_type == "streamflow", wy=="1943") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=change, shape=as.character(thinning)), size = 1.3) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(title = "Post-Treatment Change in Annual Streamflow", x = "Annual Precipitation (mm)", y = "Change in Annual Streamflow (mm)") +
  facet_grid(soil_depth~., labeller = labeller(.rows=soil_depth_id)) +
  xlim(0,3000) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_q_year1.jpg", plot=x, width = 6, height = 4)


# Comparison plot
x <- filter(p301_shed_diff, var_type == "trans", wy=="1943") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=change, shape=as.character(thinning)), size = 1.3) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(title = "Post-Treatment Change in Annual Transpiration", x = "Annual Precipitation (mm)", y = "Change in Annual Transpiration (mm)") +
  facet_grid(soil_depth~., labeller = labeller(.rows=soil_depth_id)) +
  xlim(0,3000) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_trans_year1.jpg", plot=x, width = 6, height = 4)



# Comparison plot
x <- filter(p301_shed_diff, var_type == "streamflow", wy=="1943") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.3) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(title = "Post-Treatment Annual Streamflow", x = "Annual Precipitation (mm)", y = "Annual Streamflow (mm)") +
  facet_grid(soil_depth~., labeller = labeller(.rows=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_q2_year1.jpg", plot=x, width = 6, height = 4)



# Comparison plot
x <- filter(p301_shed_diff, var_type == "trans", wy=="1943") %>%
  ggplot() +
  geom_point(aes(x=annual_precip,y=biomass100), size = 1.5) +
  scale_shape_manual(values=seq(1,9), name="Percent\nForest\nThinned", labels=c(10,20,30,40,50,60,70,80,90)) +
  labs(title = "Post-Treatment Annual transpiration", x = "Annual Precipitation (mm)", y = "Annual Transpiration (mm)") +
  facet_grid(soil_depth~., labeller = labeller(.rows=soil_depth_id)) +
  theme_bw() +
  NULL
print(x)
ggsave("output/icrw_branch/precip_vs_change_in_trans2_year1.jpg", plot=x, width = 6, height = 4)

