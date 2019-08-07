# Combine effect size from Q and NDVI and make figure

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

out_ndvi_dummy_draws <- read_rds("output/3.8/out_ndvi_dummy_draws.rds")
out_treated_dummy_draws <- read_rds("output/2.4_mixed_model_analysis/out_treated_dummy_draws.rds")

# ---------------------------------------------------------------------
# Combine the data

# Remove non-matching columns and change name of response variable
out_ndvi_dummy_draws_tmp <- out_ndvi_dummy_draws %>% 
  ungroup() %>% 
  dplyr::select(response_variable, treatment_effect_size_percent) %>% 
  dplyr::mutate(response_variable = factor(7))

# Remove non-matching columns
out_treated_dummy_draws_tmp <- out_treated_dummy_draws %>% 
  dplyr::select(response_variable, treatment_effect_size_percent)

# Combine
out_dummy_draws <- out_ndvi_dummy_draws_tmp %>% 
  bind_rows(out_treated_dummy_draws_tmp)

# Refactor the response variable column
out_dummy_draws$response_variable <- factor(out_dummy_draws$response_variable,
                                            levels = c(6,5,4,3,2,1,7))

# ---------------------------------------------------------------------
# Plot mixed modeling effect size output

response_variable_id <- c(
  `7` = "NDVI", `1` = "Q95", `2` = "Oct-Dec",
  `3` = "Jan-Mar",`4` = "Apr-Jun",
  `5` = "Jul-Sep", `6` = "Annual"
)


# Plot uncertainty intervals by parameter (treatment variable)
# Higher values indicate a post-fire increase, lower values a post-fire decrease 
plot_dummy_draws <- out_dummy_draws %>% 
  ggplot(data=.) +
  tidybayes::geom_halfeyeh(aes(y = response_variable, x = treatment_effect_size_percent), color="black", fill="gray55") +
  geom_vline(xintercept = 0, col="red") +
  scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = "Estimated Change in Response Due to Fire (%)" , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/4.2/plot_dummy_draws_effect_size.jpg",plot=plot_dummy_draws, width = 5, height = 3)


# This plot has issues. First, I need to get the distribution back for
# streamflow. Unclear why the distribution is generated for ndvi but not others.
# Maybe due to the different number of samples? Second, the 'effect' of
# treatment on NDVI is relatively small since the treatment includes many years
# after treatment. This could be confusing to show or it may be useful?


