# KREW mixed-model analysis

# With rstanarm, and tidybayes


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

# Mixed model input data
pair_mam7 <- read_rds(PAIR_MAM7_RDS)
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_monthly <- read_rds(PAIR_MONTHLY_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)


# Import mixed model results
out_q_ndvi_var <- read_rds(Q_NDVI_VAR_RDS)
out_q_ndvi_ratio <- read_rds(Q_NDVI_RATIO_RDS)

out_treated_dummy <- read_rds(TREATED_DUMMY_RDS)
out_thinning_dummy <- read_rds(THINNING_DUMMY_RDS)
out_prescribed_fire_dummy <- read_rds(PRESCRIBED_FIRE_DUMMY_RDS)
out_treated_dummy_int <- read_rds(TREATED_DUMMY_INT_RDS)

# ---------------------------------------------------------------------
# Process mixed modeling output

# ----
# NDVI: Variables

# Extract the median (and intervals) for each parameter.
out_q_ndvi_var_median <- purrr::map(out_q_ndvi_var, function(x) x %>%
                                         tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_treated, ndvi_control) %>% 
                                         tidybayes::median_qi()
)
out_q_ndvi_var_median <- bind_rows(out_q_ndvi_var_median, .id="response_variable")


# Generate the draws for each parameter
out_q_ndvi_var_draws <- purrr::map(out_q_ndvi_var, function(x) x %>%
                                        tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_treated, ndvi_control)
)
out_q_ndvi_var_draws <- bind_rows(out_q_ndvi_var_draws, .id="response_variable")

out_q_ndvi_var_draws$response_variable <- factor(out_q_ndvi_var_draws$response_variable,
                                                    levels = c(6,5,4,3,2,1))
out_q_ndvi_var_draws$.chain <- factor(out_q_ndvi_var_draws$.chain)
out_q_ndvi_var_draws$.iteration <- factor(out_q_ndvi_var_draws$.iteration)
out_q_ndvi_var_draws$.draw <- factor(out_q_ndvi_var_draws$.draw)


# ----
# NDVI: Ratio

# Extract the median (and intervals) for each parameter.
out_q_ndvi_ratio_median <- purrr::map(out_q_ndvi_ratio, function(x) x %>%
                                         tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_ratio) %>% 
                                         tidybayes::median_qi()
)
out_q_ndvi_ratio_median <- bind_rows(out_q_ndvi_ratio_median, .id="response_variable")


# Generate the draws for each parameter
out_q_ndvi_ratio_draws <- purrr::map(out_q_ndvi_ratio, function(x) x %>%
                                        tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_ratio)
)
out_q_ndvi_ratio_draws <- bind_rows(out_q_ndvi_ratio_draws, .id="response_variable")

out_q_ndvi_ratio_draws$response_variable <- factor(out_q_ndvi_ratio_draws$response_variable,
                                                    levels = c(6,5,4,3,2,1))
out_q_ndvi_ratio_draws$.chain <- factor(out_q_ndvi_ratio_draws$.chain)
out_q_ndvi_ratio_draws$.iteration <- factor(out_q_ndvi_ratio_draws$.iteration)
out_q_ndvi_ratio_draws$.draw <- factor(out_q_ndvi_ratio_draws$.draw)


# ----
# Dummy: Treated

# Extract the median (and intervals) for each parameter.
out_treated_dummy_median <- purrr::map(out_treated_dummy, function(x) x %>%
                                         tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, treatment_dummy1) %>% 
                                         tidybayes::median_qi()
)
out_treated_dummy_median <- bind_rows(out_treated_dummy_median, .id="response_variable")


# Generate the draws for each parameter
out_treated_dummy_draws <- purrr::map(out_treated_dummy, function(x) x %>%
                                        tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, treatment_dummy1)
)
out_treated_dummy_draws <- bind_rows(out_treated_dummy_draws, .id="response_variable")

out_treated_dummy_draws$response_variable <- factor(out_treated_dummy_draws$response_variable,
                                                    levels = c(6,5,4,3,2,1))
out_treated_dummy_draws$.chain <- factor(out_treated_dummy_draws$.chain)
out_treated_dummy_draws$.iteration <- factor(out_treated_dummy_draws$.iteration)
out_treated_dummy_draws$.draw <- factor(out_treated_dummy_draws$.draw)


# ----
# Dummy: Thinning

# Extract the median (and intervals) for each parameter.
out_thinning_dummy_median <- purrr::map(out_thinning_dummy, function(x) x %>%
                                          tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, thinning_dummy1) %>% 
                                          tidybayes::median_qi()
)
out_thinning_dummy_median <- bind_rows(out_thinning_dummy_median, .id="response_variable")


# Generate the draws for each parameter
out_thinning_dummy_draws <- purrr::map(out_thinning_dummy, function(x) x %>%
                                         tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, thinning_dummy1)
)
out_thinning_dummy_draws <- bind_rows(out_thinning_dummy_draws, .id="response_variable")

out_thinning_dummy_draws$response_variable <- factor(out_thinning_dummy_draws$response_variable,
                                                     levels = c(6,5,4,3,2,1))
out_thinning_dummy_draws$.chain <- factor(out_thinning_dummy_draws$.chain)
out_thinning_dummy_draws$.iteration <- factor(out_thinning_dummy_draws$.iteration)
out_thinning_dummy_draws$.draw <- factor(out_thinning_dummy_draws$.draw)


# ----
# Dummy: Prescribed Fire

# Extract the median (and intervals) for each parameter.
out_prescribed_fire_dummy_median <- purrr::map(out_prescribed_fire_dummy, function(x) x %>%
                                                 tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, prescribed_fire_dummy1) %>% 
                                                 tidybayes::median_qi()
)
out_prescribed_fire_dummy_median <- bind_rows(out_prescribed_fire_dummy_median, .id="response_variable")


# Generate the draws for each parameter
out_prescribed_fire_dummy_draws <- purrr::map(out_prescribed_fire_dummy, function(x) x %>%
                                                tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, prescribed_fire_dummy1)
)
out_prescribed_fire_dummy_draws <- bind_rows(out_prescribed_fire_dummy_draws, .id="response_variable")

out_prescribed_fire_dummy_draws$response_variable <- factor(out_prescribed_fire_dummy_draws$response_variable,
                                                            levels = c(6,5,4,3,2,1))
out_prescribed_fire_dummy_draws$.chain <- factor(out_prescribed_fire_dummy_draws$.chain)
out_prescribed_fire_dummy_draws$.iteration <- factor(out_prescribed_fire_dummy_draws$.iteration)
out_prescribed_fire_dummy_draws$.draw <- factor(out_prescribed_fire_dummy_draws$.draw)



# ----
# Dummy: Treated Interaction

# Extract the median (and intervals) for each parameter.
out_treated_dummy_int_median <- purrr::map(out_treated_dummy_int, function(x) x %>%
                                             tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, treatment_dummy1, `log(q_control):treatment_dummy1`) %>% 
                                             tidybayes::median_qi()
)
out_treated_dummy_int_median <- bind_rows(out_treated_dummy_int_median, .id="response_variable")


# Generate the draws for each parameter
out_treated_dummy_int_draws <- purrr::map(out_treated_dummy_int, function(x) x %>%
                                            tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, treatment_dummy1, `log(q_control):treatment_dummy1`)
)
out_treated_dummy_int_draws <- bind_rows(out_treated_dummy_int_draws, .id="response_variable")

out_treated_dummy_int_draws$response_variable <- factor(out_treated_dummy_int_draws$response_variable,
                                                    levels = c(6,5,4,3,2,1))
out_treated_dummy_int_draws$.chain <- factor(out_treated_dummy_draws$.chain)
out_treated_dummy_int_draws$.iteration <- factor(out_treated_dummy_int_draws$.iteration)
out_treated_dummy_int_draws$.draw <- factor(out_treated_dummy_int_draws$.draw)



# ---------------------------------------------------------------------
# Modeling Checks

# What variables are there?
purrr::map(out_treated_dummy, tidybayes::get_variables)
purrr::map(out_thinning_dummy, tidybayes::get_variables)
purrr::map(out_prescribed_fire_dummy, tidybayes::get_variables)
purrr::map(out_treated_dummy_int, tidybayes::get_variables)


# Plot chains through time
out_treated_dummy_draws %>%      
  ggplot() +
  geom_line(aes(y = treatment_dummy1, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
out_thinning_dummy_draws %>%      
  ggplot() +
  geom_line(aes(y = thinning_dummy1, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
out_prescribed_fire_dummy_draws %>%      
  ggplot() +
  geom_line(aes(y = prescribed_fire_dummy1, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
# Interaction Model: treatment variable
out_treated_dummy_int_draws %>%      
  ggplot() +
  geom_line(aes(y = treatment_dummy1, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
# Interaction Model: Interaction variable
out_treated_dummy_int_draws %>%      
  ggplot() +
  geom_line(aes(y = `log(q_control):treatment_dummy1`, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL

# Evaluate chains
# shinystan::launch_shinystan(out_treated_dummy[[1]])
# shinystan::launch_shinystan(out_thinning_dummy[[1]])
# shinystan::launch_shinystan(out_prescribed_fire_dummy[[1]])


# Possibly helpful functions
rstantools::prior_summary(out_treated_dummy[[1]])
rstantools::posterior_interval(out_treated_dummy[[1]])
rstantools::posterior_predict(out_treated_dummy[[1]])


# ---------------------------------------------------------------------
# What is the effect size?

q_estimates <- purrr::map(out_treated_dummy, broom::tidy)
bb <- 1000

# Cycle through the 6 outputs
for (aa in seq(1,6)){
  q_percent <- (exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 1) / 
    exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 0))*100 - 100
  
  q_diff <- exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 1) - 
    exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 0)

  print(q_percent)
  print(q_diff)
  print("---------------")
}

# ---------------------------------------------------------------------
# Change draws to effect size

# 
# out_q_ndvi_var_draws <- out_q_ndvi_var_draws %>% 
#   dplyr::group_by(response_variable) %>% 
#   dplyr::mutate(treatment_effect_size_percent = (exp(treatment_dummy1 * 1) /  exp(treatment_dummy1 * 0))*100 - 100)

# For a 0.05 point reduction in NDVI
out_q_ndvi_ratio_draws <- out_q_ndvi_ratio_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size_percent = (exp(ndvi_ratio * 0.5) /  exp(ndvi_ratio * 0.7))*100 - 100)

ndvi_c=0.7
out_q_ndvi_ratio_draws <- out_q_ndvi_ratio_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size_percent = (exp(ndvi_ratio * (ndvi_c*0.5)) /  exp(ndvi_ratio * ndvi_c))*100 - 100)

# Difference
out_q_ndvi_ratio_draws <- out_q_ndvi_ratio_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size_percent = (exp(ndvi_ratio * (0.1)) /  exp(ndvi_ratio * 0))*100 - 100)


out_treated_dummy_draws <- out_treated_dummy_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size_percent = (exp(treatment_dummy1 * 1) /  exp(treatment_dummy1 * 0))*100 - 100)



# ---------------------------------------------------------------------
# Plot mixed modeling output

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",`4` = "Apr-Jun",
  `5` = "Jul-Sep",`6` = "Annual"
)


# Plot uncertainty intervals by parameter (treatment variable)
# Higher values indicate a post-fire increase, lower values a post-fire decrease 
plot_treated_dummy <- out_treated_dummy_draws %>%      
  ggplot(data=., aes(y = response_variable, x = treatment_dummy1)) +
  tidybayes::geom_halfeyeh() +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(title = "Treatment Variable", x = expression('Coefficient ('*beta*')'), y = "Response Variable") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_treated_dummy.jpg",plot=plot_treated_dummy, width = 4, height = 4)



# Plot uncertainty intervals by parameter (thinning variable)
# Higher values indicate a post-fire increase, lower values a post-fire decrease 
plot_thinning_dummy <- out_thinning_dummy_draws %>%      
  ggplot(data=., aes(y = response_variable, x = thinning_dummy1)) +
  tidybayes::geom_halfeyeh() +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(title = "Thinning Variable", x = expression('Coefficient ('*beta*')'), y = "Response Variable") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_thinning_dummy.jpg",plot=plot_thinning_dummy, width = 4, height = 4)



# Plot uncertainty intervals by parameter (treatment variable)
# Higher values indicate a post-fire increase, lower values a post-fire decrease 
plot_prescribed_fire_dummy <- out_prescribed_fire_dummy_draws %>%      
  ggplot(data=., aes(y = response_variable, x = prescribed_fire_dummy1)) +
  tidybayes::geom_halfeyeh() +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(title = "Prescribed Fire Variable", x = expression('Coefficient ('*beta*')'), y = "Response Variable") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_prescribed_fire_dummy.jpg",plot=plot_prescribed_fire_dummy, width = 4, height = 4)


# Plot uncertainty intervals by parameter (treatment variable)
# Higher values indicate a post-fire increase, lower values a post-fire decrease 
plot_treated_dummy_int <- out_treated_dummy_int_draws %>%      
  ggplot(data=., aes(y = response_variable, x = `log(q_control):treatment_dummy1`)) +
  tidybayes::geom_halfeyeh() +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(title = "Treatment Variable", x = expression('Coefficient ('*beta*')'), y = "Response Variable") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_treated_dummy_int.jpg",plot=plot_treated_dummy_int, width = 4, height = 4)



# Cowplot
plot_dummy <- cowplot::plot_grid(plot_treated_dummy,
                                 plot_thinning_dummy,
                                 plot_prescribed_fire_dummy,
                                 labels=c("a","b","c"),
                                 nrow=2,
                                 label_size = 14,
                                 label_x = 0.95,
                                 label_y = 1)

cowplot::save_plot(DUMMY_PDF,
                   plot_dummy,
                   ncol=2,
                   nrow=2,
                   base_height=4,
                   base_width=4)


# ---------------------------------------------------------------------
# Plot mixed modeling effect size output

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",`4` = "Apr-Jun",
  `5` = "Jul-Sep",`6` = "Annual"
)

# Plot uncertainty intervals by parameter (ndvi_ratio)
# Higher beta values indicate that greater flow in the treated watershed with higher relative ndvi
plot_ndvi_ratio <- out_q_ndvi_ratio_draws %>%      
  #ggplot(data=., aes(y = response_variable, x = ndvi_ratio)) +
  ggplot(data=., aes(y = response_variable, x = treatment_effect_size_percent)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  # scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = "Change in Streamflow Response Variable (%)\n given a 0.05 point reduction in NDVI" , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  #xlim(-100,100) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_ratio_effect_size.jpg",plot=plot_ndvi_ratio, width = 5, height = 3)


# Plot uncertainty intervals by parameter (treatment variable)
# Higher values indicate a post-fire increase, lower values a post-fire decrease 
plot_treated_dummy <- out_treated_dummy_draws %>%      
  ggplot(data=., aes(y = response_variable, x = treatment_effect_size_percent)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = "Estimated Change in Response Due to Fire (%)" , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_treated_dummy_effect_size.jpg",plot=plot_treated_dummy, width = 5, height = 3)



# ---------------------------------------------------------------------
# Save output

write_rds(out_treated_dummy_draws, "output/2.4_mixed_model_analysis/out_treated_dummy_draws.rds")








# ---------------------------------------------------------------------
# Extra

out_1

summary(out_1)


ci95 <- posterior_interval(out_1, prob = 0.95)
round(ci95, 2)

cbind(Median = coef(out_1), MAD_SD = se(out_1))
summary(residuals(out_1)) # not deviance residuals
cov2cor(vcov(out_1))

summary(out_1, 
        #pars = c("(Intercept)", "sigma", "Sigma[school:(Intercept),(Intercept)]"),
        probs = c(0.025, 0.975),
        digits = 2)

prior_summary(out_1)

prior(out_1)

# ---------------------------------------------------------------------
# shinystan

launch_shinystan(out_1)


y_rep <- posterior_predict(out_1)
dim(y_rep)

loo(out_1)

# ---------------------------------------------------------------------
# tidybayes



out_1 %>%
  spread_draws(condition_mean[condition]) %>%
  ggplot(aes(x = condition_mean, y = condition)) +
  geom_halfeyeh()










