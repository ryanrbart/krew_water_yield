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
out_q_ndvi_diff <- read_rds(Q_NDVI_DIFF_RDS)
out_q_ndvi_diff_int <- read_rds(Q_NDVI_DIFF_INT_RDS)

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
# NDVI: Diff

# Extract the median (and intervals) for each parameter.
out_q_ndvi_diff_median <- purrr::map(out_q_ndvi_diff, function(x) x %>%
                                       tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n) %>% 
                                       tidybayes::median_qi()
)
out_q_ndvi_diff_median <- bind_rows(out_q_ndvi_diff_median, .id="response_variable")


# Generate the draws for each parameter
out_q_ndvi_diff_draws <- purrr::map(out_q_ndvi_diff, function(x) x %>%
                                      tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n)
)
out_q_ndvi_diff_draws <- bind_rows(out_q_ndvi_diff_draws, .id="response_variable")

out_q_ndvi_diff_draws$response_variable <- factor(out_q_ndvi_diff_draws$response_variable,
                                                  levels = c(6,5,4,3,2,1))
out_q_ndvi_diff_draws$.chain <- factor(out_q_ndvi_diff_draws$.chain)
out_q_ndvi_diff_draws$.iteration <- factor(out_q_ndvi_diff_draws$.iteration)
out_q_ndvi_diff_draws$.draw <- factor(out_q_ndvi_diff_draws$.draw)


# ----
# NDVI: Diff Int

# Extract the median (and intervals) for each parameter.
out_q_ndvi_diff_int_median <- purrr::map(out_q_ndvi_diff_int, function(x) x %>%
                                       tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n, `log(q_control):ndvi_diff_n`) %>% 
                                       tidybayes::median_qi()
)
out_q_ndvi_diff_int_median <- bind_rows(out_q_ndvi_diff_int_median, .id="response_variable")


# Generate the draws for each parameter
out_q_ndvi_diff_int_draws <- purrr::map(out_q_ndvi_diff_int, function(x) x %>%
                                      tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n, `log(q_control):ndvi_diff_n`)
)
out_q_ndvi_diff_int_draws <- bind_rows(out_q_ndvi_diff_int_draws, .id="response_variable")

out_q_ndvi_diff_int_draws$response_variable <- factor(out_q_ndvi_diff_int_draws$response_variable,
                                                  levels = c(6,5,4,3,2,1))
out_q_ndvi_diff_int_draws$.chain <- factor(out_q_ndvi_diff_int_draws$.chain)
out_q_ndvi_diff_int_draws$.iteration <- factor(out_q_ndvi_diff_int_draws$.iteration)
out_q_ndvi_diff_int_draws$.draw <- factor(out_q_ndvi_diff_int_draws$.draw)


# ---------------------------------------------------------------------
# Modeling Checks

# What variables are there?
purrr::map(out_q_ndvi_var, tidybayes::get_variables)
purrr::map(out_q_ndvi_ratio, tidybayes::get_variables)
purrr::map(out_q_ndvi_diff, tidybayes::get_variables)
purrr::map(out_q_ndvi_diff_int, tidybayes::get_variables)


# Plot chains through time
out_q_ndvi_var_draws %>%      
  ggplot() +
  geom_line(aes(y = ndvi_treated, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
out_q_ndvi_ratio_draws %>%      
  ggplot() +
  geom_line(aes(y = ndvi_ratio, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
out_q_ndvi_diff_draws %>%      
  ggplot() +
  geom_line(aes(y = ndvi_diff_n, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
out_q_ndvi_diff_int_draws %>%      
  ggplot() +
  geom_line(aes(y = ndvi_diff_n, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL
out_q_ndvi_diff_int_draws %>%      
  ggplot() +
  geom_line(aes(y = `log(q_control):ndvi_diff_n`, x = .iteration, group = .chain, col=.chain)) +
  facet_grid(response_variable~.) +
  NULL


# Evaluate chains
# shinystan::launch_shinystan(out_q_ndvi_var[[1]])
# shinystan::launch_shinystan(out_q_ndvi_ratio[[1]])
# shinystan::launch_shinystan(out_q_ndvi_diff[[1]])
# shinystan::launch_shinystan(out_q_ndvi_diff_int[[1]])


# Possibly helpful functions
rstantools::prior_summary(out_q_ndvi_ratio[[1]])
rstantools::posterior_interval(out_q_ndvi_ratio[[1]])
rstantools::posterior_predict(out_q_ndvi_ratio[[1]])


# ---------------------------------------------------------------------
# What is the effect size?

# NDVI diff example
q_estimates <- purrr::map(out_q_ndvi_diff, broom::tidy)

# Cycle through the 6 outputs
for (aa in seq(1,6)){
  # This script computes the percent change (or absolute change) in Qt for a given point change in the normalized NDVI of the treated watersheds
  # If using the diff metric, this code should (approximately?) scale linearly with the given point change in normalized NDVI
  # If using the normalized metric, the percent results should be the same for different NDVI values as long as the difference is the same. The absolute value may change.
  # Similarly, if using the normalized metric, the percent results should be the same for different levels of wetness. The absolute value may change.
  
  ndvi_t <- -0.2     # Treated watershed NDVI
  ndvi_c <- 0.0     # Control watershed NDVI
  q_control <- 100
  
  q_funct3_t <- exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(q_control) + q_estimates[[aa]]$estimate[3] * ndvi_t)
  q_funct3_c <- exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(q_control) + q_estimates[[aa]]$estimate[3] * ndvi_c)  
  
  q_percent <- (q_funct3_t / q_funct3_c)*100 - 100  # Percent change
  q_diff <- (q_funct3_t - q_funct3_c)               # Absolute change
  
  print(paste("q_funct3_t: ", round(q_funct3_t,3)))
  print(paste("q_funct3_c: ", round(q_funct3_c,3)))
  print(paste("q_percent:  ", round(q_percent,3)))
  print(paste("q_diff:     ", round(q_diff,3)))
  print("---------------")
}


# ---
# NDVI diff interaction example
q_estimates <- purrr::map(out_q_ndvi_diff_int, broom::tidy)

# Cycle through the 6 outputs
for (aa in seq(1,6)){
  # This script computes the percent change (or absolute change) in Qt for a given point change in the normalized NDVI of the treated watersheds
  # If using the diff metric, this code (and the following) should scale linearly with the given point change in normalized NDVI
  # If using the normalized metric, the percent results should be the same for different NDVI values as long as the difference is the same. The absolute value may change.
  # Similarly, if using the normalized metric, the percent results should be the same for levels of wetness. The absolute value may change.
  
  ndvi_t <- 0.1     # Treated watershed NDVI
  ndvi_c <- 1     # Control watershed NDVI
  q_control <- 100
  
  q_funct4_t <- exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(q_control) + q_estimates[[aa]]$estimate[3] * ndvi_t + q_estimates[[aa]]$estimate[4] * log(q_control) * ndvi_t)
  q_funct4_c <- exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(q_control) + q_estimates[[aa]]$estimate[3] * ndvi_c + q_estimates[[aa]]$estimate[4] * log(q_control) * ndvi_c)  
  
  q_percent <- (q_funct4_t / q_funct4_c)*100 - 100
  q_diff <- (q_funct4_t - q_funct4_c)
  
  print(paste("q_funct4_t: ", round(q_funct4_t,3)))
  print(paste("q_funct4_c: ", round(q_funct4_c,3)))
  print(paste("q_percent:  ", round(q_percent,3)))
  print(paste("q_diff:     ", round(q_diff,3)))
  print("---------------")
}


# ---------------------------------------------------------------------
# Change draws to effect size

# -----
# NDVI diff

# For a fixed normalized point reduction in NDVI
out_q_ndvi_diff_draws <- out_q_ndvi_diff_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size = (exp(ndvi_diff_n * (-0.1)) /  exp(ndvi_diff_n * 0))*100 - 100)


# -----
# NDVI diff interaction

# For a fixed normalized point reduction in NDVI
out_q_ndvi_diff_int_draws <- out_q_ndvi_diff_int_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size = (exp(ndvi_diff_n * (-0.1)) /  exp(ndvi_diff_n * 0))*100 - 100)



# ---------------------------------------------------------------------
# Plot mixed modeling output with Betas

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)


# Plot uncertainty intervals by parameter (ndvi_diff_n)
# Lower beta values indicate that greater flow in the treated watershed with lower relative ndvi
plot_ndvi_diff_beta <- out_q_ndvi_diff_draws %>%      
  ggplot(data=., aes(y = response_variable, x = ndvi_diff_n)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  # scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = expression('NDVI Diff Coefficient ('*beta*')') , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  #xlim(-100,100) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_beta.jpg",plot=plot_ndvi_diff_beta, width = 5, height = 3)


# Plot uncertainty intervals by parameter (ndvi_diff_n) for interaction model
# Lower beta values indicate that greater flow in the treated watershed with lower relative ndvi
plot_ndvi_diff_int_beta <- out_q_ndvi_diff_int_draws %>%      
  ggplot(data=., aes(y = response_variable, x = ndvi_diff_n)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  # scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = expression('NDVI Diff Coefficient ('*beta*')') , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  #xlim(-100,100) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_int_beta.jpg",plot=plot_ndvi_diff_int_beta, width = 5, height = 3)


# Plot uncertainty intervals by parameter ('log(q_control):ndvi_diff_n`)for interaction model
plot_ndvi_diff_int_beta2 <- out_q_ndvi_diff_int_draws %>%      
  ggplot(data=., aes(y = response_variable, x = `log(q_control):ndvi_diff_n`)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  # scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = expression('NDVI Diff Coefficient ('*beta*')') , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  #xlim(-100,100) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_int_beta2.jpg",plot=plot_ndvi_diff_int_beta2, width = 5, height = 3)



# ---------------------------------------------------------------------
# Plot mixed modeling output with effect sizes

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)


# Plot effect size by parameter (treatment_effect_size)
plot_ndvi_diff_es <- out_q_ndvi_diff_draws %>%      
  ggplot(data=., aes(y = response_variable, x = treatment_effect_size)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  #scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = "Change in Streamflow Response Variable (%)\n given a 10 point reduction in normalized NDVI" , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  #xlim(-100,100) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_effect_size.jpg",plot=plot_ndvi_diff_es, width = 5, height = 3)


# Plot effect size by parameter (ndvi_diff)
plot_ndvi_diff_es <- out_q_ndvi_diff_draws %>%      
  ggplot(data=., aes(y = response_variable, x = treatment_effect_size)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  #scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  labs(x = "Change in Streamflow Response Variable (%)\n given a 10 percent reduction in normalized NDVI" , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  #xlim(-100,100) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_effect_size.jpg",plot=plot_ndvi_diff_es, width = 5, height = 3)



# ---------------------------------------------------------------------
# Plot showing effect size across of range of NDVI differences (diff)

q_estimates <- purrr::map(out_q_ndvi_diff, broom::tidy)
names(q_estimates) <- c("Q95", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Annual")

ndvi_c <- 1.0               # Starting control watershed Normalized NDVI
frac <- seq(0.01,1,0.01)    # Fraction of control watershed NDVI in treated watershed

ndvi_ratio_es_range <- purrr::map_dfr(frac, function(x){
  purrr::map_dfr(q_estimates, function(y) (exp(y$estimate[3] * (ndvi_c*x)) / 
                                             exp(y$estimate[3] * ndvi_c))*100 - 100)
})

ndvi_ratio_es_range <- ndvi_ratio_es_range %>% 
  dplyr::bind_cols(., ndvi_frac = frac) %>% 
  tidyr::gather(key="response_variable", value="q_change", -ndvi_frac) %>% 
  dplyr::mutate(ndvi_reduction = (1 - ndvi_frac)*100)


x <- ggplot(data=ndvi_ratio_es_range) +
  geom_line(aes(x=ndvi_reduction, y=q_change, linetype=response_variable, group=response_variable)) +
  geom_hline(aes(yintercept=0), color="red") +
  labs(x = "Reduction in normalized NDVI of treated watershed\nrelative to normalized NDVI of control watershed (%)" , y = "Change in streamflow (%)") +
  theme_bw(base_size = 9) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_deltaQ_vs_ndvi_diff.jpg",plot=x, width = 4, height = 3)



# ---------------------------------------------------------------------
# Save output


write_rds(out_q_ndvi_var_draws, "output/2.4_mixed_model_analysis/out_q_ndvi_var_draws.rds")
write_rds(out_q_ndvi_ratio_draws, "output/2.4_mixed_model_analysis/out_q_ndvi_ratio_draws.rds")
write_rds(out_q_ndvi_diff_draws, "output/2.4_mixed_model_analysis/out_q_ndvi_diff_draws.rds")
write_rds(out_q_ndvi_diff_int_draws, "output/2.4_mixed_model_analysis/out_q_ndvi_diff_int_draws.rds")














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










