# KREW mixed-model analysis: Effect size

# With rstanarm, and tidybayes


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

# Mixed model input data
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)


# Import mixed model results
out_q_ndiff_all <- read_rds("output/2.4_mixed_model/out_q_ndiff_all.rds")
out_q_ndiff_bull <- read_rds("output/2.4_mixed_model/out_q_ndiff_bull.rds")
out_q_ndiff_prov <- read_rds("output/2.4_mixed_model/out_q_ndiff_prov.rds")

out_q_diff_all <- read_rds("output/2.4_mixed_model/out_q_diff_all.rds")
out_q_diff_bull <- read_rds("output/2.4_mixed_model/out_q_diff_bull.rds")
out_q_diff_prov <- read_rds("output/2.4_mixed_model/out_q_diff_prov.rds")

out_q_ratio_all <- read_rds("output/2.4_mixed_model/out_q_ratio_all.rds")
out_q_ratio_bull <- read_rds("output/2.4_mixed_model/out_q_ratio_bull.rds")
out_q_ratio_prov <- read_rds("output/2.4_mixed_model/out_q_ratio_prov.rds")


# Import mixed model draws
out_q_diff_draws <- read_rds("output/2.5_mixed_model_analysis/out_q_diff_draws.rds")
out_q_ndiff_draws <- read_rds("output/2.5_mixed_model_analysis/out_q_ndiff_draws.rds")
out_q_ratio_draws <- read_rds("output/2.5_mixed_model_analysis/out_q_ratio_draws.rds")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Effect size code

# Need to quantify the change in streamflow for a given reduction/difference in NDVI










# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Old code from before using NDVIdiff variable



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






# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# What is the effect size?

# Example for NDVI ratio with 0.05 point reduction in NDVI
q_estimates <- purrr::map(out_q, broom::tidy)
bb <- 1000

# Cycle through the 6 outputs
for (aa in seq(1,6)){
  # q_percent <- (exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 0.65 + q_estimates[[aa]]$estimate[4] * log(bb) * 0.65) / 
  #                 exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 0.7 + q_estimates[[aa]]$estimate[4] * log(bb) * 0.7))*100 - 100
  
  q_diff <- exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 0.5 + q_estimates[[aa]]$estimate[4] * log(bb) * 0.5) - 
    exp(q_estimates[[aa]]$estimate[1] + q_estimates[[aa]]$estimate[2] * log(bb) + q_estimates[[aa]]$estimate[3] * 0.7 + q_estimates[[aa]]$estimate[4] * log(bb) * 0.7)
  
  #print(q_percent)
  print(q_diff)
  print("---------------")
}

# ---------------------------------------------------------------------
# Change draws to effect size


# For a fixed point reduction in NDVI
out_q_draws <- out_q_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size = (exp(ndvi_diff_n * (-0.05)) /  exp(ndvi_diff_n * 0))*100 - 100)




# ---------------------------------------------------------------------
# Plot mixed modeling output with effect sizes


# Plot uncertainty intervals by parameter (ndvi_diff)
plot_diff_es <- out_q_draws %>%      
  ggplot(data=., aes(y = response_variable, x = treatment_effect_size)) +
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
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_effect_size.jpg",plot=plot_diff_es, width = 5, height = 3)





# ---------------------------------------------------------------------
# Plot showing effect size across of range of NDVI differences

q_estimates <- purrr::map(out_q, broom::tidy)
names(q_estimates) <- c("Q95", "Oct-Dec", "Jan-Mar", "Apr-Jun", "Jul-Sep", "Annual")

ndvi_c=0.7     # Starting control watershed NDVI
frac=seq(0.01,1,0.01)    # Fraction of control watershed NDVI in treated watershed

ndvi_ratio_es_range <- purrr::map_dfr(frac, function(x){
  purrr::map_dfr(q_estimates, function(y) (exp(y$estimate[3] * (ndvi_c*x)) / 
                                             exp(y$estimate[3] * ndvi_c))*100 - 100)
})

ndvi_ratio_es_range <- ndvi_ratio_es_range %>% 
  dplyr::bind_cols(., reduction_frac = frac) %>% 
  tidyr::gather(key="response_variable", value="q_change", -reduction_frac)


x <- ggplot(data=ndvi_ratio_es_range) +
  geom_line(aes(x=reduction_frac, y=q_change, linetype=response_variable, group=response_variable)) +
  geom_hline(aes(yintercept=0), color="red") +
  labs(x = "Fraction of NDVI in treated watershed\nrelative to control watershed" , y = "Change in streamflow (%)") +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_deltaQ_vs_ndvi_diff.jpg",plot=x, width = 4, height = 3)


