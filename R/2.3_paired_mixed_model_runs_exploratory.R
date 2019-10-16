# KREW mixed-model analysis - exploratory

# Code separates out All watersheds, Bull and Prov for all hydrologic variables
# Individual 'regression-equivalent' code at bottom


source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Import data

pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_monthly <- read_rds(PAIR_MONTHLY_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)

# ---------------------------------------------------------------------
# Mixed modeling with rstanarm

# Inputs
ITER <- 5000
WARMUP <- 1000
CHAINS <- 4
CORES <- 2
THIN <- 5
SEED <- 49
ADAPT_DELTA <- 0.99



# ---------------------------------------------------------------------
# NDVI: Exploratory

paired_function1 <- function(DATA, ITER, WARMUP, CHAINS,
                                CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + ndvi_diff_n +
                      (1 | shed_treated),
                    data = DATA,
                    family = gaussian, 
                    prior = normal(),
                    prior_intercept = normal(),
                    chains = CHAINS, cores = CORES, seed = SEED,
                    iter = ITER, warmup = WARMUP, thin = THIN,
                    adapt_delta = ADAPT_DELTA)
  return(out)
}


# ---------------------------------------------------------------------
# NDVI: All data

out_wy <- pair_wy %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95 <- pair_q95 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1 <- pair_seasonal_1 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2 <- pair_seasonal_2 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3 <- pair_seasonal_3 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4 <- pair_seasonal_4 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ---

summary(out_q95, digits=2, probs = c(0.025, 0.975))
summary(out_s1, digits=2, probs = c(0.025, 0.975))
summary(out_s2, digits=2, probs = c(0.025, 0.975))
summary(out_s3, digits=2, probs = c(0.025, 0.975))
summary(out_s4, digits=2, probs = c(0.025, 0.975))
summary(out_wy, digits=2, probs = c(0.025, 0.975))


# ---------------------------------------------------------------------
# NDVI: Bull data

out_wyb <- pair_wy %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95b <- pair_q95 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1b <- pair_seasonal_1 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2b <- pair_seasonal_2 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3b <- pair_seasonal_3 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4b <- pair_seasonal_4 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ---

summary(out_q95b, digits=2, probs = c(0.025, 0.975))
summary(out_s1b, digits=2, probs = c(0.025, 0.975))
summary(out_s2b, digits=2, probs = c(0.025, 0.975))
summary(out_s3b, digits=2, probs = c(0.025, 0.975))
summary(out_s4b, digits=2, probs = c(0.025, 0.975))
summary(out_wyb, digits=2, probs = c(0.025, 0.975))


# ---------------------------------------------------------------------
# NDVI: Prov data

out_wyp <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95p <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1p <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2p <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3p <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4p <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ---

summary(out_q95p, digits=2, probs = c(0.025, 0.975))
summary(out_s1p, digits=2, probs = c(0.025, 0.975))
summary(out_s2p, digits=2, probs = c(0.025, 0.975))
summary(out_s3p, digits=2, probs = c(0.025, 0.975))
summary(out_s4p, digits=2, probs = c(0.025, 0.975))
summary(out_wyp, digits=2, probs = c(0.025, 0.975))


# ---------------------------------------------------------------------
# 

out_q <- list(out_q95,
              out_s1,
              out_s2,
              out_s3,
              out_s4,
              out_wy)


out_q_bull <- list(out_q95b,
                   out_s1b,
                   out_s2b,
                   out_s3b,
                   out_s4b,
                   out_wyb)

out_q_prov <- list(out_q95p,
                   out_s1p,
                   out_s2p,
                   out_s3p,
                   out_s4p,
                   out_wyp)




# ---------------------------------------------------------------------
# Process mixed modeling output


# Extract the median (and intervals) for each parameter.
out_q_median <- purrr::map(out_q, function(x) x %>%
                             tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n) %>% 
                             tidybayes::median_qi()
)
out_q_median <- bind_rows(out_q_median, .id="response_variable")


generate_draws <- function(out_q){
  # Generate the draws for each parameter
  out_q_draws <- purrr::map(out_q, function(x) x %>%
                              tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n)
  )
  out_q_draws <- bind_rows(out_q_draws, .id="response_variable")
  
  out_q_draws$response_variable <- factor(out_q_draws$response_variable,
                                          levels = c(6,5,4,3,2,1))
  out_q_draws$.chain <- factor(out_q_draws$.chain)
  out_q_draws$.iteration <- factor(out_q_draws$.iteration)
  out_q_draws$.draw <- factor(out_q_draws$.draw)
  return(out_q_draws)
}

out_q_draws <- generate_draws(out_q)
out_q_bull_draws <- generate_draws(out_q_bull)
out_q_prov_draws <- generate_draws(out_q_prov)

out_draws <- bind_rows(all_data = out_q_draws,
                       bull_data = out_q_bull_draws,
                       prov_data = out_q_prov_draws,
                       .id="model")
out_draws$model <- factor(out_draws$model,
                              levels =c("prov_data", "bull_data", "all_data"))

# ---------------------------------------------------------------------
# Plot mixed modeling output with Betas

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)


# Plot uncertainty intervals by parameter (ndvi_diff_n)
# Lower beta values indicate that greater flow in the treated watershed with lower relative ndvi
plot_out_draws <- out_draws %>%      
  ggplot(data=., aes(y = response_variable, x = ndvi_diff_n)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55") +
  geom_vline(xintercept = 0) +
  # scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  facet_grid(.~model) +
  labs(x = expression('NDVI Diff Coefficient ('*beta*')') , y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  xlim(-3,3) +
  NULL
plot(plot_out_draws)
#ggsave("output/2.3_mixed_model_analysis/plot_ndvi_diff_beta.jpg",plot=plot_out_draws, width = 5, height = 3)


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



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Plot mixed modeling output with Betas (just annual)
# This should eventually be moved to 2.4

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)

watershed_id <- c(all_data = "All\nWatersheds",
                  bull_data = "Bull",
                  #prov_data = "Providence\n(Control: P304)")
                  prov_data = "Providence")

# Plot uncertainty intervals by parameter (interaction variable for All watersheds, Bull, and Prov1)
x <- out_draws %>%
  dplyr::filter(response_variable == 6) %>% 
  ggplot(data=., aes(y = model, x = ndvi_diff_n)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Normalized NDVI variable",
       x = expression('nNDVI'[diff]~'Coefficient ('*beta*')'),
       y = "Watersheds Included in Model") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  xlim(-2,2) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_mixed_model_beta1.jpg",plot=x, width = 5, height = 3)
#plot(x)


# Plot uncertainty intervals by parameter (change variable for Bull, Prov1)
x <- out_draws %>%
  dplyr::filter(model != "all_data") %>% 
  dplyr::filter(response_variable == 6) %>% 
  ggplot(data=., aes(y = model, x = ndvi_diff_n)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Normalized NDVI variable",
       x = expression('nNDVI'[diff]~'Coefficient ('*beta[3]*')'),
       y = "Watersheds Included in Model") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  xlim(-2,2) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_mixed_model_beta1.jpg",plot=x, width = 5, height = 3)
#plot(x)




# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------
# Model at single watershed level



paired_function1 <- function(DATA, ITER, WARMUP, CHAINS,
                             CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glm(log(q_treated) ~ log(q_control) + ndvi_diff_n,
                    data = DATA,
                    family = gaussian, 
                    prior = normal(),
                    prior_intercept = normal(),
                    chains = CHAINS, cores = CORES, seed = SEED,
                    iter = ITER, warmup = WARMUP, thin = THIN,
                    adapt_delta = ADAPT_DELTA)
  return(out)
}


# ---------------------------------------------------------------------
# NDVI: All data

out_wy_individual <- pair_wy %>% 
  #dplyr::filter(shed_treated %in% c("P301")) %>% 
  #dplyr::filter(shed_treated %in% c("D102")) %>% 
  #dplyr::filter(shed_treated %in% c("P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201")) %>% 
  #dplyr::filter(shed_treated %in% c("B203")) %>% 
  dplyr::filter(shed_treated %in% c("B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

summary(out_wy, digits=2, probs = c(0.025, 0.975))


# -----

out_q95_individual <- pair_q95 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1_individual <- pair_seasonal_1 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2_individual <- pair_seasonal_2 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3_individual <- pair_seasonal_3 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4_individual <- pair_seasonal_4 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ---

summary(out_q95_individual, digits=2, probs = c(0.025, 0.975))
summary(out_s1_individual, digits=2, probs = c(0.025, 0.975))
summary(out_s2_individual, digits=2, probs = c(0.025, 0.975))
summary(out_s3_individual, digits=2, probs = c(0.025, 0.975))
summary(out_s4_individual, digits=2, probs = c(0.025, 0.975))
summary(out_wy_individual, digits=2, probs = c(0.025, 0.975))







