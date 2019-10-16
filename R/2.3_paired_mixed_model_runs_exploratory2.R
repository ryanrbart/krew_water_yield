# KREW mixed-model analysis - exploratory of interaction variable

# Similar to 2.3_paired_mixed_model_runs_exploratory, but for interaction variables.
# Code separates out All watersheds, Bull and Prov for all hydrologic variables


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
ITER <- 7000
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
  out <- stan_glmer(log(q_treated) ~ log(q_control) + ndvi_diff_n + log(q_control)*ndvi_diff_n +
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

out_wy_int <- pair_wy %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95_int <- pair_q95 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1_int <- pair_seasonal_1 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2_int <- pair_seasonal_2 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3_int <- pair_seasonal_3 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4_int <- pair_seasonal_4 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ---

summary(out_q95_int, digits=2, probs = c(0.025, 0.975))
summary(out_s1_int, digits=2, probs = c(0.025, 0.975))
summary(out_s2_int, digits=2, probs = c(0.025, 0.975))
summary(out_s3_int, digits=2, probs = c(0.025, 0.975))
summary(out_s4_int, digits=2, probs = c(0.025, 0.975))
summary(out_wy_int, digits=2, probs = c(0.025, 0.975))


# ---------------------------------------------------------------------
# NDVI: Bull data

out_wyb_int <- pair_wy %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95b_int <- pair_q95 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1b_int <- pair_seasonal_1 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2b_int <- pair_seasonal_2 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3b_int <- pair_seasonal_3 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4b_int <- pair_seasonal_4 %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ---

summary(out_q95b_int, digits=2, probs = c(0.025, 0.975))
summary(out_s1b_int, digits=2, probs = c(0.025, 0.975))
summary(out_s2b_int, digits=2, probs = c(0.025, 0.975))
summary(out_s3b_int, digits=2, probs = c(0.025, 0.975))
summary(out_s4b_int, digits=2, probs = c(0.025, 0.975))
summary(out_wyb_int, digits=2, probs = c(0.025, 0.975))


# ---------------------------------------------------------------------
# NDVI: Prov data

out_wyp_int <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95p_int <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1p_int <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2p_int <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3p_int <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4p_int <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ---

summary(out_q95p_int, digits=2, probs = c(0.025, 0.975))
summary(out_s1p_int, digits=2, probs = c(0.025, 0.975))
summary(out_s2p_int, digits=2, probs = c(0.025, 0.975))
summary(out_s3p_int, digits=2, probs = c(0.025, 0.975))
summary(out_s4p_int, digits=2, probs = c(0.025, 0.975))
summary(out_wyp_int, digits=2, probs = c(0.025, 0.975))


# ---------------------------------------------------------------------
# 

out_q_int <- list(out_q95_int,
              out_s1_int,
              out_s2_int,
              out_s3_int,
              out_s4_int,
              out_wy_int)


out_q_int_bull <- list(out_q95b_int,
                   out_s1b_int,
                   out_s2b_int,
                   out_s3b_int,
                   out_s4b_int,
                   out_wyb_int)

out_q_int_prov <- list(out_q95p_int,
                   out_s1p_int,
                   out_s2p_int,
                   out_s3p_int,
                   out_s4p_int,
                   out_wyp_int)




# ---------------------------------------------------------------------
# Process mixed modeling output


# Extract the median (and intervals) for each parameter.
out_q_int_median <- purrr::map(out_q_int, function(x) x %>%
                             tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n, `log(q_control):ndvi_diff_n`) %>% 
                             tidybayes::median_qi()
)
out_q_int_median <- bind_rows(out_q_int_median, .id="response_variable")


generate_draws <- function(out_q){
  # Generate the draws for each parameter
  out_q_draws <- purrr::map(out_q, function(x) x %>%
                              tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n, `log(q_control):ndvi_diff_n`)
  )
  out_q_draws <- bind_rows(out_q_draws, .id="response_variable")
  
  out_q_draws$response_variable <- factor(out_q_draws$response_variable,
                                          levels = c(6,5,4,3,2,1))
  out_q_draws$.chain <- factor(out_q_draws$.chain)
  out_q_draws$.iteration <- factor(out_q_draws$.iteration)
  out_q_draws$.draw <- factor(out_q_draws$.draw)
  return(out_q_draws)
}

out_q_int_draws <- generate_draws(out_q_int)
out_q_int_bull_draws <- generate_draws(out_q_int_bull)
out_q_int_prov_draws <- generate_draws(out_q_int_prov)

out_int_draws <- bind_rows(all_data = out_q_int_draws,
                       bull_data = out_q_int_bull_draws,
                       prov_data = out_q_int_prov_draws,
                       .id="model")
out_int_draws$model <- factor(out_int_draws$model,
                              levels =c("prov_data", "bull_data", "all_data"))


# ---------------------------------------------------------------------
# Plot mixed modeling output with Betas
# This should eventually be moved to 2.4


response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)


# Plot uncertainty intervals by parameter (ndvi_diff_n)
# Lower beta values indicate that greater flow in the treated watershed with lower relative ndvi
plot_out_draws <- out_int_draws %>%      
  ggplot(data=., aes(y = response_variable, x = ndvi_diff_n)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55", .width = c(0.9, 0.95), size_range=c(0.65,1.5), fatten_point=0.8) +
  geom_vline(xintercept = 0, color = "firebrick") +
  # scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  facet_grid(.~model) +
  labs(title= "Normalized NDVI Variable",
       x = expression('nNDVI'[diff]~'Coefficient ('*beta*')'),
       y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  xlim(-7,7) +
  NULL
#plot(plot_out_draws)
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_int_beta1.jpg",plot=plot_out_draws, width = 5, height = 3)



# Plot uncertainty intervals by parameter (`log(q_control):ndvi_diff_n`)
# Lower beta values indicate that greater flow in the treated watershed with lower relative ndvi
plot_out_draws_int <- out_int_draws %>%      
  ggplot(data=., aes(y = response_variable, x = `log(q_control):ndvi_diff_n`)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55", size=2, .width = c(0.9, 0.95), size_range=c(0.65,1.5), fatten_point=0.8) +
  geom_vline(xintercept = 0, color = "firebrick") +
  # scale_x_continuous(breaks = c(-80,-60,-40,-20,0,20,40,60),labels = c("-80","-60","-40","-20","0","20","40","60")) +
  scale_y_discrete(labels = c(response_variable_id)) +
  facet_grid(.~model) +
  labs(title= "Normalized NDVI * Control Streamflow Interaction Variable",
       x = expression('nNDVI'[diff]~'*Q'[c]~'Coefficient ('*beta*')'),
       y = "Streamflow Response Variable") +
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  xlim(-3,3) +
  NULL
#plot(plot_out_draws_int)
ggsave("output/2.4_mixed_model_analysis/plot_ndvi_diff_int_beta2.jpg",plot=plot_out_draws_int, width = 5, height = 3)




# ---------------------------------------------------------------------
# Plot mixed modeling output with Betas (just annual)
# This should eventually be moved to 2.4

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)

watershed_id <- c(all_data = "All\nWatersheds",
                  bull_data = "Bull",
                  prov_data = "Providence\n(Control: P304)")



# Plot uncertainty intervals by parameter (interaction variable for Bull, Prov1 and Prov2)
x <- out_int_draws %>%
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
  xlim(-10,10) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_mixed_model_int_beta1.jpg",plot=x, width = 5, height = 3)
#plot(x)


# Plot uncertainty intervals by parameter (interaction variable for Bull, Prov1 and Prov2)
x <- out_int_draws %>%      
  dplyr::filter(response_variable == 6) %>% 
  ggplot(data=., aes(y = model, x = `log(q_control):ndvi_diff_n`)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Normalized NDVI * Control Streamflow\nInteraction Variable",
       x = expression('nNDVI'[diff]~'*Q'[c]~'Coefficient ('*beta*')'),
       y = "Watersheds Included in Model") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  xlim(-1.75,1.75) +
  NULL
ggsave("output/2.4_mixed_model_analysis/plot_mixed_model_int_beta2.jpg",plot=x, width = 5, height = 3)
#plot(x)





