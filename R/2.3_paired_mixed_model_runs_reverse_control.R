# KREW mixed-model analysis

# This code reverses the control in Providence from P304 to P303. 
# # Individual 'regression-equivalent' code at bottom


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
# Code for changing control watershed
# Change control from P304 to P303

# Removes P304 control from the tibble
pair_wy_tmp <- pair_wy %>% 
  dplyr::filter(shed_treated != "P303", shed_control != "T003") %>% 
  dplyr::select(-c(ndvi_ratio, ndvi_diff, ndvi_treated_n, ndvi_control_n, ndvi_ratio_n, ndvi_diff_n)) %>%     # Remove ndvi variables that will need to be recalculated
  dplyr::select(-c(shed_control, q_control, ndvi_control))

# Isolates P303 so it can be joined as a control
control_new <- pair_wy %>% 
  dplyr::filter(shed_treated == "P303") %>% 
  dplyr::select(c(shed_treated, WY, q_treated, ndvi_treated)) %>% 
  dplyr::rename(shed_control = shed_treated, q_control = q_treated, ndvi_control = ndvi_treated)

# Rearranges the control P304 to be put in the treated position
treated_new <- pair_wy %>% 
  dplyr::filter(shed_treated == "P303") %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(shed_treated, q_treated, ndvi_treated)) %>% 
  dplyr::select(-c(ndvi_ratio, ndvi_diff, ndvi_treated_n, ndvi_control_n, ndvi_ratio_n, ndvi_diff_n)) %>%     # Remove ndvi variables that will need to be recalculated
  dplyr::rename(shed_treated = shed_control, q_treated = q_control, ndvi_treated = ndvi_control)

# Combine everything
pair_wy_new <- dplyr::full_join(pair_wy_tmp, control_new, by="WY")
pair_wy_new <- bind_rows(pair_wy_new,
                         dplyr::full_join(treated_new, control_new, by="WY"))

# Recalculate NDVI metrics
pair_wy_new <- pair_wy_new %>% 
  dplyr::mutate(ndvi_ratio = ndvi_treated/ndvi_control,
                ndvi_diff = ndvi_treated - ndvi_control) %>% 
  dplyr::group_by(shed_treated) %>% 
  dplyr::mutate(ndvi_treated_n = (ndvi_treated - min(ndvi_treated))/(max(ndvi_treated) - min(ndvi_treated)),
                ndvi_control_n = (ndvi_control - min(ndvi_control))/(max(ndvi_control) - min(ndvi_control)),
                ndvi_ratio_n = ndvi_treated_n/ndvi_control_n,
                ndvi_diff_n = ndvi_treated_n - ndvi_control_n) %>% 
  dplyr::ungroup()

pair_wy_new <- dplyr::filter(pair_wy_new, WY != 2017)

# Combine back with Bull

pair_wy_alt <- bind_rows(pair_wy_new, dplyr::filter(pair_wy, shed_control == "T003"))

# This is the new variable with the alternative control
pair_wy_alt





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

out_wy_alt <- pair_wy_alt %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P304")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  #dplyr::filter(shed_treated != "D102") %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wyb_alt <- pair_wy_alt %>% 
  #dplyr::filter(shed_treated %in% c("P301","D102","P304")) %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wyp_alt <- pair_wy_alt %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P304")) %>% 
  #dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  paired_function1(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)


# ---

summary(out_wy_alt, digits=2, probs = c(0.025, 0.975))
summary(out_wyb_alt, digits=2, probs = c(0.025, 0.975))
summary(out_wyp_alt, digits=2, probs = c(0.025, 0.975))

out_q_alt <- list(out_wy_alt,
                  out_wyb_alt,
                  out_wyp_alt)


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

out_q_alt_draws <- generate_draws(out_q_alt)







# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------
# Model at single watershed level


paired_function2 <- function(DATA, ITER, WARMUP, CHAINS,
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

out_wy_individual_alt <- pair_wy_alt %>% 
  #dplyr::filter(shed_treated %in% c("P301")) %>% 
  #dplyr::filter(shed_treated %in% c("D102")) %>% 
  dplyr::filter(shed_treated %in% c("P304")) %>% 
  #dplyr::filter(shed_treated %in% c("B201")) %>% 
  #dplyr::filter(shed_treated %in% c("B203")) %>% 
  #dplyr::filter(shed_treated %in% c("B204")) %>% 
  paired_function2(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                   CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

summary(out_wy_individual_alt, digits=2, probs = c(0.025, 0.975))


