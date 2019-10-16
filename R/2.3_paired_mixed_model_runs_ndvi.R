# KREW mixed-model analysis

# This code produces four models with NDVI treatment variables:
# Raw NDVI, NDVI ratio, NDVI normalized difference, and NDVI normalized difference with interaction



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
ADAPT_DELTA <- 0.98

# ---------------------------------------------------------------------
# NDVI: Treated and control variable

paired_function1 <- function(DATA, ITER, WARMUP, CHAINS,
                             CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + ndvi_treated + ndvi_control +
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

out_q95 <- paired_function1(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1 <- paired_function1(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2 <- paired_function1(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3 <- paired_function1(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4 <- paired_function1(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy <- paired_function1(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q_ndvi_var <- list(out_q95,
                       out_s1,
                       out_s2,
                       out_s3,
                       out_s4,
                       out_wy)


# ---------------------------------------------------------------------
# NDVI: Ratio variable

paired_function2 <- function(DATA, ITER, WARMUP, CHAINS,
                             CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + ndvi_ratio +
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

out_q95 <- paired_function2(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1 <- paired_function2(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2 <- paired_function2(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3 <- paired_function2(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4 <- paired_function2(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy <- paired_function2(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q_ndvi_ratio <- list(out_q95,
                         out_s1,
                         out_s2,
                         out_s3,
                         out_s4,
                         out_wy)


# ---------------------------------------------------------------------
# NDVI: Diff variable

paired_function3 <- function(DATA, ITER, WARMUP, CHAINS,
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

out_q95 <- paired_function3(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1 <- paired_function3(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2 <- paired_function3(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3 <- paired_function3(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4 <- paired_function3(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy <- paired_function3(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q_ndvi_diff <- list(out_q95,
                        out_s1,
                        out_s2,
                        out_s3,
                        out_s4,
                        out_wy)


# ---------------------------------------------------------------------
# NDVI: Diff variable with interaction variable

paired_function4 <- function(DATA, ITER, WARMUP, CHAINS,
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

out_q95 <- paired_function4(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1 <- paired_function4(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2 <- paired_function4(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3 <- paired_function4(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4 <- paired_function4(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy <- paired_function4(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q_ndvi_diff_int <- list(out_q95,
                            out_s1,
                            out_s2,
                            out_s3,
                            out_s4,
                            out_wy)


# ---------------------------------------------------------------------
# Save modeling output

# Treated dummy
write_rds(out_q_ndvi_var, Q_NDVI_VAR_RDS)
write_rds(out_q_ndvi_ratio, Q_NDVI_RATIO_RDS)
write_rds(out_q_ndvi_diff, Q_NDVI_DIFF_RDS)
write_rds(out_q_ndvi_diff_int, Q_NDVI_DIFF_INT_RDS)


