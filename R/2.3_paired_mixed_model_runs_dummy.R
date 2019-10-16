# KREW mixed-model analysis - for dummy variable that doesn't work

# With rstanarm, and tidybayes

# To-do
# Add a treatment_wy variable (have prescribed burn reset post-year number)


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
# Dummy: Treated variable

paired_treated_dummy <- function(DATA, ITER, WARMUP, CHAINS,
                                 CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + treatment_dummy + 
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

# out_mam7_treated_dummy <- paired_treated_dummy(DATA=pair_mam7, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                                                      CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95_treated_dummy <- paired_treated_dummy(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                              CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1_treated_dummy <- paired_treated_dummy(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2_treated_dummy <- paired_treated_dummy(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3_treated_dummy <- paired_treated_dummy(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4_treated_dummy <- paired_treated_dummy(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_treated_dummy <- paired_treated_dummy(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_treated_dummy <- list(#out_mam7_treated_dummy,
  out_q95_treated_dummy,
  out_s1_treated_dummy,
  out_s2_treated_dummy,
  out_s3_treated_dummy,
  out_s4_treated_dummy,
  out_wy_treated_dummy)


# ---------------------------------------------------------------------
# Dummy: Thinning Variable

paired_thinning_dummy <- function(DATA, ITER, WARMUP, CHAINS,
                                  CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + thinning_dummy + 
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

# out_mam7_thinning_dummy <- paired_thinning_dummy(DATA=pair_mam7, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                                                      CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95_thinning_dummy <- paired_thinning_dummy(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1_thinning_dummy <- paired_thinning_dummy(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                               CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2_thinning_dummy <- paired_thinning_dummy(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                               CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3_thinning_dummy <- paired_thinning_dummy(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                               CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4_thinning_dummy <- paired_thinning_dummy(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                               CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_thinning_dummy <- paired_thinning_dummy(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                               CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_thinning_dummy <- list(#out_mam7_thinning_dummy,
  out_q95_thinning_dummy,
  out_s1_thinning_dummy,
  out_s2_thinning_dummy,
  out_s3_thinning_dummy,
  out_s4_thinning_dummy,
  out_wy_thinning_dummy)


# ---------------------------------------------------------------------
# Dummy: Prescribed Fire Variable

paired_prescribed_fire_dummy <- function(DATA, ITER, WARMUP, CHAINS,
                                         CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + prescribed_fire_dummy + 
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

# out_mam7_prescribed_fire_dummy <- paired_prescribed_fire_dummy(DATA=pair_mam7, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                                                      CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95_prescribed_fire_dummy <- paired_prescribed_fire_dummy(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                              CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1_prescribed_fire_dummy <- paired_prescribed_fire_dummy(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2_prescribed_fire_dummy <- paired_prescribed_fire_dummy(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3_prescribed_fire_dummy <- paired_prescribed_fire_dummy(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4_prescribed_fire_dummy <- paired_prescribed_fire_dummy(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_prescribed_fire_dummy <- paired_prescribed_fire_dummy(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                             CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_prescribed_fire_dummy <- list(#out_mam7_prescribed_fire_dummy,
  out_q95_prescribed_fire_dummy,
  out_s1_prescribed_fire_dummy,
  out_s2_prescribed_fire_dummy,
  out_s3_prescribed_fire_dummy,
  out_s4_prescribed_fire_dummy,
  out_wy_prescribed_fire_dummy)



# ---------------------------------------------------------------------
# Testing alternative models

paired_treated_dummy_int <- function(DATA, ITER, WARMUP, CHAINS,
                                     CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + treatment_dummy +
                      log(q_control) * treatment_dummy +
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

# out_mam7_treated_dummy_int <- paired_treated_dummy_int(DATA=pair_mam7, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                                                      CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_q95_treated_dummy_int <- paired_treated_dummy_int(DATA=pair_q95, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                      CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s1_treated_dummy_int <- paired_treated_dummy_int(DATA=pair_seasonal_1, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                     CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s2_treated_dummy_int <- paired_treated_dummy_int(DATA=pair_seasonal_2, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                     CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s3_treated_dummy_int <- paired_treated_dummy_int(DATA=pair_seasonal_3, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                     CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_s4_treated_dummy_int <- paired_treated_dummy_int(DATA=pair_seasonal_4, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                     CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_treated_dummy_int <- paired_treated_dummy_int(DATA=pair_wy, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                     CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_treated_dummy_int <- list(#out_mam7_treated_dummy_int,
  out_q95_treated_dummy_int,
  out_s1_treated_dummy_int,
  out_s2_treated_dummy_int,
  out_s3_treated_dummy_int,
  out_s4_treated_dummy_int,
  out_wy_treated_dummy_int)



# ---------------------------------------------------------------------
# Save modeling output

# Treated dummy
write_rds(out_treated_dummy, TREATED_DUMMY_RDS)
write_rds(out_thinning_dummy, THINNING_DUMMY_RDS)
write_rds(out_prescribed_fire_dummy, PRESCRIBED_FIRE_DUMMY_RDS)
write_rds(out_treated_dummy_int, TREATED_DUMMY_INT_RDS)


