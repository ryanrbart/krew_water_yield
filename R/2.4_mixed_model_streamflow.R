# KREW mixed-model analysis




source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
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
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Types of models

# Watershed (3): All, Bull, Prov
# Response variables (6): Q95, S1-4, WY
# Model Complexity (2): With interaction variable, Without interaction variable
# Change Variable (4): NDVI_ratio, NDVI_diff, NDVI_ratio_n, NDVI_diff_n
# Total permutations: 144

# Alternate control watershed: none


# ---------------------------------------------------------------------
# Mixed model functions

# ----
# Non-interaction

function_paired_diff_n <- function(DATA, ITER, WARMUP, CHAINS,
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


function_paired_diff <- function(DATA, ITER, WARMUP, CHAINS,
                                 CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + ndvi_diff +
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


function_paired_ratio_n <- function(DATA, ITER, WARMUP, CHAINS,
                                    CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(log(q_treated) ~ log(q_control) + ndvi_ratio_n +
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


function_paired_ratio <- function(DATA, ITER, WARMUP, CHAINS,
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

# -----
# Regression model

function_paired_regr_diff <- function(DATA, ITER, WARMUP, CHAINS,
                                      CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glm(log(q_treated) ~ log(q_control) + ndvi_diff,
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
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Run the mixed model (up to 144 permutations)


# -------------------------------
# paired_diff_n - Non-interaction

# ----
# All
out_q95_ndiff_all <- pair_q95 %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_ndiff_all <- pair_seasonal_1 %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_ndiff_all <- pair_seasonal_2 %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_ndiff_all <- pair_seasonal_3 %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_ndiff_all <- pair_seasonal_4 %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_ndiff_all <- pair_wy %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ----
# Bull
out_q95_ndiff_bull <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_ndiff_bull <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_ndiff_bull <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_ndiff_bull <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_ndiff_bull <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_ndiff_bull <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ----
# Prov
out_q95_ndiff_prov <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_ndiff_prov <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_ndiff_prov <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_ndiff_prov <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_ndiff_prov <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_ndiff_prov <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                         CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)



# -------------------------------
# paired_diff - Non-interaction

# ----
# All
out_q95_diff_all <- pair_q95 %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_diff_all <- pair_seasonal_1 %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_diff_all <- pair_seasonal_2 %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_diff_all <- pair_seasonal_3 %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_diff_all <- pair_seasonal_4 %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_diff_all <- pair_wy %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ----
# Bull
out_q95_diff_bull <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_diff_bull <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_diff_bull <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_diff_bull <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_diff_bull <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_diff_bull <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ----
# Prov
out_q95_diff_prov <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_diff_prov <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_diff_prov <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_diff_prov <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_diff_prov <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_diff_prov <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)



# -------------------------------
# paired_ratio_n - Non-interaction

# # ----
# # All
# out_q95_nratio_all <- pair_q95 %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s1_nratio_all <- pair_seasonal_1 %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s2_nratio_all <- pair_seasonal_2 %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s3_nratio_all <- pair_seasonal_3 %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s4_nratio_all <- pair_seasonal_4 %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_wy_nratio_all <- pair_wy %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# 
# # ----
# # Bull
# out_q95_nratio_bull <- pair_q95 %>% 
#   dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s1_nratio_bull <- pair_seasonal_1 %>% 
#   dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s2_nratio_bull <- pair_seasonal_2 %>% 
#   dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s3_nratio_bull <- pair_seasonal_3 %>% 
#   dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s4_nratio_bull <- pair_seasonal_4 %>% 
#   dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_wy_nratio_bull <- pair_wy %>% 
#   dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# 
# # ----
# # Prov
# out_q95_nratio_prov <- pair_q95 %>% 
#   dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s1_nratio_prov <- pair_seasonal_1 %>% 
#   dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s2_nratio_prov <- pair_seasonal_2 %>% 
#   dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s3_nratio_prov <- pair_seasonal_3 %>% 
#   dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_s4_nratio_prov <- pair_seasonal_4 %>% 
#   dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
# out_wy_nratio_prov <- pair_wy %>% 
#   dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
#   function_paired_ratio_n(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                           CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)


# -------------------------------
# paired_ratio - Non-interaction

# ----
# All
out_q95_ratio_all <- pair_q95 %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_ratio_all <- pair_seasonal_1 %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_ratio_all <- pair_seasonal_2 %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_ratio_all <- pair_seasonal_3 %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_ratio_all <- pair_seasonal_4 %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_ratio_all <- pair_wy %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ----
# Bull
out_q95_ratio_bull <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_ratio_bull <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_ratio_bull <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_ratio_bull <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_ratio_bull <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_ratio_bull <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("B201","B203","B204")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# ----
# Prov
out_q95_ratio_prov <- pair_q95 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s1_ratio_prov <- pair_seasonal_1 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s2_ratio_prov <- pair_seasonal_2 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s3_ratio_prov <- pair_seasonal_3 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_s4_ratio_prov <- pair_seasonal_4 %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)
out_wy_ratio_prov <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("P301","D102","P303")) %>% 
  function_paired_ratio(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)


# -------------------------------
# Run regression models

out_wy_regr_diff_b201 <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("B201")) %>% 
  function_paired_regr_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_regr_diff_b203 <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("B203")) %>% 
  function_paired_regr_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_regr_diff_b204 <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("B204")) %>% 
  function_paired_regr_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_regr_diff_p301 <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("P301")) %>% 
  function_paired_regr_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                       CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_regr_diff_d102 <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("D102")) %>% 
  function_paired_regr_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_wy_regr_diff_p303 <- pair_wy %>% 
  dplyr::filter(shed_treated %in% c("P303")) %>% 
  function_paired_regr_diff(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Consolodate mixed model output

out_q_ndiff_all <- list(out_q95_ndiff_all, out_s1_ndiff_all,
                        out_s2_ndiff_all, out_s3_ndiff_all,
                        out_s4_ndiff_all, out_wy_ndiff_all)

out_q_ndiff_bull <- list(out_q95_ndiff_bull, out_s1_ndiff_bull,
                         out_s2_ndiff_bull, out_s3_ndiff_bull,
                         out_s4_ndiff_bull, out_wy_ndiff_bull)

out_q_ndiff_prov <- list(out_q95_ndiff_prov, out_s1_ndiff_prov,
                         out_s2_ndiff_prov, out_s3_ndiff_prov,
                         out_s4_ndiff_prov, out_wy_ndiff_prov)


# ----
out_q_diff_all <- list(out_q95_diff_all, out_s1_diff_all,
                        out_s2_diff_all, out_s3_diff_all,
                        out_s4_diff_all, out_wy_diff_all)

out_q_diff_bull <- list(out_q95_diff_bull, out_s1_diff_bull,
                         out_s2_diff_bull, out_s3_diff_bull,
                         out_s4_diff_bull, out_wy_diff_bull)

out_q_diff_prov <- list(out_q95_diff_prov, out_s1_diff_prov,
                         out_s2_diff_prov, out_s3_diff_prov,
                         out_s4_diff_prov, out_wy_diff_prov)


# ----
# out_q_nratio_all <- list(out_q95_nratio_all, out_s1_nratio_all,
#                         out_s2_nratio_all, out_s3_nratio_all,
#                         out_s4_nratio_all, out_wy_nratio_all)
# 
# out_q_nratio_bull <- list(out_q95_nratio_bull, out_s1_nratio_bull,
#                          out_s2_nratio_bull, out_s3_nratio_bull,
#                          out_s4_nratio_bull, out_wy_nratio_bull)
# 
# out_q_nratio_prov <- list(out_q95_nratio_prov, out_s1_nratio_prov,
#                          out_s2_nratio_prov, out_s3_nratio_prov,
#                          out_s4_nratio_prov, out_wy_nratio_prov)


# ----
out_q_ratio_all <- list(out_q95_ratio_all, out_s1_ratio_all,
                         out_s2_ratio_all, out_s3_ratio_all,
                         out_s4_ratio_all, out_wy_ratio_all)

out_q_ratio_bull <- list(out_q95_ratio_bull, out_s1_ratio_bull,
                          out_s2_ratio_bull, out_s3_ratio_bull,
                          out_s4_ratio_bull, out_wy_ratio_bull)

out_q_ratio_prov <- list(out_q95_ratio_prov, out_s1_ratio_prov,
                          out_s2_ratio_prov, out_s3_ratio_prov,
                          out_s4_ratio_prov, out_wy_ratio_prov)


# ----
out_q_regr_diff_all <- list(out_wy_regr_diff_b201, out_wy_regr_diff_b203,
                            out_wy_regr_diff_b204, out_wy_regr_diff_p301,
                            out_wy_regr_diff_d102, out_wy_regr_diff_p303)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Save modeling output


write_rds(out_q_ndiff_all, "output/2.4_mixed_model/out_q_ndiff_all.rds")
write_rds(out_q_ndiff_bull, "output/2.4_mixed_model/out_q_ndiff_bull.rds")
write_rds(out_q_ndiff_prov, "output/2.4_mixed_model/out_q_ndiff_prov.rds")

write_rds(out_q_diff_all, "output/2.4_mixed_model/out_q_diff_all.rds")
write_rds(out_q_diff_bull, "output/2.4_mixed_model/out_q_diff_bull.rds")
write_rds(out_q_diff_prov, "output/2.4_mixed_model/out_q_diff_prov.rds")

# write_rds(out_q_nratio_all, "output/2.4_mixed_model/out_q_nratio_all.rds")
# write_rds(out_q_nratio_bull, "output/2.4_mixed_model/out_q_nratio_bull.rds")
# write_rds(out_q_nratio_prov, "output/2.4_mixed_model/out_q_nratio_prov.rds")

write_rds(out_q_ratio_all, "output/2.4_mixed_model/out_q_ratio_all.rds")
write_rds(out_q_ratio_bull, "output/2.4_mixed_model/out_q_ratio_bull.rds")
write_rds(out_q_ratio_prov, "output/2.4_mixed_model/out_q_ratio_prov.rds")

write_rds(out_q_regr_diff_all, "output/2.4_mixed_model/out_q_regr_diff_all.rds")


