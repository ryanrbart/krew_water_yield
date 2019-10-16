# KREW NDVI and ET mixed-modeling

# With rstanarm, and tidybayes

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Import data

krew_paired <- read_rds("output/1.5/krew_paired.rds")


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

# Function for running the mixed model
function1 <- function(DATA, ITER, WARMUP, CHAINS,
                      CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(ndvi_treated ~ ndvi_control + treatment_dummy + 
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

out_ndvi_dummy <- function1(DATA=krew_paired, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

summary(out_ndvi_dummy, digits=2)

# ---------------------------------------------------------------------
# Dummy: Treated variable plus interaction

# Function for running the mixed model
function2 <- function(DATA, ITER, WARMUP, CHAINS,
                      CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer(ndvi_treated ~ ndvi_control + treatment_dummy + ndvi_control*treatment_dummy +
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

out_ndvi_dummy_int <- function2(DATA=krew_paired, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_ndvi_dummy_bull <- function2(DATA=dplyr::filter(krew_paired,location=="bull"), ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                 CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_ndvi_dummy_prov1 <- function2(DATA=dplyr::filter(krew_paired,location=="prov"), ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                 CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

# To get krew_paired_prov, run 1.6 for lines ~120 to 161.
out_ndvi_dummy_prov2 <- krew_paired_prov %>% 
  dplyr::filter(shed_control=="P303") %>% 
  function2(DATA=., ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)



summary(out_ndvi_dummy_int, digits=2)
summary(out_ndvi_dummy_bull, digits=2)
summary(out_ndvi_dummy_prov1, digits=2)
summary(out_ndvi_dummy_prov2, digits=2)

# ---------------------------------------------------------------------
# Save mixed modeling results

write_rds(out_ndvi_dummy, "output/1.7/out_ndvi_dummy.rds")
write_rds(out_ndvi_dummy_int, "output/1.7/out_ndvi_dummy_int.rds")
write_rds(out_ndvi_dummy_bull, "output/1.7/out_ndvi_dummy_bull.rds")
write_rds(out_ndvi_dummy_prov1, "output/1.7/out_ndvi_dummy_prov1.rds")
write_rds(out_ndvi_dummy_prov2, "output/1.7/out_ndvi_dummy_prov2.rds")


