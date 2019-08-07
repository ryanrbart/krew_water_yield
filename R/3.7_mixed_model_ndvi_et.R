# KREW NDVI and ET mixed-modeling

# With rstanarm, and tidybayes

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Import data

krew_paired <- read_rds("output/3.5/krew_paired.rds")


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
paired_ndvi_dummy <- function(DATA, ITER, WARMUP, CHAINS,
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

out_ndvi_dummy <- paired_ndvi_dummy(DATA=krew_paired, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                    CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)


# ---------------------------------------------------------------------
# Save mixed modeling results

write_rds(out_ndvi_dummy, "output/3.7/out_ndvi_dummy.rds")

