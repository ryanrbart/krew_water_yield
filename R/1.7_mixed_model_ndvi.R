# KREW NDVI and ET mixed-modeling

# With rstanarm, and tidybayes

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Import data

krew_paired <- read_rds("output/1.5/krew_paired.rds")


# ---------------------------------------------------------------------
# Mixed modeling with rstanarm

# Inputs
ITER <- 55000
WARMUP <- 5000
CHAINS <- 4
CORES <- 2
THIN <- 10
SEED <- 49
ADAPT_DELTA <- 0.99


# ---------------------------------------------------------------------
# Dummy: Treated variable plus interaction
# The interaction variable is the key to testing for slope (mortality) changes

# Mixed model code - ndvi
function_paired_ndvi_int <- function(DATA, ITER, WARMUP, CHAINS,
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

# out_ndvi_int_all <- function_paired_ndvi_int(DATA=krew_paired, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
#                                                CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_ndvi_int_bull <- function_paired_ndvi_int(DATA=dplyr::filter(krew_paired,location=="Bull"), ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

out_ndvi_int_prov <- function_paired_ndvi_int(DATA=dplyr::filter(krew_paired,location=="Prov"), ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                                 CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)



# ----
# Quick assessment of results
#summary(out_ndvi_int_all, digits=2)
summary(out_ndvi_int_bull, digits=2)
summary(out_ndvi_int_prov, digits=2)



# ---------------------------------------------------------------------
# Save mixed modeling results

write_rds(out_ndvi_int_bull, "output/1.7/out_ndvi_int_bull.rds")
write_rds(out_ndvi_int_prov, "output/1.7/out_ndvi_int_prov.rds")


