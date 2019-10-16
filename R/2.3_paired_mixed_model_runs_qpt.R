# KREW mixed-model analysis - PQT

# With rstanarm, and tidybayes


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

QP <- read_rds(QP_WY_RDS)
QPT <- read_rds(QPT_WY_RDS)

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
# NDVI: QP variable

QP_function1 <- function(DATA, ITER, WARMUP, CHAINS,
                          CORES, THIN, SEED, ADAPT_DELTA){
  # out <- stan_glmer((q) ~ (precip) + ndvi_annual + precip*ndvi_annual +
  out <- stan_glmer((q) ~ (precip) + ndvi_annual +
                      (1 | watershed),
                    data = DATA,
                    family = gaussian, 
                    prior = normal(),
                    prior_intercept = normal(),
                    chains = CHAINS, cores = CORES, seed = SEED,
                    iter = ITER, warmup = WARMUP, thin = THIN,
                    adapt_delta = ADAPT_DELTA)
  return(out)
}

out_wy_QP <- QP_function1(DATA=dplyr::filter(QPT,up_low=="upper"), ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                            CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

summary(out_wy_QP, digits=2)


# ---------------------------------------------------------------------
# NDVI: QPT variable

QPT_function1 <- function(DATA, ITER, WARMUP, CHAINS,
                              CORES, THIN, SEED, ADAPT_DELTA){
  out <- stan_glmer((q) ~ (precip) + ndvi_annual + temperature +
                      (1 | watershed),
                    data = DATA,
                    family = gaussian, 
                    prior = normal(),
                    prior_intercept = normal(),
                    chains = CHAINS, cores = CORES, seed = SEED,
                    iter = ITER, warmup = WARMUP, thin = THIN,
                    adapt_delta = ADAPT_DELTA)
  return(out)
}

out_wy_QPT <- QPT_function1(DATA=QPT, ITER=ITER, WARMUP=WARMUP, CHAINS=CHAINS,
                                        CORES=CORES, THIN=THIN, SEED=SEED, ADAPT_DELTA=ADAPT_DELTA)

summary(out_wy_QPT, digits=2)


# ---------------------------------------------------------------------
# Save modeling output

write_rds(out_wy_QP, "output/2.3_mixed_model/out_qp.rds")
write_rds(out_wy_QPT, "output/2.3_mixed_model/out_qpt.rds")


# ---------------------------------------------------------------------
# Effect size







