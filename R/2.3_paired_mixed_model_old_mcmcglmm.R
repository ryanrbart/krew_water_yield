# KREW mixed-model analysis

# Note: Code could be updated using tidybayes!!!

source("R/0_utilities.R")

theme_set(theme_bw(base_size = 14))


# ---------------------------------------------------------------------
# Import data

pair_mam7 <- read_rds(PAIR_MAM7_RDS)
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_monthly <- read_rds(PAIR_MONTHLY_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)


# ---------------------------------------------------------------------
# Center control streamflow

transform_pair_data <- function(x){
  x %>% 
    mutate(q_treated_log = log(q_treated)) %>% 
    mutate(q_control_log = log(q_control)) %>% 
    group_by(shed_treated) %>% 
    mutate(q_control_log_center = q_control_log - mean(q_control_log))
}

pair_mam7 <- transform_pair_data(pair_mam7)
pair_q95 <- transform_pair_data(pair_q95)
pair_wy <- transform_pair_data(pair_wy)


transform_pair_data <- function(x){
  x %>% 
    mutate(q_treated_log = log(q_treated)) %>% 
    mutate(q_control_log = log(q_control)) %>% 
    group_by(shed_treated, Season) %>% 
    mutate(center = mean(q_control_log)) %>% 
    mutate(q_control_log_center = q_control_log - mean(q_control_log))
}

pair_seasonal <- transform_pair_data(pair_seasonal)
pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)

# ---------------------------------------------------------------------

n_runs = 100000

prior <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = diag(2), nu = .002)))


out_mcmc_mam7 <- MCMCglmm(log(q_treated) ~ q_control_log_center + treatment_dummy,
                             random = ~ us(1+q_control_log_center):shed_treated,
                             nitt = n_runs, thin = 20, burnin = 20000,
                             data = pair_mam7, verbose = T,
                             prior=prior,
                             pr=F,saveX=F,saveZ=F)

out_mcmc_q95 <- MCMCglmm(log(q_treated) ~ q_control_log_center + treatment_dummy,
                          random = ~ us(1+q_control_log_center):shed_treated,
                          nitt = n_runs, thin = 20, burnin = 20000,
                          data = pair_q95, verbose = T,
                          prior=prior,
                          pr=F,saveX=F,saveZ=F)

out_mcmc_seasonal_1 <- MCMCglmm(log(q_treated) ~ q_control_log_center + prescribed_fire_dummy,
                              random = ~ us(1+q_control_log_center):shed_treated,
                              nitt = n_runs, thin = 20, burnin = 20000,
                              data = pair_seasonal_1, verbose = T,
                              prior=prior,
                              pr=F,saveX=F,saveZ=F)

out_mcmc_seasonal_2 <- MCMCglmm(log(q_treated) ~ q_control_log_center + treatment_dummy,
                                random = ~ us(1+q_control_log_center):shed_treated,
                                nitt = n_runs, thin = 20, burnin = 20000,
                                data = pair_seasonal_2, verbose = T,
                                prior=prior,
                                pr=F,saveX=F,saveZ=F)

out_mcmc_seasonal_3 <- MCMCglmm(log(q_treated) ~ q_control_log_center + prescribed_fire_dummy,
                                random = ~ us(1+q_control_log_center):shed_treated,
                                nitt = n_runs, thin = 20, burnin = 20000,
                                data = pair_seasonal_3, verbose = T,
                                prior=prior,
                                pr=F,saveX=F,saveZ=F)

out_mcmc_seasonal_4 <- MCMCglmm(log(q_treated) ~ q_control_log_center + treatment_dummy,
                                random = ~ us(1+q_control_log_center):shed_treated,
                                nitt = n_runs, thin = 20, burnin = 20000,
                                data = pair_seasonal_4, verbose = T,
                                prior=prior,
                                pr=F,saveX=F,saveZ=F)

out_mcmc_wy <- MCMCglmm(log(q_treated) ~ q_control_log_center + thinning_dummy + prescribed_fire_dummy,
                        random = ~ us(1+q_control_log_center):shed_treated,
                        nitt = n_runs, thin = 20, burnin = 20000,
                        data = pair_wy, verbose = T,
                        prior=prior,
                        pr=F,saveX=F,saveZ=F)


out_mcmc_wy_interaction <- MCMCglmm(log(q_treated) ~ q_control_log_center +
                                      prescribed_fire_dummy + q_control_log_center:prescribed_fire_dummy,
                                    random = ~ us(1+q_control_log_center):shed_treated,
                                    nitt = n_runs, thin = 20, burnin = 20000,
                                    data = pair_seasonal_2, verbose = T,
                                    prior=prior,
                                    pr=F,saveX=F,saveZ=F)


write_rds(out_mcmc_mam7, "output/2.3_mixed_model/out_mcmc_mam7.rds")
write_rds(out_mcmc_q95, "output/2.3_mixed_model/out_mcmc_q95.rds")
write_rds(out_mcmc_seasonal_1, "output/2.3_mixed_model/out_mcmc_seasonal_1.rds")
write_rds(out_mcmc_seasonal_2, "output/2.3_mixed_model/out_mcmc_seasonal_2.rds")
write_rds(out_mcmc_seasonal_3, "output/2.3_mixed_model/out_mcmc_seasonal_3.rds")
write_rds(out_mcmc_seasonal_4, "output/2.3_mixed_model/out_mcmc_seasonal_4.rds")
write_rds(out_mcmc_wy, "output/2.3_mixed_model/out_mcmc_wy.rds")

beep(4)



# ----------
# Analysis


out_mcmc_mam7 <- read_rds("output/2.3_mixed_model/out_mcmc_mam7.rds")
out_mcmc_q95 <- read_rds("output/2.3_mixed_model/out_mcmc_q95.rds")
out_mcmc_seasonal_1 <- read_rds("output/2.3_mixed_model/out_mcmc_seasonal_1.rds")
out_mcmc_seasonal_2 <- read_rds("output/2.3_mixed_model/out_mcmc_seasonal_2.rds")
out_mcmc_seasonal_3 <- read_rds("output/2.3_mixed_model/out_mcmc_seasonal_3.rds")
out_mcmc_seasonal_4 <- read_rds("output/2.3_mixed_model/out_mcmc_seasonal_4.rds")
out_mcmc_wy <- read_rds("output/2.3_mixed_model/out_mcmc_wy.rds")

#out_mcmc <- out_mcmc_mam7
#out_mcmc <- out_mcmc_q95
#out_mcmc <- out_mcmc_seasonal_1
#out_mcmc <- out_mcmc_seasonal_2
#out_mcmc <- out_mcmc_seasonal_3
#out_mcmc <- out_mcmc_seasonal_4
out_mcmc <- out_mcmc_wy
out_mcmc <- out_mcmc_wy_interaction

# ----------

summary(out_mcmc)
summary(out_mcmc$Sol)
summary(out_mcmc$VCV)

plot(out_mcmc$Sol)
autocorr(out_mcmc$Sol)

plot(out_mcmc$VCV)
autocorr(out_mcmc$VCV)

posterior.mode(out_mcmc$Sol)
posterior.mode(out_mcmc$VCV)

HPDinterval(out_mcmc$Sol, 0.95)
HPDinterval(out_mcmc$VCV, 0.95)


# -------






# Do a plot like the ones in Bart & Tague (fig 5 & 6) with each response variable (e.g. WY, Q95, etc) on left axis.





