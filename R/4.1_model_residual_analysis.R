# Model residual analysis of NDVI, ET and Q

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data


out_ndvi_dummy <- read_rds("output/3.7/out_ndvi_dummy.rds")
out_treated_dummy <- read_rds(TREATED_DUMMY_RDS)

krew_paired <- read_rds("output/3.5/krew_paired.rds")
pair_wy <- read_rds(PAIR_WY_RDS)

# ---------------------------------------------------------------------
# Helpful functions

# Note: The summary from the individual runs and summary functions don't match
# but are close. Unclear why, but likely to have little influence on results

# # Residual error (aka residuals)
# summary(as_tibble(predictive_error(out_ndvi_dummy)))
# out_ndvi_dummy$residuals
# 
# # Prediction
# purrr::map(as_tibble(rstanarm::posterior_predict(out_ndvi_dummy)), mean)
# out_ndvi_dummy$fitted.values
# 
# # Dependent variable
# out_ndvi_dummy$y
# 
# # Coefficients
# out_ndvi_dummy$coefficients
# 
# # Residuals for logged Q
# exp(out_treated_dummy[[6]]$fitted.values)
# exp(out_treated_dummy[[6]]$residuals) # This doesn't work with logged dependent variables.
# exp(out_treated_dummy[[6]]$y)
# 
# exp(out_treated_dummy[[6]]$y - out_treated_dummy[[6]]$fitted.values) # This matches the model residuals, but is incorrect
# exp(out_treated_dummy[[6]]$y) - exp(out_treated_dummy[[6]]$fitted.values) # This is the correct residuals


# ---------------------------------------------------------------------
# Combine data and model results for NDVI and Q

# Note that this section is for residuals from the full model, so the residuals
# for some of the watersheds are much higher than would be inferred from looking
# at the paired watershed plots.

# # Attach model results to NDVI input table
# paired_model_ndvi <- krew_paired %>% 
#   dplyr::bind_cols(ndvi_fitted = out_ndvi_dummy$fitted.values,
#                    ndvi_residual = out_ndvi_dummy$y - out_ndvi_dummy$fitted.values) %>% 
#   dplyr::select(-c("treatment_dummy", "treatment_wy",   # Remove these so there are no duplicates when merging with paired_model_q
#                    "thinning_dummy", "thinning_wy", 
#                    "prescribed_fire_dummy","prescribed_fire_wy"))
# 
# # Attach model results to Q input table
# paired_model_q <- pair_wy %>% 
#   dplyr::bind_cols(q_fitted = exp(out_treated_dummy[[6]]$fitted.values),
#                    q_residual = exp(out_treated_dummy[[6]]$y) - exp(out_treated_dummy[[6]]$fitted.values))
# 
# # Join the paired watershed tables for streamflow and NDVI 
# paired_all <- dplyr::left_join(paired_model_q, 
#                                paired_model_ndvi,
#                                by=c("WY"="year", "shed_treated", "shed_control"))

# paired_all <- paired_all %>% 
#   tidyr::gather(key="model_resid", value="model_resid_value", ndvi_residual, q_residual)



# ---------------------------------------------------------------------
# Derive paired watershed residuals for watershed-level regression models

# Create nested streamflow data within each watershed
q_nest <- pair_wy %>% 
  dplyr::group_by(shed_treated) %>% 
  tidyr::nest()

# Generate regression models for each watershed
q_resid_tmp <- q_nest$data %>% 
  map(~augment(lm(log(q_treated) ~ log(q_control) + treatment_dummy, data = .))) %>% 
  setNames(q_nest$shed_treated) %>% 
  bind_rows(.id = "watershed") %>% 
  bind_cols(WY = pair_wy$WY, q_treated = pair_wy$q_treated, q_control = pair_wy$q_control) %>% 
  dplyr::select(-c(.se.fit, .hat, .sigma, .cooksd, .std.resid)) %>% 
  dplyr::rename(q_fitted = .fitted, q_resid = .resid)

q_lm <- q_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment_dummy, data = .)),
         results_terms = map(regr, broom::tidy),
         results_fit = map(regr, broom::glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# ---
# Change the treatment_dummy variable within q_nest and rename
q_nest_pred <- pair_wy %>%
  dplyr::mutate(treatment_dummy = factor(rep(0,nrow(.)))) %>% 
  dplyr::group_by(shed_treated) %>% 
  tidyr::nest()

# Make prediction (should this be the dummy model or just pre-treatment???)
q_predict <- purrr::map2(q_lm$regr, q_nest_pred$data,
                         function(.x,.y)predict(.x,.y)) %>% 
  unlist() %>% 
  tibble::enframe(name = NULL) %>% 
  rename(q_predict_log = value)

q_resid <- q_resid_tmp %>% 
  bind_cols(q_predict) %>% 
  dplyr::mutate(q_predict_resid = exp(log.q_treated.) - exp(q_predict_log))


# ----

# Create nested streamflow data within each watershed
ndvi_nest <- krew_paired %>% 
  dplyr::group_by(shed_treated) %>% 
  tidyr::nest() 

# Generate regression models for each watershed
ndvi_resid_tmp <- ndvi_nest$data %>% 
  map(~augment(lm(ndvi_treated ~ ndvi_control + treatment_dummy, data = .))) %>% 
  setNames(ndvi_nest$shed_treated) %>% 
  bind_rows(.id = "watershed") %>% 
  bind_cols(year = krew_paired$year) %>% 
  dplyr::select(-c(.se.fit, .hat, .sigma, .cooksd, .std.resid)) %>% 
  dplyr::rename(ndvi_fitted = .fitted, ndvi_resid = .resid)

ndvi_lm <- ndvi_nest %>% 
  mutate(regr = map(data, ~ lm(ndvi_treated ~ ndvi_control + treatment_dummy, data = .)),
         results_terms = map(regr, broom::tidy),
         results_fit = map(regr, broom::glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# ---
# Change the treatment_dummy variable within q_nest and rename
ndvi_nest_pred <- krew_paired %>%
  ungroup() %>% 
  dplyr::mutate(treatment_dummy = factor(rep(0,nrow(.)))) %>% 
  dplyr::group_by(shed_treated) %>% 
  tidyr::nest()

# Make prediction (should this be the dummy model or just pre-treatment???)
ndvi_predict <- purrr::map2(ndvi_lm$regr, ndvi_nest_pred$data,
                         function(.x,.y)predict(.x,.y)) %>% 
  unlist() %>% 
  tibble::enframe(name = NULL) %>% 
  rename(ndvi_predict = value)

ndvi_resid <- ndvi_resid_tmp %>% 
  bind_cols(ndvi_predict) %>% 
  dplyr::mutate(ndvi_predict_resid = ndvi_treated - ndvi_predict)



# ---------------------------------------------------------------------
# Combine ndvi and q residual/fitted results 

# Join the paired watershed tables for streamflow and NDVI
paired_all <- dplyr::left_join(q_resid,
                               ndvi_resid,
                               by=c("WY"="year", "watershed","treatment_dummy"))


# ---------------------------------------------------------------------
# Correlation between ndvi and Q


# paired_all %>% 
#   dplyr::group_by(watershed, treatment_dummy) %>% 
#   dplyr::summarise(correlation = cor(ndvi_resid, 
#                                      q_resid,
#                                      use = "complete.obs"))


paired_all %>% 
  dplyr::group_by(watershed, treatment_dummy) %>% 
  dplyr::summarise(correlation = cor(ndvi_predict_resid, 
                                     q_predict_resid,
                                     use = "complete.obs"))

# ---------------------------------------------------------------------
# Plot (for now. Later move to 4.2)

x <- paired_all %>% 
  ggplot(.) +
  geom_line(aes(x=WY, y=q_predict_resid)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2012, linetype=2, col="red") +
  scale_y_reverse() +
  facet_wrap(.~watershed, nrow=1) +
  xlim(2004,2017) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  NULL
#plot(x)

y <- paired_all %>% 
  ggplot(.) +
  geom_line(aes(x=WY, y=ndvi_predict_resid)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 2012, linetype=2, col="red") +
  facet_wrap(.~watershed, nrow=1) +
  xlim(2004,2017) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  NULL
#plot(y)

# Cowplot
plot_comparison_ndvi_q <- cowplot::plot_grid(y,
                                 x,
                                 #labels=c("a","b"),
                                 nrow=2,
                                 label_size = 14,
                                 label_x = 0.95,
                                 label_y = 1)
#plot(plot_comparison_ndvi_q)



# ---------------------------------------------------------------------
# Save 

cowplot::save_plot("output/4.1/plot_comparison_ndvi_q.pdf",
                   plot_comparison_ndvi_q,
                   ncol=1,
                   nrow=2,
                   base_height=2,
                   base_width=8)


# ---------------------------------------------------------------------






rstanarm::posterior_predict(out_ndvi_dummy)

summary(as_tibble(predictive_error(out_ndvi_dummy)))

predictive_interval(out_ndvi_dummy)



fit <- stan_glm(mpg ~ wt, data = mtcars, iter = 300)
preds <- posterior_predict(fit, seed = 123)
all.equal(
  predictive_error(fit, seed = 123),
  predictive_error(preds, y = fit$y)
)








