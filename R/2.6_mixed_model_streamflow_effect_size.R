# KREW mixed-model analysis: Effect size


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

# Mixed model input data
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)


# Import mixed model results

out_wy_diff_bull <- read_rds("output/2.4_mixed_model/out_wy_diff_bull.rds")
out_wy_diff_prov <- read_rds("output/2.4_mixed_model/out_wy_diff_prov.rds")
out_QP2_bull <- read_rds("output/2.4_mixed_model/out_QP2_bull.rds")
out_QP2_prov <- read_rds("output/2.4_mixed_model/out_QP2_prov.rds")

# Import mixed model draws
out_q_diff_draws <- read_rds("output/2.5_mixed_model_betas/out_q_diff_draws.rds")
out_QP2_draws <- read_rds("output/2.5_mixed_model_betas/out_QP2_draws.rds")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Effect size notes: Since the dependent variable in the mixed model is logged,
# the effect on the NDVI variable is raised to e. This means that for a constant
# increase in the beta of the NDVI variable, there will be a constant percent
# increase in the dependent variable (e.g. every 0.05 decrease in NDVI_diff
# produces a 10% increase in treated streamflow).


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Plot showing effect size across of range of NDVI differences (paired streamflow model)

extact_es_values <- function(out, ndvi_chg){
  
  q_diff_median <- out %>%
    tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff) %>%
    tidybayes::median_qi()
  
  q_diff_median <- q_diff_median %>% 
    expand_grid(., ndvi_chg) %>% 
    dplyr::mutate(treatment_es = exp(ndvi_diff * ndvi_chg)*100-100,
                  treatment_es_lower = exp(ndvi_diff.lower * ndvi_chg)*100-100,
                  treatment_es_upper = exp(ndvi_diff.upper * ndvi_chg)*100-100)
  
  return(q_diff_median)
}

# Extract effect sizes
es_values_bull2 <- extact_es_values(out=out_wy_diff_bull, ndvi_chg <- seq(-0.15,0,0.01))    # ndvi_chg is the range along the x axis of plot
es_values_prov2 <- extact_es_values(out=out_wy_diff_prov, ndvi_chg <- seq(-0.15,0,0.01))    # ndvi_chg is the range along the x axis of plot

# Combine ES values
es_values <- es_values_bull2 %>% 
  dplyr::bind_rows(.,es_values_prov2, .id="site") %>% 
  dplyr::mutate(site = if_else(site==1, "Bull", "Providence"))


# ----
# Make plot

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)


x_q_model <- es_values %>% 
  ggplot(data=.) +
  geom_ribbon(aes(x=ndvi_chg, ymin=treatment_es_lower, ymax=treatment_es_upper),
              alpha=0.7, fill="#33a02c") +
  geom_line(aes(x=ndvi_chg, y=treatment_es)) +
  geom_line(aes(x=ndvi_chg, y=treatment_es_lower), linetype=2, color="black") +
  geom_line(aes(x=ndvi_chg, y=treatment_es_upper), linetype=2, color="black") +
  #geom_hline(aes(yintercept=0), color="red") +
  scale_linetype_discrete(name="Watershed\nGroup") +
  labs(title = "Paired Streamflow Model",
       x = "Change in NDVI",
       y = "Predicted change in streamflow (%)") +
  facet_wrap(.~site, ncol=1) + 
  theme_bw(base_size = 13) +
  ylim(-62, 310) +
  NULL
ggsave("output/2.6_mixed_model_effect_size/plot_deltaQ_vs_deltaNDVI_q_model.pdf",plot=x_q_model, width = 3, height = 5.5)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Plot showing effect size across of range of NDVI differences (precipitation model)

extact_es_values <- function(out, ndvi_chg){
  
  q_diff_median <-  out %>%
    tidybayes::spread_draws(`(Intercept)`, `log(precip)`, `log(p_lag)`, ndvi_annual) %>%
    tidybayes::median_qi()
  
  q_diff_median <- q_diff_median %>% 
    expand_grid(., ndvi_chg) %>% 
    dplyr::mutate(treatment_es = exp(ndvi_annual * ndvi_chg)*100-100,
                  treatment_es_lower = exp(ndvi_annual.lower * ndvi_chg)*100-100,
                  treatment_es_upper = exp(ndvi_annual.upper * ndvi_chg)*100-100)
  
  return(q_diff_median)
}

# Extract effect sizes
es_values_bull2 <- extact_es_values(out=out_QP2_bull, ndvi_chg <- seq(-0.15,0,0.01))    # ndvi_chg is the range along the x axis of plot
es_values_prov2 <- extact_es_values(out=out_QP2_prov, ndvi_chg <- seq(-0.15,0,0.01))    # ndvi_chg is the range along the x axis of plot

# Combine ES values
es_values <- es_values_bull2 %>% 
  dplyr::bind_rows(.,es_values_prov2, .id="site") %>% 
  dplyr::mutate(site = if_else(site==1, "Bull", "Providence"))


# ----
# Make plot

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)


x_p_model <- es_values %>% 
  #dplyr::filter(response_variable == 6) %>% 
  ggplot(data=.) +
  geom_ribbon(aes(x=ndvi_chg, ymin=treatment_es_lower, ymax=treatment_es_upper),
              alpha=0.7, fill="#33a02c") +
  geom_line(aes(x=ndvi_chg, y=treatment_es)) +
  geom_line(aes(x=ndvi_chg, y=treatment_es_lower), linetype=2, color="black") +
  geom_line(aes(x=ndvi_chg, y=treatment_es_upper), linetype=2, color="black") +
  #geom_hline(aes(yintercept=0), color="red") +
  scale_linetype_discrete(name="Watershed\nGroup") +
  labs(title = "Longitudinal Model",
       x = "Change in NDVI",
       y = "Predicted change in streamflow (%)") +
  facet_wrap(.~site, ncol=1) + 
  theme_bw(base_size = 13) +
  ylim(-62, 310) +
  NULL
ggsave("output/2.6_mixed_model_effect_size/plot_deltaQ_vs_deltaNDVI_p_model.pdf",plot=x_p_model, width = 3, height = 5.5)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Plot showing effect size across of range of NDVI differences (Both models)


plot_x_all <- cowplot::plot_grid(x_q_model,
                                 x_p_model,
                                 ncol=2,
                                 rel_widths = c(1,1))

# Save the plot
cowplot::save_plot("output/2.6_mixed_model_effect_size/plot_deltaQ_vs_deltaNDVI_both_models.pdf",
                   plot_x_all,
                   # ncol=2,
                   base_height=7,
                   base_width=7)






