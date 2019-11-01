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

out_q_diff_all <- read_rds("output/2.4_mixed_model/out_q_diff_all.rds")
out_q_diff_bull <- read_rds("output/2.4_mixed_model/out_q_diff_bull.rds")
out_q_diff_prov <- read_rds("output/2.4_mixed_model/out_q_diff_prov.rds")


# Import mixed model draws
out_q_diff_draws <- read_rds("output/2.5_mixed_model_betas/out_q_diff_draws.rds")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Effect size notes: Since the dependent variable in the mixed model is logged,
# the effect on the NDVI variable is raised to e. This means that for a constant
# increase in the beta of the NDVI variable, there will be a constant percent
# increase in the dependent variable (e.g. every 0.05 decrease in NDVI_diff
# produces a 10% increase in treated streamflow).


# Extract the median and intervals for each parameter.

extact_es_values <- function(out, ndvi_chg){
  
  q_diff_median <- purrr::map(out, function(x) x %>%
                                     tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff) %>%
                                     tidybayes::median_qi()
  )
  
  q_diff_median <- q_diff_median %>% 
    bind_rows(.id="response_variable") %>% 
    dplyr::mutate(treatment_es = exp(ndvi_diff * ndvi_chg)*100-100,
                  treatment_es_lower = exp(ndvi_diff.lower * ndvi_chg)*100-100,
                  treatment_es_upper = exp(ndvi_diff.upper * ndvi_chg)*100-100) %>% 
    dplyr::select(response_variable, treatment_es_upper, treatment_es, treatment_es_lower)
  
  return(q_diff_median)
}

# Output is percent change from null model (0 = no change, negative value = negative percent change, positive value = positive percent change)
es_values_bull <- extact_es_values(out=out_q_diff_bull, ndvi_chg = -0.05)
es_values_prov <- extact_es_values(out=out_q_diff_prov, ndvi_chg = -0.05)
print(es_values_bull)
print(es_values_prov)


# For future reference: alternative approach to parameter values
purrr::map(out_q_diff_bull, broom::tidy)
purrr::map(out_q_diff_prov, broom::tidy)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Change draws to effect size and plot


# Assign a fixed point change in NDVI (negative value means that treated watershed is decreasing relative to control)
ndvi_chg <- -0.05

out_q_diff_draws <- out_q_diff_draws %>% 
  dplyr::group_by(model, response_variable) %>% 
  # In below code, ndvi_diff is beta from model. Output is percent change from null model (0 = no change, negative value = negative percent change, positive value = positive percent change)
  dplyr::mutate(treatment_effect_size = exp(ndvi_diff * ndvi_chg)*100-100)


# ----
# Make draws plot

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)

watershed_id <- c(all_data = "All\nWatersheds",
                  bull_data = "Bull",
                  prov_data = "Providence")

# Plot uncertainty intervals by parameter (ndvi_diff)
x <- out_q_diff_draws %>% 
  dplyr::filter(model != "all_data") %>% 
  ggplot(data=., aes(y = model, x = treatment_effect_size)) +
  tidybayes::geom_halfeyeh(color="black", fill="gray55", .width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Change in Streamflow Response Variable",
       x = "Change in Streamflow Response Variable (%)\n given a 0.05 point reduction in NDVI" ,
       y = "Watershed Group") +
  facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
  theme_tidybayes() +
  theme(axis.text.y = element_text(angle = 90, hjust=0.55, vjust=1)) +
  panel_border() + 
  background_grid() +
  #xlim(-100,100) +
  NULL
ggsave("output/2.6_mixed_model_effect_size/plot_q_diff_es_draws.pdf",plot=x, width = 5, height = 4)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Plot showing effect size across of range of NDVI differences

extact_es_values <- function(out, ndvi_chg){
  
  q_diff_median <- purrr::map(out, function(x) x %>%
                                tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff) %>%
                                tidybayes::median_qi()
  )
  
  q_diff_median <- q_diff_median %>% 
    bind_rows(.id="response_variable") %>% 
    expand_grid(., ndvi_chg) %>% 
    dplyr::mutate(treatment_es = exp(ndvi_diff * ndvi_chg)*100-100,
                  treatment_es_lower = exp(ndvi_diff.lower * ndvi_chg)*100-100,
                  treatment_es_upper = exp(ndvi_diff.upper * ndvi_chg)*100-100)
  
  return(q_diff_median)
}

# Extract effect sizes
es_values_bull2 <- extact_es_values(out=out_q_diff_bull, ndvi_chg <- seq(-0.15,0,0.01))    # ndvi_chg is the range along the x axis of plot
es_values_prov2 <- extact_es_values(out=out_q_diff_prov, ndvi_chg <- seq(-0.15,0,0.01))    # ndvi_chg is the range along the x axis of plot

# Combine ES values
es_values <- es_values_bull2 %>% 
  dplyr::bind_rows(.,es_values_prov2, .id="site") %>% 
  dplyr::mutate(site = if_else(site==1, "Bull", "Prov"))


# ----
# Make plot

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)


x <- es_values %>% 
  dplyr::filter(response_variable == 6) %>% 
  ggplot(data=.) +
  geom_ribbon(aes(x=ndvi_chg, ymin=treatment_es_lower, ymax=treatment_es_upper),
              alpha=0.7, fill="#b2df8a") +
  geom_line(aes(x=ndvi_chg, y=treatment_es)) +
  geom_line(aes(x=ndvi_chg, y=treatment_es_lower), linetype=2, color="black") +
  geom_line(aes(x=ndvi_chg, y=treatment_es_upper), linetype=2, color="black") +
  #geom_hline(aes(yintercept=0), color="red") +
  scale_linetype_discrete(name="Watershed\nGroup") +
  labs(x = "Change in NDVI",
       y = "Change in streamflow (%)") +
  facet_wrap(.~site) + 
  theme_bw(base_size = 13) +
  NULL
ggsave("output/2.6_mixed_model_effect_size/plot_deltaQ_vs_ndvi_diff.pdf",plot=x, width = 5.5, height = 3)





