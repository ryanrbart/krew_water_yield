# KREW mixed-model analysis: Betas

# With rstanarm, and tidybayes


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

out_q_regr_diff_all <- read_rds("output/2.4_mixed_model/out_q_regr_diff_all.rds")

out_QP2_bull <- read_rds("output/2.4_mixed_model/out_QP2_bull.rds")
out_QP2_prov <- read_rds("output/2.4_mixed_model/out_QP2_prov.rds")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Mixed model draw function


# Function: Paired streamflow model
generate_draws1 <- function(out_q){
  # Generate the draws for each parameter
  out_q_draws <- purrr::map(out_q, function(x) x %>%
                              tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff)
  )
  out_q_draws <- bind_rows(out_q_draws, .id="response_variable")
  
  out_q_draws$response_variable <- factor(out_q_draws$response_variable,
                                          levels = c(1,2,3,4,5,6))
  out_q_draws$.chain <- factor(out_q_draws$.chain)
  out_q_draws$.iteration <- factor(out_q_draws$.iteration)
  out_q_draws$.draw <- factor(out_q_draws$.draw)
  return(out_q_draws)
}


# Function: Regression model
# For getting draws for individual watersheds
generate_draws2 <- function(out_q){
  # Generate the draws for each parameter
  out_q_draws <- purrr::map(out_q, function(x) x %>%
                              tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff)
  )
  out_q_draws <- bind_rows(out_q_draws, .id="model")
  
  out_q_draws$model <- factor(out_q_draws$model,
                                          levels = c(1,2,3,4,5,6))
  out_q_draws$.chain <- factor(out_q_draws$.chain)
  out_q_draws$.iteration <- factor(out_q_draws$.iteration)
  out_q_draws$.draw <- factor(out_q_draws$.draw)
  return(out_q_draws)
}


# Function: Precipitation model
generate_draws3 <- function(out_q){
  # Generate the draws for each parameter
  out_q_draws <- out_q %>%
    tidybayes::spread_draws(`(Intercept)`, `log(precip)`, `log(p_lag)`, ndvi_annual)
  
  out_q_draws$.chain <- factor(out_q_draws$.chain)
  out_q_draws$.iteration <- factor(out_q_draws$.iteration)
  out_q_draws$.draw <- factor(out_q_draws$.draw)
  return(out_q_draws)
}


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Run mixed model draw functions and process data

# -------------------------------
# Paired streamflow model

out_q_diff_all_draws <- generate_draws1(out_q_diff_all)
out_q_diff_bull_draws <- generate_draws1(out_q_diff_bull)
out_q_diff_prov_draws <- generate_draws1(out_q_diff_prov)

out_q_diff_draws <- bind_rows(all_data = out_q_diff_all_draws,
                              bull_data = out_q_diff_bull_draws,
                              prov_data = out_q_diff_prov_draws,
                              .id="model")
out_q_diff_draws$model <- factor(out_q_diff_draws$model,
                                 levels =c("prov_data", "bull_data", "all_data"))

# -------------------------------
# Regression model

out_q_regr_diff_all_draws <- generate_draws2(out_q_regr_diff_all)

# Merge out_q_diff_draws with out_q_regr_diff_all_draws
out_q_regr_draws <- out_q_diff_draws %>% 
  dplyr::filter(response_variable == 6) %>% 
  dplyr::select(-response_variable) %>% 
  bind_rows(., out_q_regr_diff_all_draws)

out_q_regr_draws$model <- factor(out_q_regr_draws$model,
                                  levels =c("all_data", "bull_data", "prov_data", "6","5","4","3","2","1"))  

out_q_regr_draws <- out_q_regr_draws %>% 
  dplyr::mutate(site = case_when(model == "prov_data" ~ "Prov",
                                 model == "bull_data" ~ "Bull",
                                 model == "all_data" ~ "None",
                                 model == "6" ~ "Prov",
                                 model == "5" ~ "Prov",
                                 model == "4" ~ "Prov",
                                 model == "3" ~ "Bull",
                                 model == "2" ~ "Bull",
                                 model == "1" ~ "Bull"))


# -------------------------------
# Precipitation model

out_QP2_bull_draws <- generate_draws3(out_q=out_QP2_bull)
out_QP2_prov_draws <- generate_draws3(out_q=out_QP2_prov)

out_QP2_draws <- bind_rows(bull_data = out_QP2_bull_draws,
                           prov_data = out_QP2_prov_draws,
                           .id="model")
out_QP2_draws$model <- factor(out_QP2_draws$model,
                              levels =c("prov_data", "bull_data"))



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Figures: Mixed modeling output (Betas)
# Plot the mixed modeling and regression results

model_id <- c(
  `1` = "B201", `2` = "B203", `3` = "B204",
  `4` = "P301", `5` = "D102", `6` = "P303",
  all_data = "All\nWatersheds",
  bull_data = "Bull",
  prov_data = "Providence"
)


# Plot uncertainty intervals by parameter (paired_diff - Non-interaction)

# Bull
x <- out_q_regr_draws %>%
  dplyr::filter(model %in% c("1", "2", "3", "bull_data")) %>%
  ggplot(data=., aes(y = model, x = ndvi_diff)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(model_id)) +
  labs(title = "Change in streamflow",
       x = expression('NDVI'[diff]~'Coefficient ('*beta*')'),
       y = "Watershed Group") +
  theme_tidybayes() +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() +
  background_grid() +
  #xlim(-2,2) +
  NULL
ggsave("output/2.5_mixed_model_betas/plot_q_regr_diff_draws_bull.jpg",plot=x, width = 4, height = 2.5)

# Prov
x <- out_q_regr_draws %>%
  dplyr::filter(model %in% c("4", "5", "6", "prov_data")) %>%
  ggplot(data=., aes(y = model, x = ndvi_diff)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(model_id)) +
  labs(title = "Change in streamflow",
       x = expression('NDVI'[diff]~'Coefficient ('*beta*')'),
       y = "Watershed Group") +
  theme_tidybayes() +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() +
  background_grid() +
  #xlim(-2,2) +
  NULL
ggsave("output/2.5_mixed_model_betas/plot_q_regr_diff_draws_prov.jpg",plot=x, width = 4, height = 2.5)


# Cowplot together
# Forthcoming


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Figures: Mixed modeling output (Betas)
# Plot only the mixed modeling results

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)

watershed_id <- c(all_data = "All\nWatersheds",
                  bull_data = "Bull",
                  prov_data = "Providence")


# Plot uncertainty intervals by parameter (paired_diff - Non-interaction)
plot_q_diff_draws <- out_q_diff_draws %>%
  dplyr::filter(model != "all_data", response_variable == 6) %>%
  ggplot(data=., aes(y = model, x = ndvi_diff)) +
  ggridges::geom_density_ridges(scale = 0.9, fill="#33a02c", color=NA, alpha=0.7) +
  stat_pointintervalh(.width = c(0.9, 0.95)) +
  #tidybayes::geom_halfeyeh(.width = c(0.9, 0.95), fill="#33a02c") +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Paired Streamflow Model",
       x = expression('NDVI'[diff]~'Coefficient ('*beta[0]*')'),
       y = "Watershed Group") +
  theme_bw(base_size = 9) +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() +
  background_grid() +
  xlim(-12,10) +
  NULL
ggsave("output/2.5_mixed_model_betas/plot_q_diff_draws.jpg",plot=x, width = 4, height = 2.5)
write_rds(x, "output/2.5_mixed_model_betas/plot_q_diff_draws.rds")



# Plot uncertainty intervals by parameter
plot_QP2_draws <- out_QP2_draws %>%
  dplyr::filter(model != "all_data") %>%
  ggplot(data=., aes(y = model, x = ndvi_annual)) +
  ggridges::geom_density_ridges(scale = 0.9, fill="#33a02c", color=NA, alpha=0.7) +
  stat_pointintervalh(.width = c(0.9, 0.95)) +
  #tidybayes::geom_halfeyeh(.width = c(0.9, 0.95), fill="#33a02c") +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Precipitation Model",
       x = expression('NDVI'[annual]~'Coefficient ('*beta[0]*')'),
       y = "Watershed Group") +
  theme_bw(base_size = 9) +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() +
  background_grid() +
  xlim(-12,10) +
  NULL
ggsave("output/2.5_mixed_model_betas/plot_q_precip_draws.jpg",plot=plot_QP2_draws, width = 4, height = 2.5)
write_rds(x, "output/2.5_mixed_model_betas/plot_q_precip_draws.rds")


# ---------------------------------------------------------------------
# Cowplot: Combine plot with paired streamflow plot

# Make cowplot
plot_q_beta <- cowplot::plot_grid(plot_q_diff_draws,
                                  plot_QP2_draws,
                                  ncol=1)


save_plot("output/2.5_mixed_model_betas/plot_q_beta.pdf",
          plot=plot_q_beta,
          ncol = 1,
          base_height = 4.5,
          base_width = 2.5)












# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Plot 6 significance of NDVI for predicting 6 streamflow variables 

# # Plot uncertainty intervals by parameter (paired_ndiff - Non-interaction)
# x <- out_q_ndiff_draws %>%
#   dplyr::filter(model != "all_data") %>% 
#   ggplot(data=., aes(y = model, x = ndvi_diff_n)) +
#   tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
#   geom_vline(xintercept = 0) +
#   scale_y_discrete(labels = c(watershed_id)) +
#   labs(title = "Change in streamflow (using difference in nNDVI)",
#        x = expression('nNDVI'[diff]~'Coefficient ('*beta*')'),
#        y = "Watershed Group") + 
#   facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
#   theme_tidybayes() +
#   theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
#   panel_border() + 
#   background_grid() +
#   xlim(-2.5,2.5) +
#   NULL
# ggsave("output/2.5_mixed_model_betas/plot_q_ndiff_draws.jpg",plot=x, width = 5, height = 4)
# 
# 
# 
# # Plot uncertainty intervals by parameter (paired_diff - Non-interaction)
# x <- out_q_diff_draws %>%
#   dplyr::filter(model != "all_data") %>% 
#   ggplot(data=., aes(y = model, x = ndvi_diff)) +
#   tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
#   geom_vline(xintercept = 0) +
#   scale_y_discrete(labels = c(watershed_id)) +
#   labs(title = "Change in streamflow (using difference in NDVI)",
#        x = expression('NDVI'[diff]~'Coefficient ('*beta*')'),
#        y = "Watershed Group") +  
#   facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
#   theme_tidybayes() +
#   theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
#   panel_border() + 
#   background_grid() +
#   #xlim(-2,2) +
#   NULL
# ggsave("output/2.5_mixed_model_betas/plot_q_diff_draws.jpg",plot=x, width = 5, height = 4)
# 
# 
# 
# # Plot uncertainty intervals by parameter (paired_ratio - Non-interaction)
# x <- out_q_ratio_draws %>%
#   dplyr::filter(model != "all_data") %>% 
#   ggplot(data=., aes(y = model, x = ndvi_ratio)) +
#   tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
#   geom_vline(xintercept = 0) +
#   scale_y_discrete(labels = c(watershed_id)) +
#   labs(title = "Change in streamflow (using ratio of NDVI)",
#        x = expression('NDVI'[ratio]~'Coefficient ('*beta*')'),
#        y = "Watershed Group") + 
#   facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
#   theme_tidybayes() +
#   theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
#   panel_border() + 
#   background_grid() +
#   #xlim(-2,2) +
#   NULL
# ggsave("output/2.5_mixed_model_betas/plot_q_ratio_draws.jpg",plot=x, width = 5, height = 4)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Save draws data

write_rds(out_q_diff_draws, "output/2.5_mixed_model_betas/out_q_diff_draws.rds")
# write_rds(out_q_ndiff_draws, "output/2.5_mixed_model_betas/out_q_ndiff_draws.rds")
# write_rds(out_q_ratio_draws, "output/2.5_mixed_model_betas/out_q_ratio_draws.rds")


