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
out_q_ndiff_all <- read_rds("output/2.4_mixed_model/out_q_ndiff_all.rds")
out_q_ndiff_bull <- read_rds("output/2.4_mixed_model/out_q_ndiff_bull.rds")
out_q_ndiff_prov <- read_rds("output/2.4_mixed_model/out_q_ndiff_prov.rds")

out_q_diff_all <- read_rds("output/2.4_mixed_model/out_q_diff_all.rds")
out_q_diff_bull <- read_rds("output/2.4_mixed_model/out_q_diff_bull.rds")
out_q_diff_prov <- read_rds("output/2.4_mixed_model/out_q_diff_prov.rds")

out_q_ratio_all <- read_rds("output/2.4_mixed_model/out_q_ratio_all.rds")
out_q_ratio_bull <- read_rds("output/2.4_mixed_model/out_q_ratio_bull.rds")
out_q_ratio_prov <- read_rds("output/2.4_mixed_model/out_q_ratio_prov.rds")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Mixed model draw function


# Function: paired_ndiff - Non-interaction
generate_draws1 <- function(out_q){
  # Generate the draws for each parameter
  out_q_draws <- purrr::map(out_q, function(x) x %>%
                              tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_diff_n)
  )
  out_q_draws <- bind_rows(out_q_draws, .id="response_variable")
  
  out_q_draws$response_variable <- factor(out_q_draws$response_variable,
                                          levels = c(1,2,3,4,5,6))
  out_q_draws$.chain <- factor(out_q_draws$.chain)
  out_q_draws$.iteration <- factor(out_q_draws$.iteration)
  out_q_draws$.draw <- factor(out_q_draws$.draw)
  return(out_q_draws)
}


# Function: paired_diff - Non-interaction
generate_draws2 <- function(out_q){
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

# Function: paired_ratio - Non-interaction
generate_draws3 <- function(out_q){
  # Generate the draws for each parameter
  out_q_draws <- purrr::map(out_q, function(x) x %>%
                              tidybayes::spread_draws(`(Intercept)`, `log(q_control)`, ndvi_ratio)
  )
  out_q_draws <- bind_rows(out_q_draws, .id="response_variable")
  
  out_q_draws$response_variable <- factor(out_q_draws$response_variable,
                                          levels = c(1,2,3,4,5,6))
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
# paired_ndiff - Non-interaction

out_q_ndiff_all_draws <- generate_draws1(out_q_ndiff_all)
out_q_ndiff_bull_draws <- generate_draws1(out_q_ndiff_bull)
out_q_ndiff_prov_draws <- generate_draws1(out_q_ndiff_prov)

out_q_ndiff_draws <- bind_rows(all_data = out_q_ndiff_all_draws,
                               bull_data = out_q_ndiff_bull_draws,
                               prov_data = out_q_ndiff_prov_draws,
                               .id="model")
out_q_ndiff_draws$model <- factor(out_q_ndiff_draws$model,
                                  levels =c("prov_data", "bull_data", "all_data"))


# -------------------------------
# paired_diff - Non-interaction

out_q_diff_all_draws <- generate_draws2(out_q_diff_all)
out_q_diff_bull_draws <- generate_draws2(out_q_diff_bull)
out_q_diff_prov_draws <- generate_draws2(out_q_diff_prov)

out_q_diff_draws <- bind_rows(all_data = out_q_diff_all_draws,
                              bull_data = out_q_diff_bull_draws,
                              prov_data = out_q_diff_prov_draws,
                              .id="model")
out_q_diff_draws$model <- factor(out_q_diff_draws$model,
                                 levels =c("prov_data", "bull_data", "all_data"))


# -------------------------------
# paired_ratio - Non-interaction

out_q_ratio_all_draws <- generate_draws3(out_q_ratio_all)
out_q_ratio_bull_draws <- generate_draws3(out_q_ratio_bull)
out_q_ratio_prov_draws <- generate_draws3(out_q_ratio_prov)

out_q_ratio_draws <- bind_rows(all_data = out_q_ratio_all_draws,
                              bull_data = out_q_ratio_bull_draws,
                              prov_data = out_q_ratio_prov_draws,
                              .id="model")
out_q_ratio_draws$model <- factor(out_q_ratio_draws$model,
                                 levels =c("prov_data", "bull_data", "all_data"))



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Figures: Mixed modeling output (Betas)


response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)

watershed_id <- c(all_data = "All\nWatersheds",
                  bull_data = "Bull",
                  prov_data = "Providence")

# Plot uncertainty intervals by parameter (paired_ndiff - Non-interaction)
x <- out_q_ndiff_draws %>%
  dplyr::filter(model != "all_data") %>% 
  ggplot(data=., aes(y = model, x = ndvi_diff_n)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Change in streamflow (using difference in nNDVI)",
       x = expression('nNDVI'[diff]~'Coefficient ('*beta*')'),
       y = "Watershed Group") + 
  facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
  theme_tidybayes() +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() + 
  background_grid() +
  xlim(-2.5,2.5) +
  NULL
ggsave("output/2.5_mixed_model_betas/plot_q_ndiff_draws.jpg",plot=x, width = 5, height = 4)



# Plot uncertainty intervals by parameter (paired_diff - Non-interaction)
x <- out_q_diff_draws %>%
  dplyr::filter(model != "all_data") %>% 
  ggplot(data=., aes(y = model, x = ndvi_diff)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Change in streamflow (using difference in NDVI)",
       x = expression('NDVI'[diff]~'Coefficient ('*beta*')'),
       y = "Watershed Group") +  
  facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
  theme_tidybayes() +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() + 
  background_grid() +
  #xlim(-2,2) +
  NULL
ggsave("output/2.5_mixed_model_betas/plot_q_diff_draws.jpg",plot=x, width = 5, height = 4)



# Plot uncertainty intervals by parameter (paired_ratio - Non-interaction)
x <- out_q_ratio_draws %>%
  dplyr::filter(model != "all_data") %>% 
  ggplot(data=., aes(y = model, x = ndvi_ratio)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Change in streamflow (using ratio of NDVI)",
       x = expression('NDVI'[ratio]~'Coefficient ('*beta*')'),
       y = "Watershed Group") + 
  facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
  theme_tidybayes() +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() + 
  background_grid() +
  #xlim(-2,2) +
  NULL
ggsave("output/2.5_mixed_model_betas/plot_q_ratio_draws.jpg",plot=x, width = 5, height = 4)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Save draws data

write_rds(out_q_diff_draws, "output/2.5_mixed_model_betas/out_q_diff_draws.rds")
write_rds(out_q_ndiff_draws, "output/2.5_mixed_model_betas/out_q_ndiff_draws.rds")
write_rds(out_q_ratio_draws, "output/2.5_mixed_model_betas/out_q_ratio_draws.rds")



