# KREW NDVI and ET mixed-model analysis

# Can effect size be added to plots?


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

out_ndvi_dummy <- read_rds("output/1.7/out_ndvi_dummy.rds")
out_ndvi_dummy_int <- read_rds("output/1.7/out_ndvi_dummy_int.rds")
out_ndvi_dummy_bull <- read_rds("output/1.7/out_ndvi_dummy_bull.rds")
out_ndvi_dummy_prov1 <- read_rds("output/1.7/out_ndvi_dummy_prov1.rds")
out_ndvi_dummy_prov2 <- read_rds("output/1.7/out_ndvi_dummy_prov2.rds")


# ---------------------------------------------------------------------
# Process mixed modeling output

# ----
# Dummy: Treated

# Extract the median (and intervals) for each parameter.
out_ndvi_dummy_median <-  out_ndvi_dummy %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1) %>% 
  median_qi()

# Generate the draws for each parameter
out_ndvi_dummy_draws <- out_ndvi_dummy %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1) %>% 
  dplyr::mutate(response_variable="NDVI")
out_ndvi_dummy_draws$.chain <- factor(out_ndvi_dummy_draws$.chain)
out_ndvi_dummy_draws$.iteration <- factor(out_ndvi_dummy_draws$.iteration)
out_ndvi_dummy_draws$.draw <- factor(out_ndvi_dummy_draws$.draw)

# Change draws to effect size
out_ndvi_dummy_draws <- out_ndvi_dummy_draws %>% 
  dplyr::group_by(response_variable) %>% 
  dplyr::mutate(treatment_effect_size_percent = (exp(treatment_dummy1 * 1) /  exp(treatment_dummy1 * 0))*100 - 100)

# ----
# Bull

# Extract the median (and intervals) for each parameter.
out_ndvi_dummy_bull_median <-  out_ndvi_dummy_bull %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1, `ndvi_control:treatment_dummy1`) %>% 
  median_qi()

# Generate the draws for each parameter
out_ndvi_dummy_bull_draws <- out_ndvi_dummy_bull %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1, `ndvi_control:treatment_dummy1`) %>% 
  dplyr::mutate(response_variable="NDVI")
out_ndvi_dummy_bull_draws$.chain <- factor(out_ndvi_dummy_bull_draws$.chain)
out_ndvi_dummy_bull_draws$.iteration <- factor(out_ndvi_dummy_bull_draws$.iteration)
out_ndvi_dummy_bull_draws$.draw <- factor(out_ndvi_dummy_bull_draws$.draw)

# Change draws to effect size
# out_ndvi_dummy_bull_draws <- out_ndvi_dummy_bull_draws %>% 
#   dplyr::group_by(response_variable) %>% 
#   dplyr::mutate(treatment_effect_size_percent = (exp(treatment_dummy1 * 1) /  exp(treatment_dummy1 * 0))*100 - 100)

# ----
# Prov1

# Extract the median (and intervals) for each parameter.
out_ndvi_dummy_prov1_median <-  out_ndvi_dummy_prov1 %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1, `ndvi_control:treatment_dummy1`) %>% 
  median_qi()

# Generate the draws for each parameter
out_ndvi_dummy_prov1_draws <- out_ndvi_dummy_prov1 %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1, `ndvi_control:treatment_dummy1`) %>% 
  dplyr::mutate(response_variable="NDVI")
out_ndvi_dummy_prov1_draws$.chain <- factor(out_ndvi_dummy_prov1_draws$.chain)
out_ndvi_dummy_prov1_draws$.iteration <- factor(out_ndvi_dummy_prov1_draws$.iteration)
out_ndvi_dummy_prov1_draws$.draw <- factor(out_ndvi_dummy_prov1_draws$.draw)

# Change draws to effect size
# out_ndvi_dummy_prov1_draws <- out_ndvi_dummy_prov1_draws %>% 
#   dplyr::group_by(response_variable) %>% 
#   dplyr::mutate(treatment_effect_size_percent = (exp(treatment_dummy1 * 1) /  exp(treatment_dummy1 * 0))*100 - 100)

# ----
# Prov2 (with P303 as alternative control)

# Extract the median (and intervals) for each parameter.
out_ndvi_dummy_prov2_median <-  out_ndvi_dummy_prov2 %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1, `ndvi_control:treatment_dummy1`) %>% 
  median_qi()

# Generate the draws for each parameter
out_ndvi_dummy_prov2_draws <- out_ndvi_dummy_prov2 %>%
  spread_draws(`(Intercept)`, `ndvi_control`, treatment_dummy1, `ndvi_control:treatment_dummy1`) %>% 
  dplyr::mutate(response_variable="NDVI")
out_ndvi_dummy_prov2_draws$.chain <- factor(out_ndvi_dummy_prov2_draws$.chain)
out_ndvi_dummy_prov2_draws$.iteration <- factor(out_ndvi_dummy_prov2_draws$.iteration)
out_ndvi_dummy_prov2_draws$.draw <- factor(out_ndvi_dummy_prov2_draws$.draw)

# Change draws to effect size
# out_ndvi_dummy_prov2_draws <- out_ndvi_dummy_prov2_draws %>% 
#   dplyr::group_by(response_variable) %>% 
#   dplyr::mutate(treatment_effect_size_percent = (exp(treatment_dummy1 * 1) /  exp(treatment_dummy1 * 0))*100 - 100)


# ----
# Combine Bull, Prov1 and Prov2

out_ndvi_dummy_combine_draws <- bind_rows(Bull=out_ndvi_dummy_bull_draws,
                                          Providence1=out_ndvi_dummy_prov1_draws,
                                          Providence2=out_ndvi_dummy_prov2_draws,
                                          .id="pair_type")
out_ndvi_dummy_combine_draws$pair_type <- factor(out_ndvi_dummy_combine_draws$pair_type,
                                                 levels=c("Providence2","Providence1","Bull"))

# ---------------------------------------------------------------------
# Modeling Checks

# # What variables are there?
# get_variables(out_ndvi_dummy)
# 
# 
# # Plot chains through time
# out_ndvi_dummy_draws %>%      
#   ggplot(data=.) +
#   geom_line(aes(y = treatment_dummy1, x = .iteration, group = .chain, col=.chain)) +
#   NULL
# 
# # Evaluate chains
# shinystan::launch_shinystan(out_ndvi_dummy[[1]])
# 
# # Possibly helpful functions
# rstantools::prior_summary(out_ndvi_dummy[[1]])
# rstantools::posterior_interval(out_ndvi_dummy[[1]])
# rstantools::posterior_predict(out_ndvi_dummy[[1]])


# ---------------------------------------------------------------------
# What is the effect size?

broom::tidy(out_ndvi_dummy)


# ---------------------------------------------------------------------
# Plot mixed modeling output


# Plot uncertainty intervals by parameter (treatment variable)
# Higher values indicate a post-fire increase, lower values a post-fire decrease 
x <- out_ndvi_dummy_draws %>%      
  ggplot(data=., aes(y = response_variable, x = treatment_dummy1)) +
  tidybayes::geom_halfeyeh() +
  geom_vline(xintercept = 0) +
  #scale_y_discrete(labels = c(response_variable_id)) +
  labs(title = "Treatment Variable", x = expression('Coefficient ('*beta*')'), y = "Response Variable") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/1.8/plot_ndvi_dummy.jpg",plot=x, width = 4, height = 4)
plot(x)

watershed_id <- c(Bull = "Bull",
                  Providence1 = "Providence\n(Control: P304)",
                  Providence2 = "Providence\n(Control: P303)")


# Plot uncertainty intervals by parameter (interaction variable for Bull, Prov1 and Prov2)
x <- out_ndvi_dummy_combine_draws %>%      
  ggplot(data=., aes(y = pair_type, x = `ndvi_control:treatment_dummy1`)) +
  tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Change in Paired NDVI Slope", x = expression('Coefficient ('*beta*')'), y = "Watersheds") +   # See post-fire paper and code for x-label 
  theme_tidybayes() +
  panel_border() + 
  background_grid() +
  NULL
ggsave("output/1.8/plot_ndvi_dummy_combine.jpg",plot=x, width = 5, height = 3)
#plot(x)


# ---------------------------------------------------------------------
# Save output
write_rds(out_ndvi_dummy_draws, "output/1.8/out_ndvi_dummy_draws.rds")
write_rds(out_ndvi_dummy_bull_draws, "output/1.8/out_ndvi_dummy_bull_draws.rds")
write_rds(out_ndvi_dummy_prov1_draws, "output/1.8/out_ndvi_dummy_prov1_draws.rds")
write_rds(out_ndvi_dummy_prov2_draws, "output/1.8/out_ndvi_dummy_prov2_draws.rds")




