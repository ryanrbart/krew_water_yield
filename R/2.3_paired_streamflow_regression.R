# KREW simple and multiple regression analysis of streamflow plots


source("R/0_utilities.R")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Combine data

response_variable_id <- c(
  `1` = "Q95", `2` = "Oct-Dec", `3` = "Jan-Mar",
  `4` = "Apr-Jun", `5` = "Jul-Sep", `6` = "Annual"
)

pair_q95 <- pair_q95 %>% 
  dplyr::mutate(response_variable = 1)
pair_seasonal_1 <- pair_seasonal_1 %>% 
  dplyr::mutate(response_variable = 2) %>% 
  dplyr::select(-Season)
pair_seasonal_2 <- pair_seasonal_2 %>% 
  dplyr::mutate(response_variable = 3) %>% 
  dplyr::select(-Season)
pair_seasonal_3 <- pair_seasonal_3 %>% 
  dplyr::mutate(response_variable = 4) %>% 
  dplyr::select(-Season)
pair_seasonal_4 <- pair_seasonal_4 %>% 
  dplyr::mutate(response_variable = 5) %>% 
  dplyr::select(-Season)
pair_wy <- pair_wy %>% 
  dplyr::mutate(response_variable = 6)

pair_all <- as_tibble(dplyr::bind_rows(pair_q95, pair_seasonal_1, pair_seasonal_2, pair_seasonal_3, pair_seasonal_4, pair_wy))

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Regression Analysis


# Create nested streamflow data within each watershed
pair_nest <- pair_all %>% 
  group_by(shed_treated, response_variable) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + ndvi_diff, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)


# Plot the p-value for each treatment dummy variable
x <- pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="ndvi_diff") %>% 
  ggplot(data=.) +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  labs(title="Beta Coefficient for NDVI_diff variable in individual multiple regression model", y="Estimate", x="Watershed") +
  #facet_grid(response_variable~.) +
  facet_wrap(.~response_variable, labeller = labeller(.cols=response_variable_id), scales="free_x") + 
  theme_bw(base_size = 9) +
  NULL
ggsave("output/2.3_regression/plot_beta_estimate_by_watershed.pdf",plot=x, width = 7, height = 5)








# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Alternative Regression Analysis

# Compare treated streamflow to difference NDVI variable across all watersheds 
# (aka don't facet the individual watersheds)

summary(lm(log(q_treated)~ndvi_diff_n + log(q_control), data=pair_wy))
plot(q_treated~ndvi_diff_n, data=pair_wy, log="y")

pair_wy_tmp <- dplyr::filter(pair_wy, location=="Bull")
summary(lm(log(q_treated)~ndvi_diff + log(q_control), data=pair_wy_tmp))
plot(q_treated~ndvi_diff, data=pair_wy_tmp, log="y")

pair_wy_tmp <- dplyr::filter(pair_wy, location=="Prov")
summary(lm(log(q_treated)~ndvi_diff + log(q_control), data=pair_wy_tmp))
plot(q_treated~ndvi_diff, data=pair_wy_tmp, log="y")


# For playing with...
pair_wy_tmp <- dplyr::filter(pair_q95, location=="Prov")
summary(lm(log(q_treated)~ndvi_diff_n + log(q_control), data=pair_wy_tmp))
plot(q_treated~ndvi_diff_n, data=pair_wy_tmp, log="y")







