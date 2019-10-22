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
# Regression Analysis

# ----
# paired_q95 data

# Create nested streamflow data within each watershed
pair_nest <- pair_q95 %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + ndvi_diff, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="ndvi_diff") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
#geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 



# ----
# paired_seasonal data

# Create nested streamflow data within each watershed
pair_nest <- pair_seasonal %>% 
  group_by(shed_treated, Season) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + ndvi_diff, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="ndvi_diff") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_grid(Season~.)


# ----
# paired_wy data

# Create nested streamflow data within each watershed
pair_nest <- pair_wy %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + ndvi_diff, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the effect for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="ndvi_diff") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) + 
  labs(title="Effect Size for Annual Streamflow Change", y="Estimate", x="Watershed")




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Multiple Regression Analysis

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







