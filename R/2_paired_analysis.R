# KREW paired watershed analysis


source("R/0_utilities.R")

theme_set(theme_bw(base_size = 14))

# ---------------------------------------------------------------------
# Import data

pair_monthly <- read_rds(PAIR_MONTHLY_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)


# ---------------------------------------------------------------------
# Analyze paired_monthly data

# Plot pre and post streamflow for each watershed
#pair_monthly %>% 
#dplyr::filter(pair_monthly, control_shed == "P304") %>% 
#dplyr::filter(pair_monthly, control_shed == "T003") %>% 
ggplot() +
  geom_point(aes(x = log(control_q), y = log(treated_q), shape = factor(treatment), color = factor(treatment))) +
  facet_grid(treated_shed~Month) +
  theme(legend.position="none")

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_monthly %>% 
  group_by(treated_shed, Month) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(treated_q) ~ log(control_q) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=treated_shed,y=estimate)) +
#  geom_bar(stat = "identity", aes(x=treated_shed,y=p.value)) +
#  geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_wrap(~Month)



# ---------------------------------------------------------------------
# Analyze paired_seasonal data

# Plot pre and post streamflow for each watershed
#pair_seasonal %>% 
#dplyr::filter(pair_seasonal, control_shed == "P304") %>% 
#dplyr::filter(pair_seasonal, control_shed == "T003") %>% 
  ggplot() +
  geom_point(aes(x = log(control_q), y = log(treated_q), shape = factor(treatment), color = factor(treatment))) +
  facet_grid(treated_shed~Season) +
  theme(legend.position="none")
  
# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_seasonal %>% 
  group_by(treated_shed, Season) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(treated_q) ~ log(control_q) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=treated_shed,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=treated_shed,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_grid(Season~.)


# ---------------------------------------------------------------------
# Analyze paired_wy data

# Plot pre and post streamflow for each watershed
ggplot(pair_wy) +
  geom_point(aes(x = log(control_q), y = log(treated_q), shape = factor(treatment), color = factor(treatment))) +
  facet_wrap(~treated_shed)

ggplot(pair_wy, aes(x = log(control_q), y = log(treated_q), shape = factor(treatment), color = factor(treatment))) +
  geom_point() +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x) +
  facet_wrap(~treated_shed)

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_wy %>% 
  group_by(treated_shed) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(treated_q) ~ log(control_q) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=treated_shed,y=estimate))
  #geom_bar(stat = "identity", aes(x=treated_shed,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 
  

