# KREW paired watershed analysis


source("R/0_utilities.R")

theme_set(theme_bw(base_size = 16))

# ---------------------------------------------------------------------
# Import data

pair_mam7 <- read_rds(PAIR_MAM7_RDS)
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_monthly <- read_rds(PAIR_MONTHLY_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)


# ---------------------------------------------------------------------
# Analyze paired_mam7 data

# Plot pre and post streamflow for each watershed
ggplot(pair_mam7, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = factor(treatment), color = factor(treatment)), size=3) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=factor(treatment))) +
  facet_wrap(~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Thinning", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Thinning", labels = c("Pre", "Post")) +  
  labs(title="Pre and Post-Thinning 7-day Mean Minimum Flow", y="Treated Watershed - MAM7 (mm)", x="Control Watershed - MAM7 (mm)")

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_mam7 %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
#geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 



# ---------------------------------------------------------------------
# Analyze paired_q95 data

# Plot pre and post streamflow for each watershed
ggplot(pair_q95, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = factor(treatment), color = factor(treatment)), size=3) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=factor(treatment))) +
  facet_wrap(~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Thinning", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Thinning", labels = c("Pre", "Post")) +  
  labs(title="Pre and Post-Thinning Q95", y="Treated Watershed - Q95 (mm)", x="Control Watershed - Q95 (mm)")

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_q95 %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
#geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 



# ---------------------------------------------------------------------
# Analyze paired_monthly data

# Plot pre and post streamflow for each watershed
pair_monthly %>% 
#dplyr::filter(pair_monthly, shed_control == "P304") %>% 
#dplyr::filter(pair_monthly, shed_control == "T003") %>% 
ggplot() +
  geom_point(aes(x = log(q_control), y = log(q_treated), shape = factor(treatment), color = factor(treatment))) +
  facet_grid(shed_treated~Month) +
  theme(legend.position="none")

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_monthly %>% 
  group_by(shed_treated, Month) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
#  geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#  geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_wrap(~Month)



# ---------------------------------------------------------------------
# Analyze paired_seasonal data

# Plot pre and post streamflow for each watershed
pair_seasonal %>% 
#dplyr::filter(pair_seasonal, shed_control == "P304") %>% 
#dplyr::filter(pair_seasonal, shed_control == "T003") %>% 
  ggplot(aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = factor(treatment), color = factor(treatment))) +
  facet_grid(shed_treated~Season) +
  theme(legend.position="none")
ggplot(pair_seasonal, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = factor(treatment), color = factor(treatment)), size=3) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=factor(treatment))) +
  facet_grid(shed_treated~Season) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Thinning", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Thinning", labels = c("Pre", "Post")) +  
  labs(title="Pre and Post-Thinning Seasonal Streamflow", y="Treated Watershed - Q (mm)", x="Control Watershed - Q (mm)")

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_seasonal %>% 
  group_by(shed_treated, Season) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_grid(Season~.)


# ---------------------------------------------------------------------
# Analyze paired_wy data

# Plot pre and post streamflow for each watershed
ggplot(pair_wy, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = factor(treatment), color = factor(treatment)), size=3) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=factor(treatment))) +
  facet_wrap(~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Thinning", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Thinning", labels = c("Pre", "Post")) +  
  labs(title="Pre and Post-Thinning Annual Streamflow", y="Treated Watershed - Annual Q (mm)", x="Control Watershed - Annual Q (mm)")

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_wy %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the effect for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) + 
  labs(title="Effect Size for Annual Streamflow Change", y="Estimate", x="Watershed")


