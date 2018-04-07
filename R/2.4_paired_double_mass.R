# KREW double mass curve analysis


source("R/0_utilities.R")

theme_set(theme_bw(base_size = 14))

# ---------------------------------------------------------------------
# Import data

# Need to run 2.1 for precipitation data

pair_mam7 <- read_rds(PAIR_MAM7_RDS)
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_monthly <- read_rds(PAIR_MONTHLY_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)


# ---------------------------------------------------------------------
# Double Mass of Control Streamflow to Treated Streamflow

# Create cumlative total for streamflow
pair_wy_cum <- pair_wy %>% 
  dplyr::filter(shed_treated != "P303") %>%  # Note: An error with the number of factors in treatment can be problematic with P303
  group_by(shed_treated) %>%
  mutate(treated_cum=cumsum(q_treated), control_cum=cumsum(q_control))

# Plot pre and post streamflow for each watershed
ggplot(pair_wy_cum) +
  #geom_point(aes(x = control_cum, y = treated_cum, shape = treatment, color = treatment), size=3) +
  geom_smooth(method='lm',se=FALSE, formula=y~x, aes(x = control_cum, y = treated_cum, group=treatment, color=treatment)) +
  scale_x_log10() +
  scale_y_log10() +
  xlim(3000, 7000) +
  facet_wrap(~shed_treated)

# ----
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_wy_cum %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  dplyr::filter(shed_treated != "P303") %>%  # Note: An error with the number of factors in treatment can be problematic with P303
  mutate(regr = map(data, ~ lm(log(treated_cum) ~ log(control_cum) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment1") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
#geom_bar(stat = "identity", aes(x=treated_shed,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 



# ---------------------------------------------------------------------
# Double Mass of Treated Precipitation to Treated Streamflow




# Use new QPT data




plot(cumsum(p_daily$Lower_Prov), cumsum(q_daily$P301))

