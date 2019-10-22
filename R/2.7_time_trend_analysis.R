# KREW time trend analysis

# QP and QPT has two precip/temp values (upper and lower gauge) with each streamflow
# value. Data needs to be filtered by upper or lower gauge before analysis

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

QP <- read_rds(QP_WY_RDS)
QPT <- read_rds(QPT_WY_RDS)

# Filter by upper/lower gauge
QP <- dplyr::filter(QP, up_low == "upper")
QPT <- dplyr::filter(QPT, up_low == "upper")

# ---------------------------------------------------------------------
# Analysis of QP data

QP_nest <- QP %>%
  group_by(shed_treated) %>% 
  nest() 

QP_lm <- QP_nest %>% 
  mutate(regr = map(data, ~ lm(log(q) ~ log(precip) + treatment_dummy, data = .)),
         results_terms = map(regr, broom::tidy),
         results_fit = map(regr, broom::glance))
#unnest(QPT_lm, results_fit)
#unnest(QPT_lm, results_terms)

# Plot the p-value for each treatment dummy variable
QP_results_terms <- QP_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1")
  #dplyr::filter(term=="t_LP")

ggplot(QP_results_terms) +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))

ggplot(QP_results_terms) +
  geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 


# Plot the data/model

x <- ggplot(QP, aes(x = precip, y = q)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=3) +
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_wrap(~shed_treated, ncol=2, dir="v") +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment",
                       labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment",
                     labels = c("Pre", "Post")) +  
  labs(title="Pre and Post-Treatment Annual Streamflow",
       y = "Annual Streamflow (mm)",
       x = "Annual Precipitation (mm)") +
  theme_set(theme_bw(base_size = 17)) +
  NULL
ggsave("output/2.7_time_trend/time_trend_wy.jpg", plot=x, width = 7, height = 7)



# ---------------------------------------------------------------------
# Analysis of QPT data
# Need to add temperature to regression equation

QPT_nest <- QPT %>%
  group_by(shed_treated) %>% 
  nest() 

QPT_lm <- QPT_nest %>% 
  mutate(regr = map(data, ~ lm(log(q) ~ log(precip) + treatment_dummy, data = .)),
         results_terms = map(regr, broom::tidy),
         results_fit = map(regr, broom::glance))
#unnest(QPT_lm, results_fit)
#unnest(QPT_lm, results_terms)

# Plot the p-value for each treatment dummy variable
QPT_results_terms <- QPT_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1")
#dplyr::filter(term=="t_LP")

ggplot(QPT_results_terms) +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))

ggplot(QPT_results_terms) +
  geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 


# Plot the data/model

ggplot(data = QPT, aes(x=precip, y=q)) +
  geom_point(aes(color=treatment_dummy, shape=treatment_dummy), size=3) +
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_wrap(~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Thinning", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Thinning", labels = c("Pre", "Post")) +  
  labs(title="Pre and Post-Thinning Annual Streamflow", y="Annual Streamflow (mm)", x="Annual Precipitation (mm)")


  
  
  