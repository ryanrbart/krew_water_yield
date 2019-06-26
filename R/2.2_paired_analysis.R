# KREW paired watershed analysis

# To-Do
# Possibly incorporate split graph that includes 0 flows (if I can find r package).
# Add black line around each circle and triangle (if possible)
# Possibly differentiate post points that are thinning, burned, or both.


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

pair_mam7 <- read_rds(PAIR_MAM7_RDS)
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_monthly <- read_rds(PAIR_MONTHLY_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)

# ---------------------------------------------------------------------
# Analyze paired_mam7 data


plot_mam7 <- ggplot(pair_mam7, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_grid(.~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="7-day Mean Minimum Flow",
       y="Treated Watershed:\nMAM7 (mm)",
       x="Control Watershed: MAM7 (mm)") +
  theme_bw(base_size = 7) +
  NULL


plot_q95 <- ggplot(pair_q95, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_grid(.~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Q95",
       y = "Treated Watershed:\nQ95 (mm)",
       x = "Control Watershed: Q95 (mm)") +
  theme_bw(base_size = 7) +
  NULL
#ggsave("output/2.2_paired_analysis/paired_q95.jpg", plot=x, width = 7, height = 7)


plot_s1 <- ggplot(pair_seasonal_1, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_grid(.~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="October-December Streamflow",
       y="Treated Watershed:\nOct-Dec Streamflow (mm)",
       x="Control Watershed: Oct-Dec Streamflow (mm)") +
  theme_bw(base_size = 7) +
  NULL
#ggsave("output/2.2_paired_analysis/paired_seasonal.jpg", plot=x, width = 9, height = 7)


plot_s2 <- ggplot(pair_seasonal_2, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_grid(.~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="January-March Streamflow",
       y="Treated Watershed:\nJan-Mar Streamflow (mm)",
       x="Control Watershed: Jan-Mar Streamflow (mm)") +
  theme_bw(base_size = 7) +
  NULL
#ggsave("output/2.2_paired_analysis/paired_seasonal.jpg", plot=x, width = 9, height = 7)


plot_s3 <- ggplot(pair_seasonal_3, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_grid(.~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="April-June Streamflow",
       y="Treated Watershed:\nApr-Jun Streamflow (mm)",
       x="Control Watershed: Apr-Jun Streamflow (mm)") +
  theme_bw(base_size = 7) +
  NULL
#ggsave("output/2.2_paired_analysis/paired_seasonal.jpg", plot=x, width = 9, height = 7)


plot_s4 <- ggplot(pair_seasonal_4, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_grid(.~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="July-September Streamflow",
       y="Treated Watershed:\nJul-Sep Streamflow (mm)",
       x="Control Watershed: Jul-Sep Streamflow (mm)") +
  theme_bw(base_size = 7) +
  NULL
#ggsave("output/2.2_paired_analysis/paired_seasonal.jpg", plot=x, width = 9, height = 7)


# Plot pre and post streamflow for each watershed
plot_wy <- ggplot(pair_wy, aes(x = q_control, y = q_treated)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  facet_grid(.~shed_treated) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Annual Streamflow",
       y = "Treated Watershed:\nAnnual Streamflow (mm)",
       x = "Control Watershed: Annual Streamflow (mm)") +
  theme_bw(base_size = 7) +
  theme(legend.position="bottom") +
  NULL
#ggsave("output/2.2_paired_analysis/paired_wy.jpg", plot=x, width = 7, height = 7)



# ---------------------------------------------------------------------
# Cowplot Time

library(cowplot)

plot_legend <- get_legend(plot_wy)


plot_paired <- cowplot::plot_grid(plot_q95+ theme(legend.position="none"),
                                  plot_s1+ theme(legend.position="none"),
                                  plot_s2+ theme(legend.position="none"),
                                  plot_s3+ theme(legend.position="none"),
                                  plot_s4+ theme(legend.position="none"),
                                  plot_wy+ theme(legend.position="none"),
                                  plot_legend,
                                  nrow=7,
                                  rel_heights = c(1,1,1,1,1,1,0.15))

#plot(plot_paired)

cowplot::save_plot("output/2.2_paired_analysis/plot_paired_all.pdf",
                   plot_paired,
                   ncol=1,
                   nrow=7,
                   base_height=1.4,
                   base_width=8)












# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Regression Analysis

# ---------------------------------------------------------------------
# Analyze paired_mam7 data

# Create nested streamflow data within each watershed
pair_nest <- pair_mam7 %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment_dummy, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
#geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 



# ---------------------------------------------------------------------
# Analyze paired_q95 data

# Create nested streamflow data within each watershed
pair_nest <- pair_q95 %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment_dummy, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
#geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 



# ---------------------------------------------------------------------
# Analyze paired_monthly data

# Create nested streamflow data within each watershed
pair_nest <- pair_monthly %>%
  group_by(shed_treated, Month) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment_dummy, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
#  geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#  geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_wrap(~Month)



# ---------------------------------------------------------------------
# Analyze paired_seasonal data

# Create nested streamflow data within each watershed
pair_nest <- pair_seasonal %>% 
  group_by(shed_treated, Season) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment_dummy, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_grid(Season~.)


# ---------------------------------------------------------------------
# Analyze paired_wy data

# Create nested streamflow data within each watershed
pair_nest <- pair_wy %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + treatment_dummy, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the effect for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) + 
  labs(title="Effect Size for Annual Streamflow Change", y="Estimate", x="Watershed")


