# KREW paired streamflow plots

# To-Do
# Possibly incorporate split graph that includes 0 flows (if I can find r package).


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
# Make paired watershed plots with ndvi_diff_n

plot_q95 <- ggplot(pair_q95, aes(x = q_control, y = q_treated)) +
  geom_point(aes(size = ndvi_diff_n, color = ndvi_diff_n)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +  
  scale_color_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c]),
                         low= "blue", high="green") +  
  labs(title="Q95",
       y = "Treated Watershed:\nQ95 (mm)",
       x = "Control Watershed: Q95 (mm)") +
  NULL


plot_s1 <- ggplot(pair_seasonal_1, aes(x = q_control, y = q_treated)) +
  geom_point(aes(size = ndvi_diff_n, color = ndvi_diff_n)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +  
  scale_color_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c]),
                         low= "blue", high="green") +  
  labs(title="October-December Streamflow",
       y="Treated Watershed:\nOct-Dec Streamflow (mm)",
       x="Control Watershed: Oct-Dec Streamflow (mm)") +
  NULL


plot_s2 <- ggplot(pair_seasonal_2, aes(x = q_control, y = q_treated)) +
  geom_point(aes(size = ndvi_diff_n, color = ndvi_diff_n)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +  
  scale_color_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c]),
                         low= "blue", high="green") +  
  labs(title="January-March Streamflow",
       y="Treated Watershed:\nJan-Mar Streamflow (mm)",
       x="Control Watershed: Jan-Mar Streamflow (mm)") +
  NULL


plot_s3 <- ggplot(pair_seasonal_3, aes(x = q_control, y = q_treated)) +
  geom_point(aes(size = ndvi_diff_n, color = ndvi_diff_n)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +  
  scale_color_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c]),
                         low= "blue", high="green") +  
  labs(title="April-June Streamflow",
       y="Treated Watershed:\nApr-Jun Streamflow (mm)",
       x="Control Watershed: Apr-Jun Streamflow (mm)") +
  NULL


plot_s4 <- ggplot(pair_seasonal_4, aes(x = q_control, y = q_treated)) +
  geom_point(aes(size = ndvi_diff_n, color = ndvi_diff_n)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +  
  scale_color_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c]),
                         low= "blue", high="green") +  
  labs(title="July-September Streamflow",
       y="Treated Watershed:\nJul-Sep Streamflow (mm)",
       x="Control Watershed: Jul-Sep Streamflow (mm)") +
  NULL


# Plot pre and post streamflow for each watershed
plot_wy <- ggplot(pair_wy, aes(x = q_control, y = q_treated)) +
  geom_point(aes(size = ndvi_diff_n, color = ndvi_diff_n)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +  
  scale_color_continuous(name = expression('nNDVI'[t]~'-'~'nNDVI'[c]),
                         low= "blue", high="green") +  
  labs(title="Annual Streamflow",
       y = "Treated Watershed:\nAnnual Streamflow (mm)",
       x = "Control Watershed: Annual Streamflow (mm)") +
  theme(legend.position="bottom") +
  NULL


# ---------------------------------------------------------------------
# Plot all paired watershed plots together

plot_q95_all <- plot_q95 +
  facet_grid(.~shed_treated) +
  theme_bw(base_size = 7)
plot_s1_all <- plot_s1 +
  facet_grid(.~shed_treated) +
  theme_bw(base_size = 7)
plot_s2_all <- plot_s2 +
  facet_grid(.~shed_treated) +
  theme_bw(base_size = 7)
plot_s3_all <- plot_s3 +
  facet_grid(.~shed_treated) +
  theme_bw(base_size = 7)
plot_s4_all <- plot_s4 +
  facet_grid(.~shed_treated) +
  theme_bw(base_size = 7)
plot_wy_all <- plot_wy +
  facet_grid(.~shed_treated) +
  theme_bw(base_size = 7)

plot_legend <- get_legend(plot_wy)

plot_paired_ndvi <- cowplot::plot_grid(plot_q95_all+ theme(legend.position="none"),
                                       plot_s1_all+ theme(legend.position="none"),
                                       plot_s2_all+ theme(legend.position="none"),
                                       plot_s3_all+ theme(legend.position="none"),
                                       plot_s4_all+ theme(legend.position="none"),
                                       plot_wy_all+ theme(legend.position="none"),
                                       plot_legend,
                                       nrow=7,
                                       rel_heights = c(1,1,1,1,1,1,0.35))

# Save the plot
cowplot::save_plot("output/2.2_paired_analysis/plot_paired_q_all.pdf",
                   plot_paired_ndvi,
                   ncol=1,
                   nrow=7,
                   base_height=1.4,
                   base_width=8)


# ---------------------------------------------------------------------
# Plot paired streamflow plots individually by variable type


plot_q95_var <- plot_q95 +
  facet_wrap(.~shed_treated, ncol=3) +
  theme_bw(base_size = 12)
plot_s1_var <- plot_s1 +
  facet_wrap(.~shed_treated, ncol=3) +
  theme_bw(base_size = 12)
plot_s2_var <- plot_s2 +
  facet_wrap(.~shed_treated, ncol=3) +
  theme_bw(base_size = 12)
plot_s3_var <- plot_s3 +
  facet_wrap(.~shed_treated, ncol=3) +
  theme_bw(base_size = 12)
plot_s4_var <- plot_s4 +
  facet_wrap(.~shed_treated, ncol=3) +
  theme_bw(base_size = 12)
plot_wy_var <- plot_wy +
  facet_wrap(.~shed_treated, ncol=3) +
  theme_bw(base_size = 12)

ggsave("output/2.2_paired_analysis/paired_q_q95.jpg", plot=plot_wy_var, width = 7, height = 7)
ggsave("output/2.2_paired_analysis/paired_q_s1.jpg", plot=plot_wy_var, width = 7, height = 7)
ggsave("output/2.2_paired_analysis/paired_q_s2.jpg", plot=plot_wy_var, width = 7, height = 7)
ggsave("output/2.2_paired_analysis/paired_q_s3.jpg", plot=plot_wy_var, width = 7, height = 7)
ggsave("output/2.2_paired_analysis/paired_q_s4.jpg", plot=plot_wy_var, width = 7, height = 7)
ggsave("output/2.2_paired_analysis/paired_q_wy.jpg", plot=plot_wy_var, width = 7, height = 7)



# ---------------------------------------------------------------------
# Plot treated streamflow against ndvi_diff_n with control streamflow as treatment variable



plot_q_q95_alt_x <- ggplot(pair_q95, aes(x = ndvi_diff_n, y = q_treated)) +
  geom_point(aes(size = q_control, color = q_control)) +
  facet_wrap(.~shed_treated, ncol=3) +
  #scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = "Streamflow (mm)") +  
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  labs(title="Q95",
       y = "Treated Watershed:\nQ95 Streamflow (mm)",
       x = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/2.2_paired_analysis/plot_q_q95_alt_x.jpg", plot=plot_q_q95_alt_x, width = 7, height = 7)

plot_q_s1_alt_x <- ggplot(pair_seasonal_1, aes(x = ndvi_diff_n, y = q_treated)) +
  geom_point(aes(size = q_control, color = q_control)) +
  facet_wrap(.~shed_treated, ncol=3) +
  #scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = "Streamflow (mm)") +  
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  labs(title="Oct-Dec Streamflow",
       y = "Treated Watershed:\nOct-Dec Streamflow (mm)",
       x = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/2.2_paired_analysis/plot_q_s1_alt_x.jpg", plot=plot_q_s1_alt_x, width = 7, height = 7)

plot_q_s2_alt_x <- ggplot(pair_seasonal_2, aes(x = ndvi_diff_n, y = q_treated)) +
  geom_point(aes(size = q_control, color = q_control)) +
  facet_wrap(.~shed_treated, ncol=3) +
  #scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = "Streamflow (mm)") +  
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  labs(title="Jan-Mar Streamflow",
       y = "Treated Watershed:\nJan-Mar Streamflow (mm)",
       x = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/2.2_paired_analysis/plot_q_s2_alt_x.jpg", plot=plot_q_s2_alt_x, width = 7, height = 7)

plot_q_s3_alt_x <- ggplot(pair_seasonal_3, aes(x = ndvi_diff_n, y = q_treated)) +
  geom_point(aes(size = q_control, color = q_control)) +
  facet_wrap(.~shed_treated, ncol=3) +
  #scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = "Streamflow (mm)") +  
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  labs(title="Apr-Jun Streamflow",
       y = "Treated Watershed:\nApr-Jun Streamflow (mm)",
       x = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/2.2_paired_analysis/plot_q_s3_alt_x.jpg", plot=plot_q_s3_alt_x, width = 7, height = 7)

plot_q_s4_alt_x <- ggplot(pair_seasonal_4, aes(x = ndvi_diff_n, y = q_treated)) +
  geom_point(aes(size = q_control, color = q_control)) +
  facet_wrap(.~shed_treated, ncol=3) +
  #scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = "Streamflow (mm)") +  
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  labs(title="Jul-Sep Streamflow",
       y = "Treated Watershed:\nJul-Sep Streamflow (mm)",
       x = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/2.2_paired_analysis/plot_q_s4_alt_x.jpg", plot=plot_q_s4_alt_x, width = 7, height = 7)


plot_q_wy_alt_x <- ggplot(pair_wy, aes(x = ndvi_diff_n, y = q_treated)) +
  geom_point(aes(size = q_control, color = q_control)) +
  facet_wrap(.~shed_treated, ncol=3) +
  #scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = "Streamflow (mm)") +  
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  labs(title="Annual Streamflow",
       y = "Treated Watershed:\nAnnual Streamflow (mm)",
       x = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/2.2_paired_analysis/plot_q_wy_alt_x.jpg", plot=plot_q_wy_alt_x, width = 7, height = 7)





# ---------------------------------------------------------------------
# Joe's visual suggestion


ggplot(pair_wy) +
  geom_line(aes(x = ndvi_treated, y = q_treated), color = "black") +
  geom_line(aes(x = ndvi_control, y = q_treated), color = "red") +
  facet_wrap(.~shed_treated, ncol=3) +
  #scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(name = "Streamflow (mm)") +  
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  labs(title="Annual Streamflow",
       y = "Treated Watershed:\nAnnual Streamflow (mm)",
       x = expression('nNDVI'[t]~'-'~'nNDVI'[c])) +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL






# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Misc Analyses



# ---------------------------------------------------------------------
# Compare treated streamflow to normalized difference NDVI variable across all watersheds 
# (aka don't facet the individual watersheds)


summary(lm(log(q_treated)~ndvi_diff_n + log(q_control), data=pair_wy))
plot(q_treated~ndvi_diff_n, data=pair_wy, log="y")

pair_wy_tmp <- dplyr::filter(pair_wy, site=="bull")
summary(lm(log(q_treated)~ndvi_diff_n + log(q_control), data=pair_wy_tmp))
plot(q_treated~ndvi_diff_n, data=pair_wy_tmp, log="y")

pair_wy_tmp <- dplyr::filter(pair_wy, site=="prov")
summary(lm(log(q_treated)~ndvi_diff_n + log(q_control), data=pair_wy_tmp))
plot(q_treated~ndvi_diff_n, data=pair_wy_tmp, log="y")


# For playing with...
pair_wy_tmp <- dplyr::filter(pair_q95, site=="prov")
summary(lm(log(q_treated)~ndvi_diff_n + log(q_control), data=pair_wy_tmp))
plot(q_treated~ndvi_diff_n, data=pair_wy_tmp, log="y")



# ---------------------------------------------------------------------
# Regression Analysis: paired_q95 data

# Create nested streamflow data within each watershed
pair_nest <- pair_q95 %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + ndvi_diff_n, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="ndvi_diff_n") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
#geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 



# ---------------------------------------------------------------------
# Regression Analysis: paired_seasonal data

# Create nested streamflow data within each watershed
pair_nest <- pair_seasonal %>% 
  group_by(shed_treated, Season) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + ndvi_diff_n, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="ndvi_diff_n") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  facet_grid(Season~.)


# ---------------------------------------------------------------------
# Regression Analysis: paired_wy data

# Create nested streamflow data within each watershed
pair_nest <- pair_wy %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(q_treated) ~ log(q_control) + ndvi_diff_n, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the effect for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="ndvi_diff_n") %>% 
  ggplot() +
  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate)) +
  #geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
  #geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) + 
  labs(title="Effect Size for Annual Streamflow Change", y="Estimate", x="Watershed")


