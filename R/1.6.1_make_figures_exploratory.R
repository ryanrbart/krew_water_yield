# Make alternative plots for exploration

# Types of plots
# P303 reaction to drought.
# Alternative control watershed for Providence, P303


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

krew_annual <- read_rds("output/3.5/krew_annual.rds")
krew_paired <- read_rds("output/3.5/krew_paired.rds")


# --------------------------------------------------------------------- Figure:
# P303 reaction to drought. Changes Post-treatment period to highlight P303
# behavior during drought, but before prescribe fire in 2016

krew_paired_p303 <- krew_paired
krew_paired_p303$treatment_dummy[110:113] <- 1

krew_paired_p303 <- krew_paired_p303 %>% 
  # dplyr::mutate(treatment_label = if_else(year<2002 | year>2011,year,NA_real_))
  dplyr::mutate(treatment_label = if_else(year>2012,year,NA_real_))

# Plot pre and post streamflow for each watershed
x <- ggplot(krew_paired_p303, aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text(size = 2.5, check_overlap = FALSE) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(~shed_treated, nrow = 2) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired Watershed: NDVI (with P303 labeled as\npost-treatment after 2013)",
       y = "Treated Watershed:\nAnnual NDVI",
       x = "Control Watershed: Annual NDVI") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/3.6/plot_paired_ndvi_p303.jpg", plot=x, width = 5, height = 5)
#plot(x)


# ---------------------------------------------------------------------
# Figure: Alternative control watershed for Providence, P303
# Shows paired watershed figures for P301 and D102 with P303 as control (ignore year 2017)

# Removes P304 control from the tibble
krew_paired_tmp <- krew_paired %>% 
  dplyr::filter(shed_treated != "P303", shed_control != "T003") %>% 
  dplyr::select(-c(shed_control, ndvi_control))

# Isolates P303 so it can be joined as a control
control_new <- krew_paired %>% 
  dplyr::filter(shed_treated == "P303") %>% 
  dplyr::select(c(shed_treated, year, ndvi_treated)) %>% 
  dplyr::rename(shed_control = shed_treated, ndvi_control = ndvi_treated)

# Rearranges the control P304 to be put in the treated position
treated_new <- krew_paired %>% 
  dplyr::filter(shed_treated == "P303") %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-c(shed_treated, ndvi_treated)) %>% 
  dplyr::rename(shed_treated = shed_control, ndvi_treated = ndvi_control)

# Combine everything
krew_paired_new <- dplyr::full_join(krew_paired_tmp, control_new, by="year")
krew_paired_new <- bind_rows(krew_paired_new,
                             dplyr::full_join(treated_new, control_new, by="year"))

# Adjust treatment period
krew_paired_new$treatment_dummy[53:56] <- 1

# Adjust labels
krew_paired_new <- krew_paired_new %>% 
  # dplyr::mutate(treatment_label = if_else(year<2002 | year>2011,year,NA_real_))
  dplyr::mutate(treatment_label = if_else(year>2012,year,NA_real_))

# ---

# Plot pre and post streamflow for each watershed
x <- ggplot(krew_paired_new, aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text(size = 2.5, check_overlap = FALSE) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(~shed_treated, nrow = 1) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired Watershed: NDVI (with P303 as a control\nand P304 labeled as post-treatment after 2013)",
       y = "Treated Watershed:\nAnnual NDVI",
       x = "Control Watershed: Annual NDVI") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/3.6/plot_paired_ndvi_p303_alt_control.jpg", plot=x, width = 5, height = 5)
#plot(x)



# ---------------------------------------------------------------------
# 


