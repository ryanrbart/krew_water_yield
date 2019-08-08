# Make plots

# Types of plots
# NDVI through time
# Paired watershed: NDVI


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

krew_annual <- read_rds("output/3.5/krew_annual.rds")
krew_paired <- read_rds("output/3.5/krew_paired.rds")

# Table describing which watersheds are designated as control/treated pairs
treat_control <-read_csv("data/treated_control.csv")


# ---------------------------------------------------------------------
# Figure: NDVI through time

krew_annual %>% 
  left_join(dplyr::select(treat_control, c(treatment, ndvi_thin)),
            by = c("shed"="treatment", "year"="ndvi_thin"))

happy <- krew_annual %>% 
  left_join(dplyr::select(treat_control, c(treatment, ndvi_thin, ndvi_burn)),
            by = c("shed"="treatment")) %>% 
  dplyr::mutate(ndvi_thin = as.integer(ndvi_thin)) %>% 
  dplyr::mutate(ndvi_burn = as.integer(ndvi_burn)) %>% 
  dplyr::mutate(ndvi_thin = if_else(year==ndvi_thin, ndvi_thin, NA_integer_)) %>% 
  dplyr::mutate(ndvi_burn = if_else(year==ndvi_burn, ndvi_burn, NA_integer_))


# Update to put a circle and triangle on points with treatments (instead of bar)
# Could also change "treated" color label to be "thinned", "fire" and "thinned/fire"

x <- ggplot(data=happy) +
  geom_line(aes(x=year, y=ndvi_annual, linetype=shed, color=control)) +
  #geom_vline(aes(xintercept=2012), color="blue") +
  geom_point(aes(x=year, y=ndvi_annual, shape=shed, color=control)) +
  geom_point(aes(x=ndvi_thin, y=ndvi_annual), shape=1, size=3, color="black", stroke=0.8) +
  geom_point(aes(x=ndvi_burn, y=ndvi_annual), shape=2, size=3, color="black", stroke=0.8) +
  labs(title="NDVI Time-Series",
       y = "Average NDVI",
       x = "Year") +
  scale_color_brewer(palette = "Set1", name="Management", labels = c("Treated", "Control")) + 
  scale_linetype(name="Watershed") + 
  scale_shape(name="Watershed") + 
  facet_grid(location~.) +
  theme_bw(base_size = 10) +
  NULL
ggsave("output/3.6/plot_timeseries_ndvi.jpg", plot=x, width = 8, height = 5)
#plot(x)



# ---------------------------------------------------------------------
# Figure: Paired watershed: NDVI


# Plot pre and post streamflow for each watershed
x <- ggplot(krew_paired, aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
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
  labs(title="Paired Watershed: NDVI",
       y = "Treated Watershed:\nAnnual NDVI",
       x = "Control Watershed: Annual NDVI") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/3.6/plot_paired_ndvi.jpg", plot=x, width = 5, height = 5)
plot(x)


# ---------------------------------------------------------------------
# 


