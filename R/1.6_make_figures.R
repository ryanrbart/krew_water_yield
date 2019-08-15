# Make plots

# Types of plots
# NDVI through time
# Paired watershed: NDVI

# Todo
# Maybe change "treated" color label to be "thinned", "fire" and "thinned/fire"

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

krew_annual <- read_rds("output/1.5/krew_annual.rds")
krew_paired <- read_rds("output/1.5/krew_paired.rds")

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


x <- ggplot(data=happy) +
  geom_line(aes(x=year, y=ndvi_annual, linetype=shed, color=control)) +
  #geom_vline(aes(xintercept=2012), color="blue") +
  geom_point(aes(x=year, y=ndvi_annual, shape=shed, color=control)) +
  geom_point(aes(x=ndvi_burn, y=ndvi_annual, size = "Prescribed\nFire"), shape=2, color="black", stroke=0.8) +
  geom_point(aes(x=ndvi_thin, y=ndvi_annual, size = "Thinning"), shape=1, color="black", stroke=0.8) +
  geom_point(aes(x=ndvi_burn, y=ndvi_annual, size = "Prescribed\nFire"), shape=2, color="black", stroke=0.8) +
  labs(title="NDVI Time-Series",
       y = "Watershed Averaged NDVI",
       x = "Year") +
  scale_color_brewer(palette = "Set1", name="Management", labels = c("Treated", "Control")) + 
  scale_linetype(name="Watershed") + 
  scale_shape(name="Watershed") + 
  # http://www.quantide.com/ggplot-multiple-legends-for-the-same-aesthetic/
  scale_size_manual(
    "Fuel Treatment", values=c(3,3),
    guide=guide_legend(override.aes = list(shape=c(2,1)))) +
  facet_grid(location~.) +
  theme_bw(base_size = 12) +
  NULL
ggsave("output/1.6/plot_timeseries_ndvi.jpg", plot=x, width = 8, height = 5)
#plot(x)



# ---------------------------------------------------------------------
# Figure: Paired watershed: NDVI All


# Plot pre and post streamflow for each watershed
x <- ggplot(krew_paired, aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated) +
  # labeller = labeller(.cols = label_parsed, .rows = label_parsed)
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired Watershed: NDVI",
       y = "Treated Watershed:\nAnnual NDVI",
       x = "Control Watershed: Annual NDVI") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi.jpg", plot=x, width = 5, height = 5)
#plot(x)


# ---------------------------------------------------------------------
# Plot pre and post streamflow for Bull watersheds

x <- krew_paired %>% 
  dplyr::filter(location=="bull") %>% 
  ggplot(., aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated) +
  # labeller = labeller(.cols = label_parsed, .rows = label_parsed)
  scale_x_continuous(breaks=c(0.6,0.65,0.7), label=c(0.6,0.65,0.7), limits=c(0.6,0.71)) +
  scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7), label=c(0.55,0.6,0.65,0.7), limits=c(0.53,0.7)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired NDVI: Bull Watersheds",
       y = "Treated Watershed:\nAnnual NDVI",
       x = "Control Watershed: Annual NDVI") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_bull.jpg", plot=x, width = 5, height = 3.5)
#plot(x)


# ---------------------------------------------------------------------
# Plot pre and post streamflow for Prov watersheds




x <- krew_paired %>% 
  dplyr::filter(location=="bull") %>% 
  ggplot(., aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated) +
  # labeller = labeller(.cols = label_parsed, .rows = label_parsed)
  scale_x_continuous(breaks=c(0.6,0.65,0.7), label=c(0.6,0.65,0.7), limits=c(0.6,0.71)) +
  scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7), label=c(0.55,0.6,0.65,0.7), limits=c(0.53,0.7)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired NDVI: Bull Watersheds",
       y = "Treated Watershed:\nAnnual NDVI",
       x = "Control Watershed: Annual NDVI") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_bull.jpg", plot=x, width = 5, height = 3.5)
#plot(x)



