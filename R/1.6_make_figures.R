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
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Figure: Time-series NDVI

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


# NDVI
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


# Normalized NDVI
x <- ggplot(data=happy) +
  geom_line(aes(x=year, y=ndvi_annual_n, linetype=shed, color=control)) +
  #geom_vline(aes(xintercept=2012), color="blue") +
  geom_point(aes(x=year, y=ndvi_annual_n, shape=shed, color=control)) +
  geom_point(aes(x=ndvi_burn, y=ndvi_annual_n, size = "Prescribed\nFire"), shape=2, color="black", stroke=0.8) +
  geom_point(aes(x=ndvi_thin, y=ndvi_annual_n, size = "Thinning"), shape=1, color="black", stroke=0.8) +
  geom_point(aes(x=ndvi_burn, y=ndvi_annual_n, size = "Prescribed\nFire"), shape=2, color="black", stroke=0.8) +
  labs(title="nNDVI Time-Series",
       y = "Watershed Averaged nNDVI",
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
ggsave("output/1.6/plot_timeseries_ndvi_n.jpg", plot=x, width = 8, height = 5)





# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Figure: Paired watershed: NDVI All

watershed_id <- c(
  "P301" = "Treated: P301", "P303" = "Control: P303",
  "P304" = "Control: P304", "D102" = "Treated: D102",
  "B201" = "Treated: B201", "B203" = "Treated: B203",
  "B204" = "Treated: B204", "T003" = "Control: T003"
)


# Plot pre and post-treatment NDVI for all watersheds
x <- ggplot(krew_paired, aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated) +
  scale_x_continuous(breaks=c(0.6,0.65,0.7,0.75,0.8), label=c(0.6,0.65,0.7,0.75,0.8), limits=c(0.6,0.8)) +
  scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7,0.75,0.8), label=c(0.55,0.6,0.65,0.7,0.75,0.8), limits=c(0.53,0.77)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired NDVI: All Watersheds",
       y = "Annual NDVI: Treated Watershed",
       x = "Annual NDVI: Control Watershed") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_all.jpg", plot=x, width = 5, height = 5.5)



# ---------------------------------------------------------------------
# Figure: Paired watershed: NDVI Bull

x <- krew_paired %>% 
  dplyr::filter(location=="Bull") %>% 
  ggplot(., aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated, labeller = labeller(shed_treated = watershed_id)) +
  # labeller = labeller(.cols = label_parsed, .rows = label_parsed)
  scale_x_continuous(breaks=c(0.6,0.65,0.7), label=c(0.6,0.65,0.7), limits=c(0.6,0.71)) +
  scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7), label=c(0.55,0.6,0.65,0.7), limits=c(0.53,0.7)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired NDVI: Bull Watersheds",
       y = "Annual NDVI: Treated Watershed",
       x = "Annual NDVI: Control Watershed (T003)") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_bull.jpg", plot=x, width = 5, height = 3.5)



# ---------------------------------------------------------------------
# Figure: Paired watershed: NDVI Prov

x <- krew_paired %>% 
  dplyr::filter(location=="Prov") %>% 
  ggplot(., aes(x = ndvi_control, y = ndvi_treated, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated, labeller = labeller(shed_treated = watershed_id)) +
  # labeller = labeller(.cols = label_parsed, .rows = label_parsed)
  scale_x_continuous(breaks=c(0.6,0.65,0.7,0.75,0.8), label=c(0.6,0.65,0.7,0.75,0.8), limits=c(0.67,0.8)) +
  scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7,0.75,0.8), label=c(0.55,0.6,0.65,0.7,0.75,0.8), limits=c(0.57,0.77)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired NDVI: Providence Watersheds",
       y = "Annual NDVI: Treated Watershed",
       x = "Annual NDVI: Control Watershed (P304)") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_prov.jpg", plot=x, width = 5, height = 3.5)



# ---------------------------------------------------------------------
# Figure: Paired watershed: nNDVI All

# Plot pre and post-treatment NDVI for all watersheds
x <- ggplot(krew_paired, aes(x = ndvi_control_n, y = ndvi_treated_n, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated) +
  # scale_x_continuous(breaks=c(0.6,0.65,0.7,0.75,0.8), label=c(0.6,0.65,0.7,0.75,0.8), limits=c(0.6,0.8)) +
  # scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7,0.75,0.8), label=c(0.55,0.6,0.65,0.7,0.75,0.8), limits=c(0.53,0.77)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired nNDVI: All Watersheds",
       y = "Annual nNDVI: Treated Watershed",
       x = "Annual nNDVI: Control Watershed") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_n_all.jpg", plot=x, width = 5, height = 5.5)



# ---------------------------------------------------------------------
# Figure: Paired watershed: nNDVI Bull

x <- krew_paired %>% 
  dplyr::filter(location=="Bull") %>% 
  ggplot(., aes(x = ndvi_control_n, y = ndvi_treated_n, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated, labeller = labeller(shed_treated = watershed_id)) +
  # labeller = labeller(.cols = label_parsed, .rows = label_parsed)
  # scale_x_continuous(breaks=c(0.6,0.65,0.7), label=c(0.6,0.65,0.7), limits=c(0.6,0.71)) +
  # scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7), label=c(0.55,0.6,0.65,0.7), limits=c(0.53,0.7)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired nNDVI: Bull Watersheds",
       y = "Annual nNDVI: Treated Watershed",
       x = "Annual nNDVI: Control Watershed (T003)") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_n_bull.jpg", plot=x, width = 5, height = 3.5)



# ---------------------------------------------------------------------
# Figure: Paired watershed: nNDVI Prov

x <- krew_paired %>% 
  dplyr::filter(location=="Prov") %>% 
  ggplot(., aes(x = ndvi_control_n, y = ndvi_treated_n, label=treatment_label)) +
  geom_point(aes(shape = treatment_dummy, color = treatment_dummy), size=2) +
  stat_summary() + 
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment_dummy)) +
  geom_text_repel(size = 2.5) +
  #geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
  facet_wrap(.~shed_treated, labeller = labeller(shed_treated = watershed_id)) +
  # labeller = labeller(.cols = label_parsed, .rows = label_parsed)
  # scale_x_continuous(breaks=c(0.6,0.65,0.7,0.75,0.8), label=c(0.6,0.65,0.7,0.75,0.8), limits=c(0.64,0.8)) +
  # scale_y_continuous(breaks=c(0.55,0.6,0.65,0.7,0.75,0.8), label=c(0.55,0.6,0.65,0.7,0.75,0.8), limits=c(0.57,0.77)) +
  scale_shape_discrete(name="Treatment", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Treatment", labels = c("Pre", "Post")) +  
  labs(title="Paired nNDVI: Providence Watersheds",
       y = "Annual nNDVI: Treated Watershed",
       x = "Annual nNDVI: Control Watershed (P304)") +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 10) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/1.6/plot_paired_ndvi_n_prov.jpg", plot=x, width = 5, height = 3.5)






