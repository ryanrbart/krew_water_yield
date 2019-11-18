# KREW time series and double mass curve analysis


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

QP <- read_rds(QP_WY_RDS)
QPT <- read_rds(QPT_WY_RDS)

# Table describing which watersheds are designated as control/treated pairs
treat_control <-read_csv("data/treated_control.csv")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Time-series plot

happy <- QP %>% 
  left_join(dplyr::select(treat_control, c(treatment, ndvi_thin, ndvi_burn)),
            by = c("watershed"="treatment")) %>% 
  dplyr::mutate(ndvi_thin = as.integer(ndvi_thin)) %>% 
  dplyr::mutate(ndvi_burn = as.integer(ndvi_burn)) %>% 
  dplyr::mutate(ndvi_thin = if_else(WY==ndvi_thin, ndvi_thin, NA_integer_)) %>% 
  dplyr::mutate(ndvi_burn = if_else(WY==ndvi_burn, ndvi_burn, NA_integer_))


# Annual Streamflow
x <- ggplot(data=happy) +
  geom_line(aes(x=WY, y=q, linetype=watershed, color=control)) +
  #geom_vline(aes(xintercept=2012), color="blue") +
  geom_point(aes(x=WY, y=q, shape=watershed, color=control)) +
  geom_point(aes(x=ndvi_burn, y=q, size = "Prescribed\nFire"), shape=2, color="black", stroke=0.8) +
  geom_point(aes(x=ndvi_thin, y=q, size = "Thinning"), shape=1, color="black", stroke=0.8) +
  geom_point(aes(x=ndvi_burn, y=q, size = "Prescribed\nFire"), shape=2, color="black", stroke=0.8) +
  labs(title="Annual Streamflow Time-Series",
       y = "Annual Streamflow (mm)",
       x = "Year") +
  scale_color_brewer(palette = "Set1", name="Management", labels = c("Treated", "Control")) + 
  scale_linetype(name="Watershed") + 
  scale_shape(name="Watershed") + 
  scale_y_log10() +
  # http://www.quantide.com/ggplot-multiple-legends-for-the-same-aesthetic/
  scale_size_manual(
    "Fuel Treatment", values=c(3,3),
    guide=guide_legend(override.aes = list(shape=c(2,1)))) +
  facet_grid(location~.) +
  theme_bw(base_size = 12) +
  NULL
ggsave("output/2.7_timeseries_double_mass/plot_timeseries_q.pdf", plot=x, width = 8, height = 5)










# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Double Mass of Control Streamflow to Treated Streamflow

# Create cumlative total for streamflow
pair_wy_cum <- pair_wy %>% 
  group_by(shed_treated) %>% 
  mutate(treated_cum=cumsum(q_treated), control_cum=cumsum(q_control))

# Add regression line points




# Plot double mass for each watershed
x <- ggplot(pair_wy_cum) +
  geom_point(aes(x = control_cum, y = treated_cum, group=shed_treated, 
                 shape = shed_treated, color = ndvi_diff), size=2) +
  geom_smooth(method='lm',se=FALSE, formula=y~x, 
              aes(x = control_cum, y = treated_cum,
                  group=shed_treated)) +
  scale_color_continuous(name = "Streamflow (mm)",
                         low= "blue", high="green") +  
  facet_wrap(~location, scales="free") +
  theme_bw(base_size = 12) +
  theme(legend.position="bottom") +
  NULL
ggsave("output/2.7_timeseries_double_mass/plot_double_mass.jpg", plot=x, width = 7, height = 7)



colors_Q <- colorRampPalette(c("blue", "#007FFF",
                               "#7FFF7F", "#FF7F00", "red", "#7F0000"))(4)

x <- ggplot(aes(y = q_treated,x =q_control, colour=factor(shed_treated),fill=factor(shed_treated)), data = pair_wy_cum) +
  geom_point(size=5.0)+
  # geom_path(aes(y = Predicted_pre,x =Control, colour=factor(Catchment),fill=factor(Catchment)),size=1.0,data = df_dm_Prov)+
  # geom_path(aes(y = Predicted_post,x =Control, colour=factor(Catchment),fill=factor(Catchment)),size=1.0,data = df_dm_Prov)+
  scale_x_continuous(name="Cumulative Qctrl (x1000 mm)",limits=c(0,5000),breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000),labels =c(0,1,2,3,4,5,6,7,8))+
  scale_y_continuous(name="Cumulative Qtrtm (x1000 mm)",limits=c(0,5000),breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000),labels =c(0,1,2,3,4,5,6,7,8) ) +
  #scale_colour_manual(name="Catchment",values = colors_Q)+
  #scale_fill_manual(name="Catchment",values = colors_Q)+
  annotate("text", label = "(a) Providence", x = 4400, y = 5000, size = 10, colour = "black",family="Times New Roman")+
  theme(plot.title = element_text(size=22, lineheight=.8, face="bold",family="Times New Roman"),strip.text.y = element_text(size = 12, face="bold",colour = "black", angle = 90,family="Times New Roman"))+
  theme(panel.grid.major = element_line(size = 0.15, colour = 'gray93'),panel.grid.minor = element_blank(), panel.background = element_blank())+
  theme(legend.position = c(0.75, 0.25), legend.background = element_rect(colour = NA))+ 
  theme(legend.title = element_text(colour = 'black',size=20,face="bold",family="Times New Roman"),legend.text = element_text(colour = 'black',size=16,family="Times New Roman"))+
  theme(axis.text.x = element_text(colour = 'black',size = 24, angle=0,family="Times New Roman"),axis.title.x = element_text(face="bold",colour = 'white',size = 25,family="Times New Roman"))+
  theme(axis.title.y = element_text(face="bold",colour = 'black',size = 25, angle=90,family="Times New Roman"),axis.text.y = element_text(colour='black',size = 24,family="Times New Roman"),axis.line = element_line(colour ='black', size = 1) )
















# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Regression Analysis

# Create nested streamflow data within each watershed
pair_nest <- pair_wy_cum %>% 
  group_by(shed_treated) %>% 
  nest() 

# Generate regression models for each watershed
pair_lm <- pair_nest %>% 
  mutate(regr = map(data, ~ lm(log(treated_cum) ~ log(control_cum) + treatment_dummy, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(pair_lm, results_fit)
#unnest(pair_lm, results_terms)

# Plot the p-value for each treatment dummy variable
pair_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment_dummy1") %>% 
  ggplot() +
#  geom_bar(stat = "identity", aes(x=shed_treated,y=estimate))
geom_bar(stat = "identity", aes(x=shed_treated,y=p.value)) +
#geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) +
  NULL


