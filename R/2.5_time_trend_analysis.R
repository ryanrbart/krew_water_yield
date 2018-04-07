# KREW time trend analysis


source("R/0_utilities.R")

theme_set(theme_bw(base_size = 14))

# ---------------------------------------------------------------------
# Import data

# From 2.1. Need to save data
QPT

# ---------------------------------------------------------------------
# Analysis

QPT_nest <- QPT %>%
  dplyr::filter(watershed != "P303") %>%  # Note: An error with the number of factors in treatment can be problematic with P303
  group_by(watershed) %>% 
  nest() 

QPT_lm <- QPT_nest %>% 
  mutate(regr = map(data, ~ lm(log(q) ~ log(p_LP) + treatment, data = .)),
         results_terms = map(regr, tidy),
         results_fit = map(regr, glance))
#unnest(QPT_lm, results_fit)
#unnest(QPT_lm, results_terms)

# Plot the p-value for each treatment dummy variable
QPT_results_terms <- QPT_lm %>% 
  unnest(results_terms) %>% 
  dplyr::filter(term=="treatment1")
  #dplyr::filter(term=="t_LP")

ggplot(QPT_results_terms) +
  geom_bar(stat = "identity", aes(x=watershed,y=estimate))

ggplot(QPT_results_terms) +
  geom_bar(stat = "identity", aes(x=watershed,y=p.value)) +
  geom_hline(yintercept = 0.05, linetype=2, color="red", size=.4) 


# Plot the data/model

ggplot(data = QPT, aes(x=p_LP, y=q)) +
  geom_point(aes(color=treatment, shape=treatment), size=3) +
  geom_smooth(method='lm',formula=y~x, se=FALSE, aes(color=treatment)) +
  facet_wrap(~watershed) +
  scale_x_log10() +
  scale_y_log10() +
  scale_shape_discrete(name="Thinning", labels = c("Pre", "Post")) +
  scale_color_brewer(palette = "Set1", name="Thinning", labels = c("Pre", "Post")) +  
  labs(title="Pre and Post-Thinning Annual Streamflow", y="Annual Streamflow (mm)", x="Annual Precipitation (mm)")


  
  
  
  