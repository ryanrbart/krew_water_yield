# Import and processsing of KREW thinning basal data


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

# Basal raw
basal_data_raw <- read_csv("data/treatments/treatments_basal_data_raw.csv")

# Basal summary
basal_data_sum <- read_csv("data/treatments/treatments_basal_data_summary.csv")


# ---------------------------------------------------------------------
# Analyze raw data

View(basal_data_raw)

basal_data <- basal_data_raw %>% 
  mutate(basal_diff = BA_mean_post-BA_mean_pre) %>% 
  mutate(basal_diff_percent = (basal_diff/BA_mean_pre)*100)

basal_data %>% 
  group_by(Catchment) %>% 
  summarise(ba_pre = mean(BA_mean_pre), ba_post = mean(BA_mean_post), happy = mean(basal_diff))

View(basal_data)

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Analyze summary Data (safeeq preprocessed data in excel)


# ---------------------------------------------------------------------
# Plot basal data

basal_data_stack <- basal_data_sum %>% 
  tidyr::gather("period", "ba", 2,3)

# Need to order the pre and post values to display correctly.
basal_data_stack$period = factor(basal_data_stack$period,
                                 levels=c("ba_pre","ba_post"),
                                 ordered=TRUE)

x <- basal_data_stack %>% 
  dplyr::filter(thin==TRUE) %>% 
  ggplot() +
  geom_col(aes(x=watershed, y=ba, fill=period), position = "dodge") +
  scale_fill_discrete(name="Period", labels = c("2006", "2012")) +
  #scale_color_discrete(name="Thinned", labels = c("No", "Yes")) +  
  labs(title="Pre and Post Basal Area - Thinned", y="Basal Area", x="Watershed") +
  ylim(0,106)
print(x)

x <- basal_data_stack %>% 
  dplyr::filter(thin==FALSE) %>% 
  ggplot() +
  geom_col(aes(x=watershed, y=ba, fill=period), position = "dodge") +
  scale_fill_discrete(name="Period", labels = c("2006", "2012")) +
  #scale_color_discrete(name="Thinned", labels = c("No", "Yes")) +  
  labs(title="Pre and Post Basal Area - Unthinned", y="Basal Area", x="Watershed") +
  ylim(0,106)
print(x)


# ---------------------------------------------------------------------
# Determine average treatment level in each watershed

basal_change <- basal_data_sum %>% 
  dplyr::mutate(ba_change = ba_post - ba_pre, ba_change_percent = ba_post/ba_pre)

basal_change %>% 
  dplyr::filter(thin==TRUE) %>% 
  summarize(avg = mean(ba_change_percent))

ba_unthin_increase <- basal_change %>% 
  dplyr::filter(thin==FALSE) %>% 
  summarize(avg = mean(ba_change_percent))

ba_thin_avg_percent_decrease <- basal_change %>% 
  dplyr::filter(thin==TRUE) %>%
  mutate(ba_post_adj = ba_post / ba_unthin_increase[[1]]) %>% 
  mutate(ba_change_adj = ba_post_adj - ba_pre, 
         ba_change_percent_adj = ba_post_adj/ba_pre) %>% 
  summarize(avg = mean(ba_change_percent_adj))

# Percent thinning decrease in basal area
1-ba_thin_avg_percent_decrease

