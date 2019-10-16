# KREW data import and processsing


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

# Daily streamflow
q_daily <- read_csv("data/daily_discharge_filled_2004_2017.csv", 
                    col_types = cols(
                      P300 = col_double(),
                      B200 = col_double()))

# Remove P300 and B200 since they have missing data and contain nested watersheds
q_daily <- dplyr::select(q_daily, -P300, -B200)

# Daily precipitation
p_daily <-read_csv("data/daily_ppt_2004_2017.csv")

# Daily temperature
t_daily <-read_csv("data/daily_air_temperature_2003_2015.csv")

# Table describing which watersheds are designated as control/treated pairs
treat_control <- read_csv("data/treated_control.csv")
shed_all <- read_csv("data/shed_all.csv")
  
# Treatment summary by wateryear 
treatment_dummy <-read_csv("data/treatment_dummy.csv")
treatment_wy <-read_csv("data/treatment_wy.csv")
thinning_dummy <-read_csv("data/thinning_dummy.csv")
thinning_wy <-read_csv("data/thinning_wy.csv")
prescribed_fire_dummy <-read_csv("data/prescribed_fire_dummy.csv")
prescribed_fire_wy <-read_csv("data/prescribed_fire_wy.csv")

# Import processed NDVI data
krew_paired <- read_rds("output/1.5/krew_paired.rds")
krew_paired <- dplyr::select(krew_paired, shed_treated, year, ndvi_treated, shed_control, ndvi_control)
krew_annual <- read_rds("output/1.5/krew_annual.rds")

# ---------------------------------------------------------------------
# Summarize data to monthly and annual timesteps

# ----
# Streamflow

# Daily
q_daily$Day <- day(mdy(q_daily$Date))
q_daily$Year <- year(mdy(q_daily$Date))

# Create lfobj for functions that require lfstat library
q_lfobj <- lfobj_function(q_daily)

# Mean Annual Minimum-7
# Note: There is an issue with MAM7. The first and last years generate NAs, possibly due to errors at very beginning and end of time-series.
# Note: MAM1 does work, which represents the Mean Annual Minimum daily flow.
q_mam7 <- q_lfobj %>% 
  map(., ~MAM(.x, n=7, yearly=TRUE)) %>% 
  lfstat_MAM(.)

# Q95
q_95 <- q_lfobj %>% 
  map(., ~Qxx(.x, Qxx=95, yearly=TRUE)) %>% 
  lfstat_Qxx(.)

# Monthly
q_monthly <- q_daily %>% 
  dplyr::group_by(WY, Month) %>%
  dplyr::summarise(P301 = sum(P301),
                   P303 = sum(P303),
                   P304 = sum(P304),
                   D102 = sum(D102),
                   B201 = sum(B201),
                   B203 = sum(B203),
                   B204 = sum(B204),
                   T003 = sum(T003),
                   P303 = sum(P303)) %>% 
  ungroup()

# Seasonal
q_seasonal <- q_daily %>% 
  dplyr::group_by(WY, Season) %>%
  dplyr::summarise(P301 = sum(P301),
                   P303 = sum(P303),
                   P304 = sum(P304),
                   D102 = sum(D102),
                   B201 = sum(B201),
                   B203 = sum(B203),
                   B204 = sum(B204),
                   T003 = sum(T003),
                   P303 = sum(P303)) %>% 
  ungroup()

# Wateryear
q_wy <- q_daily %>% 
  dplyr::group_by(WY) %>%
  dplyr::summarise(P301 = sum(P301),
                   P303 = sum(P303),
                   P304 = sum(P304),
                   D102 = sum(D102),
                   B201 = sum(B201),
                   B203 = sum(B203),
                   B204 = sum(B204),
                   T003 = sum(T003),
                   P303 = sum(P303)) %>% 
  ungroup()

# ----
# Precipitation

# Monthly
p_monthly <- p_daily %>% 
  dplyr::group_by(WY, Month) %>%
  dplyr::summarise(p_LP = sum(Lower_Prov),
                   p_UP = sum(Upper_Prov),
                   p_LB = sum(Lower_Bull),
                   p_UB = sum(Upper_Bull)) %>% 
  ungroup()

# Wateryear
p_wy <- p_daily %>% 
  dplyr::group_by(WY) %>%
  dplyr::summarise(p_LP = sum(Lower_Prov),
                   p_UP = sum(Upper_Prov),
                   p_LB = sum(Lower_Bull),
                   p_UB = sum(Upper_Bull)) %>% 
  ungroup()


# ----
# Temperature

t_daily <- t_daily %>% 
  mutate(t_UP = (Tmax_UP + Tmin_UP)/2,
         t_LP = (Tmax_LP + Tmin_LP)/2,
         t_UB = (Tmax_UB + Tmin_UB)/2,
         t_LB = (Tmax_LB + Tmin_LB)/2)


# Monthly
t_monthly <- t_daily %>% 
  dplyr::group_by(WY, Month) %>%
  dplyr::summarise(t_UP = mean(t_UP),
                   t_LP = mean(t_LP),
                   t_UB = mean(t_UB),
                   t_LB = mean(t_LB)) %>% 
  ungroup()

# Wateryear
t_wy <- t_daily %>% 
  dplyr::group_by(WY) %>%
  dplyr::summarise(t_UP = mean(t_UP),
                   t_LP = mean(t_LP),
                   t_UB = mean(t_UB),
                   t_LB = mean(t_LB)) %>% 
  ungroup()

# ---------------------------------------------------------------------
# Process treatment data

treatment_dummy <- gather(treatment_dummy, key = "shed_treated", value = "treatment_dummy",-WY)
treatment_wy <- gather(treatment_wy, key = "shed_treated", value = "treatment_wy",-WY)
thinning_dummy <- gather(thinning_dummy, key = "shed_treated", value = "thinning_dummy",-WY)
thinning_wy <- gather(thinning_wy, key = "shed_treated", value = "thinning_wy",-WY)
prescribed_fire_dummy <- gather(prescribed_fire_dummy, key = "shed_treated", value = "prescribed_fire_dummy",-WY)
prescribed_fire_wy <- gather(prescribed_fire_wy, key = "shed_treated", value = "prescribed_fire_wy",-WY)

treatment_sheds <- treatment_dummy %>% 
  left_join(., treatment_wy, by=c("WY", "shed_treated")) %>% 
  left_join(., thinning_dummy, by=c("WY", "shed_treated")) %>% 
  left_join(., thinning_wy, by=c("WY", "shed_treated")) %>% 
  left_join(., prescribed_fire_dummy, by=c("WY", "shed_treated")) %>% 
  left_join(., prescribed_fire_wy, by=c("WY", "shed_treated"))

treatment_sheds$treatment_dummy <- factor(treatment_sheds$treatment_dummy)
treatment_sheds$treatment_wy <- factor(treatment_sheds$treatment_wy)
treatment_sheds$thinning_dummy <- factor(treatment_sheds$thinning_dummy)
treatment_sheds$thinning_wy <- factor(treatment_sheds$thinning_wy)
treatment_sheds$prescribed_fire_dummy <- factor(treatment_sheds$prescribed_fire_dummy)
treatment_sheds$prescribed_fire_wy <- factor(treatment_sheds$prescribed_fire_wy)

# ---------------------------------------------------------------------
# Combine paired watershed data
# Generates a tibble with for comparing treated and control streamflow 

# Paired watershed - Mean Annual Minimum-7

treat <- map(treat_control$treatment, ~ dplyr::select(q_mam7, ., WY)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ dplyr::select(q_mam7, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_mam7 <- treatment_sheds %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_mam7$q_treated[pair_mam7$q_treated < 0.0001] <- NA
pair_mam7$q_control[pair_mam7$q_control < 0.0001] <- NA

pair_mam7 <- pair_mam7 %>% 
  dplyr::left_join(krew_paired, by=c("WY"="year", "shed_treated", "shed_control")) %>% 
  dplyr::mutate(ndvi_ratio = ndvi_treated/ndvi_control,
                ndvi_diff = ndvi_treated - ndvi_control) %>% 
  dplyr::group_by(shed_treated) %>% 
  dplyr::mutate(ndvi_treated_n = (ndvi_treated - min(ndvi_treated))/(max(ndvi_treated) - min(ndvi_treated)),
                ndvi_control_n = (ndvi_control - min(ndvi_control))/(max(ndvi_control) - min(ndvi_control)),
                ndvi_ratio_n = ndvi_treated_n/ndvi_control_n,
                ndvi_diff_n = ndvi_treated_n - ndvi_control_n) %>% 
  dplyr::ungroup()



# ----
# Paired watershed - Q95
treat <- map(treat_control$treatment, ~ dplyr::select(q_95, ., WY)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ dplyr::select(q_95, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_q95 <- treatment_sheds %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_q95$q_treated[pair_q95$q_treated < 0.0001] <- NA
pair_q95$q_control[pair_q95$q_control < 0.0001] <- NA

pair_q95 <- pair_q95 %>% 
  dplyr::left_join(krew_paired, by=c("WY"="year", "shed_treated", "shed_control")) %>% 
  dplyr::mutate(ndvi_ratio = ndvi_treated/ndvi_control,
                ndvi_diff = ndvi_treated - ndvi_control) %>% 
  dplyr::group_by(shed_treated) %>% 
  dplyr::mutate(ndvi_treated_n = (ndvi_treated - min(ndvi_treated))/(max(ndvi_treated) - min(ndvi_treated)),
                ndvi_control_n = (ndvi_control - min(ndvi_control))/(max(ndvi_control) - min(ndvi_control)),
                ndvi_ratio_n = ndvi_treated_n/ndvi_control_n,
                ndvi_diff_n = ndvi_treated_n - ndvi_control_n) %>% 
  dplyr::ungroup()


# ----
# Paired watershed - Monthly

treat <- map(treat_control$treatment, ~ dplyr::select(q_monthly, ., WY, Month)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY, -Month)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ dplyr::select(q_monthly, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_monthly <- treatment_sheds %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_monthly$q_treated[pair_monthly$q_treated < 0.0001] <- NA
pair_monthly$q_control[pair_monthly$q_control < 0.0001] <- NA

pair_monthly <- pair_monthly %>% 
  dplyr::left_join(krew_paired, by=c("WY"="year", "shed_treated", "shed_control")) %>% 
  dplyr::mutate(ndvi_ratio = ndvi_treated/ndvi_control,
                ndvi_diff = ndvi_treated - ndvi_control) %>% 
  dplyr::group_by(shed_treated) %>% 
  dplyr::mutate(ndvi_treated_n = (ndvi_treated - min(ndvi_treated))/(max(ndvi_treated) - min(ndvi_treated)),
                ndvi_control_n = (ndvi_control - min(ndvi_control))/(max(ndvi_control) - min(ndvi_control)),
                ndvi_ratio_n = ndvi_treated_n/ndvi_control_n,
                ndvi_diff_n = ndvi_treated_n - ndvi_control_n) %>% 
  dplyr::ungroup()


# ----
# Paired watershed - Seasonal

treat <- map(treat_control$treatment, ~ dplyr::select(q_seasonal, ., WY, Season)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY, -Season)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ dplyr::select(q_seasonal, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_seasonal <- treatment_sheds %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_seasonal$q_treated[pair_seasonal$q_treated < 0.0001] <- NA
pair_seasonal$q_control[pair_seasonal$q_control < 0.0001] <- NA

pair_seasonal <- pair_seasonal %>% 
  dplyr::left_join(krew_paired, by=c("WY"="year", "shed_treated", "shed_control")) %>% 
  dplyr::mutate(ndvi_ratio = ndvi_treated/ndvi_control,
                ndvi_diff = ndvi_treated - ndvi_control) %>% 
  dplyr::group_by(shed_treated) %>% 
  dplyr::mutate(ndvi_treated_n = (ndvi_treated - min(ndvi_treated))/(max(ndvi_treated) - min(ndvi_treated)),
                ndvi_control_n = (ndvi_control - min(ndvi_control))/(max(ndvi_control) - min(ndvi_control)),
                ndvi_ratio_n = ndvi_treated_n/ndvi_control_n,
                ndvi_diff_n = ndvi_treated_n - ndvi_control_n) %>% 
  dplyr::ungroup()

# ----
# Paired watershed - WY

treat <- map(treat_control$treatment, ~ dplyr::select(q_wy, ., WY)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ dplyr::select(q_wy, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_wy <- treatment_sheds %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_wy$q_treated[pair_wy$q_treated < 0.0001] <- NA
pair_wy$q_control[pair_wy$q_control < 0.0001] <- NA

pair_wy <- pair_wy %>% 
  dplyr::left_join(krew_paired, by=c("WY"="year", "shed_treated", "shed_control")) %>% 
  dplyr::mutate(ndvi_ratio = ndvi_treated/ndvi_control,
                ndvi_diff = ndvi_treated - ndvi_control) %>% 
  dplyr::group_by(shed_treated) %>% 
  dplyr::mutate(ndvi_treated_n = (ndvi_treated - min(ndvi_treated))/(max(ndvi_treated) - min(ndvi_treated)),
                ndvi_control_n = (ndvi_control - min(ndvi_control))/(max(ndvi_control) - min(ndvi_control)),
                ndvi_ratio_n = ndvi_treated_n/ndvi_control_n,
                ndvi_diff_n = ndvi_treated_n - ndvi_control_n) %>% 
  dplyr::ungroup()





# ---------------------------------------------------------------------
# Combine streamflow, precipitation and temperature for double mass and
# time-trend analysis 

# Generates a tibble with designated pre and post years.
# (may need to add validation years) Based on Biederman2016-WRR

# Organize streamflow by watershed and WY
# Adding providence/bull location for joining P and T later
Q <- map(shed_all$watershed, ~ dplyr::select(q_wy, ., WY)) %>% 
  map(., ~ gather(., key = "watershed", value = "q", -WY)) %>%
  bind_rows() %>% 
  full_join(., dplyr::select(shed_all,watershed,location), by=c("watershed"="watershed"))

# Process precipitation
# Separate precipitation by location (bull/prov) and upper lower 
p_wy_expand <- p_wy %>% 
  tidyr::gather(key = "gauge", value = "precip", p_UP,p_UB,p_LP,p_LB) %>% 
  dplyr::mutate(location = case_when(gauge=="p_UP" ~ "Prov",
                                     gauge=="p_LP" ~ "Prov",
                                     gauge=="p_UB" ~ "Bull",
                                     gauge=="p_LB" ~ "Bull")) %>% 
  dplyr::mutate(up_low = case_when(gauge=="p_UP" ~ "upper",
                                   gauge=="p_LP" ~ "lower",
                                   gauge=="p_UB" ~ "upper",
                                   gauge=="p_LB" ~ "lower")) %>% 
  dplyr::select(-gauge)

# Join precipitation to streamflow. Each watershed is only associated with lower
# and upper preciptiation gauge from its location.
# Also, add NDVI
QP <- full_join(Q, p_wy_expand, by=c("WY","location")) %>% 
  dplyr::left_join(krew_annual, by=c("WY"="year", "watershed"="shed", "location"))


# Note that temperature data goes from 2003-2015 while P and Q are from
# 2014-2017. In order to combine, 2003, 2016 and 2017 are filtered out.
QP1 <- dplyr::filter(QP, WY %in% seq(2004,2015))
t_wy1 <- dplyr::filter(t_wy, WY %in% seq(2004,2015))

# Process temperature
# Separate temperature by location (bull/prov) and upper lower 
t_wy_expand <- t_wy1 %>% 
  tidyr::gather(key = "gauge", value = "temperature", t_UP,t_UB,t_LP,t_LB) %>% 
  dplyr::mutate(location = case_when(gauge=="t_UP" ~ "Prov",
                                     gauge=="t_LP" ~ "Prov",
                                     gauge=="t_UB" ~ "Bull",
                                     gauge=="t_LB" ~ "Bull")) %>% 
  dplyr::mutate(up_low = case_when(gauge=="t_UP" ~ "upper",
                                   gauge=="t_LP" ~ "lower",
                                   gauge=="t_UB" ~ "upper",
                                   gauge=="t_LB" ~ "lower")) %>% 
  dplyr::select(-gauge)

# Join temperature to streamflow. Each watershed is only associated with lower
# and upper temperature gauge from its location.
QPT <- full_join(QP1, t_wy_expand, by=c("WY","location","up_low"))

# ----
# Add fuel treatment indicator to QP and QPT dataframes, then convert to factor
QP <- treatment_sheds %>% 
  left_join(QP, ., by=c("WY", "watershed"="shed_treated"))
QPT <- treatment_sheds %>% 
  left_join(QPT, ., by=c("WY", "watershed"="shed_treated"))


# ---------------------------------------------------------------------
# Save processed data

# Paired watershed
write.csv(pair_mam7, PAIR_MAM7_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_mam7, PAIR_MAM7_RDS)

write.csv(pair_q95, PAIR_Q95_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_q95, PAIR_Q95_RDS)

write.csv(pair_monthly, PAIR_MONTHLY_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_monthly, PAIR_MONTHLY_RDS)

write.csv(pair_seasonal, PAIR_SEASONAL_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_seasonal, PAIR_SEASONAL_RDS)

write.csv(pair_wy, PAIR_WY_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_wy, PAIR_WY_RDS)

write.csv(QP, QP_WY_CSV, row.names = FALSE, quote=FALSE)
write_rds(QP, QP_WY_RDS)

write.csv(QPT, QPT_WY_CSV, row.names = FALSE, quote=FALSE)
write_rds(QPT, QPT_WY_RDS)

