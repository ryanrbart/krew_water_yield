# KREW data import and processsing


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

# Daily streamflow
q_daily <- read_csv("data/daily_discharge_filled_2003_2016.csv", 
                    col_types = cols(
                      P300 = col_double(),
                      B200 = col_double()))

# Daily precipitation
p_daily <-read_csv("data/daily_ppt_2003_2016.csv")

# Table describing which watersheds are designated as control/treated pairs
treat_control <-read_csv("data/treated_control.csv")

# Treatment summary by wateryear 
treatment_wy <-read_csv("data/treatment_wy.csv")

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
  dplyr::summarise(P300 = sum(P300),
                   P301 = sum(P301),
                   P303 = sum(P303),
                   P304 = sum(P304),
                   D102 = sum(D102),
                   B200 = sum(B200),
                   B201 = sum(B201),
                   B203 = sum(B203),
                   B204 = sum(B204),
                   T003 = sum(T003),
                   P303 = sum(P303)) %>% 
  ungroup()

# Seasonal
q_seasonal <- q_daily %>% 
  dplyr::group_by(WY, Season) %>%
  dplyr::summarise(P300 = sum(P300),
                   P301 = sum(P301),
                   P303 = sum(P303),
                   P304 = sum(P304),
                   D102 = sum(D102),
                   B200 = sum(B200),
                   B201 = sum(B201),
                   B203 = sum(B203),
                   B204 = sum(B204),
                   T003 = sum(T003),
                   P303 = sum(P303)) %>% 
  ungroup()

# Wateryear
q_wy <- q_daily %>% 
  dplyr::group_by(WY) %>%
  dplyr::summarise(P300 = sum(P300),
                   P301 = sum(P301),
                   P303 = sum(P303),
                   P304 = sum(P304),
                   D102 = sum(D102),
                   B200 = sum(B200),
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
  dplyr::summarise(LP = sum(Lower_Prov),
                   UP = sum(Upper_Prov),
                   LB = sum(Lower_Bull),
                   UB = sum(Upper_Bull)) %>% 
  ungroup()

# Wateryear
p_wy <- p_daily %>% 
  dplyr::group_by(WY) %>%
  dplyr::summarise(LP = sum(Lower_Prov),
                   UP = sum(Upper_Prov),
                   LB = sum(Lower_Bull),
                   UB = sum(Upper_Bull)) %>% 
  ungroup()


# ---------------------------------------------------------------------
# Combine paired watershed data
# Generates a tibble with for comparing treated and control streamflow 

# Paired watershed - Mean Annual Minimum-7

treat <- map(treat_control$treatment, ~ select(q_mam7, ., WY)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_mam7, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_mam7 <- gather(treatment_wy, key = "shed_treated", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_mam7$q_treated[pair_mam7$q_treated == 0] <- NA
pair_mam7$q_control[pair_mam7$q_control == 0] <- NA

# ----
# Paired watershed - Q95
treat <- map(treat_control$treatment, ~ select(q_95, ., WY)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_95, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_q95 <- gather(treatment_wy, key = "shed_treated", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_q95$q_treated[pair_q95$q_treated == 0] <- NA
pair_q95$q_control[pair_q95$q_control == 0] <- NA

# ----
# Paired watershed - Monthly

treat <- map(treat_control$treatment, ~ select(q_monthly, ., WY, Month)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY, -Month)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_monthly, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_monthly <- gather(treatment_wy, key = "shed_treated", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_monthly$q_treated[pair_monthly$q_treated == 0] <- NA
pair_monthly$q_control[pair_monthly$q_control == 0] <- NA

# ----
# Paired watershed - Seasonal

treat <- map(treat_control$treatment, ~ select(q_seasonal, ., WY, Season)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY, -Season)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_seasonal, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_seasonal <- gather(treatment_wy, key = "shed_treated", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_seasonal$q_treated[pair_seasonal$q_treated == 0] <- NA
pair_seasonal$q_control[pair_seasonal$q_control == 0] <- NA

# ----
# Paired watershed - WY

treat <- map(treat_control$treatment, ~ select(q_wy, ., WY)) %>% 
  map(., ~ gather(., key = "shed_treated", value = "q_treated", -WY)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_wy, .)) %>% 
  map(., ~ gather(., key = "shed_control", value = "q_control")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_wy <- gather(treatment_wy, key = "shed_treated", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "shed_treated"))

pair_wy$q_treated[pair_wy$q_treated == 0] <- NA
pair_wy$q_control[pair_wy$q_control == 0] <- NA





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




