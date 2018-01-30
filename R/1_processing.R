# KREW data import and processsing


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data


q_daily <- read_csv("data/daily_discharge_filled_2003_2016.csv", 
                    col_types = cols(
                      P300 = col_double(),
                      B200 = col_double()))
p_daily <-read_csv("data/daily_ppt_2003_2016.csv")

treat_control <-read_csv("data/treated_control.csv")
treatment_wy <-read_csv("data/treatment_wy.csv")

# ---------------------------------------------------------------------
# Summarize data to monthly and annual timesteps

# ----
# Streamflow


# Daily


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
# Combine data

# ----
# Paired watershed - Monthly

treat <- map(treat_control$treatment, ~ select(q_monthly, ., WY, Month)) %>% 
  map(., ~ gather(., key = "treated_shed", value = "treated_q", -WY, -Month)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_monthly, .)) %>% 
  map(., ~ gather(., key = "control_shed", value = "control_q")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_monthly <- gather(treatment_wy, key = "treated_shed", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "treated_shed"))

pair_monthly$treated_q[pair_monthly$treated_q == 0] <- NA
pair_monthly$control_q[pair_monthly$control_q == 0] <- NA

# ----
# Paired watershed - Seasonal

treat <- map(treat_control$treatment, ~ select(q_seasonal, ., WY, Season)) %>% 
  map(., ~ gather(., key = "treated_shed", value = "treated_q", -WY, -Season)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_seasonal, .)) %>% 
  map(., ~ gather(., key = "control_shed", value = "control_q")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_seasonal <- gather(treatment_wy, key = "treated_shed", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "treated_shed"))

pair_seasonal$treated_q[pair_seasonal$treated_q == 0] <- NA
pair_seasonal$control_q[pair_seasonal$control_q == 0] <- NA

# ----
# Paired watershed - WY

treat <- map(treat_control$treatment, ~ select(q_wy, ., WY)) %>% 
  map(., ~ gather(., key = "treated_shed", value = "treated_q", -WY)) %>%
  bind_rows() 

control <- map(treat_control$control, ~ select(q_wy, .)) %>% 
  map(., ~ gather(., key = "control_shed", value = "control_q")) %>% 
  bind_rows() 

pair_tmp <- bind_cols(treat, control)

pair_wy <- gather(treatment_wy, key = "treated_shed", value = "treatment",-WY) %>% 
  left_join(pair_tmp, ., by=c("WY", "treated_shed"))

pair_wy$treated_q[pair_wy$treated_q == 0] <- NA
pair_wy$control_q[pair_wy$control_q == 0] <- NA

# ----



# ---------------------------------------------------------------------
# Save processed data

# Paired watershed
write.csv(pair_monthly, PAIR_MONTHLY_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_monthly, PAIR_MONTHLY_RDS)

write.csv(pair_seasonal, PAIR_SEASONAL_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_seasonal, PAIR_SEASONAL_RDS)

write.csv(pair_wy, PAIR_WY_CSV, row.names = FALSE, quote=FALSE)
write_rds(pair_wy, PAIR_WY_RDS)




