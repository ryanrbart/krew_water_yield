# Patch drought calibration
#

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Initialize variables

# P301 RHESSys outputs
RHESSYS_OUT_DIR_3.0.3 <- "ws_p301/out/3.0.3_drought_calibration"
RHESSYS_ALLSIM_DIR_3.0.3 <- file.path(RHESSYS_OUT_DIR_3.0.3, "allsim")
RHESSYS_PAR_FILE_3.0.3 <- file.path(RHESSYS_OUT_DIR_3.0.3, "p301_cal_parameter_sets.csv")

OUTPUT_DIR <- "outputs"
OUTPUT_DIR_3.0 <- file.path(OUTPUT_DIR, "3.0_drought_calibration")


num_canopies = 1
allsim_path = RHESSYS_ALLSIM_DIR_3.0.3
initial_date = ymd("2003-10-01")
parameter_file = RHESSYS_PAR_FILE_3.0.3
watershed = "P301"
output_path = OUTPUT_DIR_3.0
cwdc_yes = FALSE


# ---------------------------------------------------------------------
# Generate observed data

# Temporary "fake" observed ET.
observed_et <- tibble(var_type = "et",
                        wy = seq(2004,2017),
                        obs = c(780,800,780,670,650,700,740,
                                777,720,670,617,610,640,700))
# Streamflow
observed_q <- tibble(var_type = "streamflow",
                      wy = seq(2004,2017),
                      obs = seq(200, by=100, length.out=14))

observed_data <- dplyr::bind_rows(observed_et, observed_q)
observed_data$var_type <- factor(observed_data$var_type, levels = c("trans",
                                                                    "unsat_stor",
                                                                    "rz_storage",
                                                                    "evap",
                                                                    "sat_def",
                                                                    "streamflow",
                                                                    "et"))

# ---------------------------------------------------------------------
# Readin calibration output and process data

happy <- readin_rhessys_output_cal(var_names = c("trans",
                                                 "unsat_stor",
                                                 "rz_storage",
                                                 "evap",
                                                 "sat_def",
                                                 "streamflow",
                                                 "et"),
                                   path = allsim_path,
                                   initial_date = initial_date,
                                   parameter_file = parameter_file,
                                   num_canopies = num_canopies)
happy$wy <- y_to_wy(lubridate::year(happy$dates),lubridate::month(happy$dates))

# Collapse by year (all variables)
happy_wy1 <- happy %>% 
  dplyr::filter(var_type %in% c("et","trans","evap","streamflow")) %>% 
  dplyr::group_by(wy, run, var_type,
                  gw1, gw2, k, k_v, 
                  m, m_v, pa, po) %>% 
  dplyr::summarise(avg_value = sum(value))

happy_wy2 <- happy %>% 
  dplyr::filter(var_type %in% c("unsat_stor","rz_storage","sat_def")) %>% 
  dplyr::group_by(wy, run, var_type,
                  gw1, gw2, k, k_v, 
                  m, m_v, pa, po) %>% 
  dplyr::summarise(avg_value = mean(value))

happy_wy <- bind_rows(happy_wy1,happy_wy2)

# ---------------------------------------------------------------------
# Process objective function

# Generate an NSE value for each variable with observed data (et and q)
happy_n <- happy_wy %>% 
  # Filter et and q
  dplyr::filter(var_type == "et" | var_type == "streamflow" ) %>% 
  # Attach observed data
  dplyr::full_join(., observed_data, by = c("wy","var_type")) %>% 
  # Nest data
  dplyr::group_by(run, var_type,
                  gw1, gw2, k, k_v, 
                  m, m_v, pa, po) %>% 
  tidyr::nest()

# Generate an NSE value for each variable
happy_nse <- happy_n %>% 
  # Add observed timeseries to nested data and generate new NSE column
  mutate(NSE_value = map(data, function(x)NSE(sim=x$avg_value,obs=x$obs))) %>% 
  # Can't unnest with more than one nested variables, so removing original data
  dplyr::select(-data) %>% 
  unnest()

# ---------------------------------------------------------------------
# ET

happy_nse_et <- happy_nse %>% 
  dplyr::filter(var_type=="et") %>% 
  dplyr::arrange(desc(NSE_value))

this_et <- happy_nse_et$run[1]
this_et2 <- happy_nse_et$run[2]

# ---------------------------------------------------------------------
# Streamflow

happy_nse_q <- happy_nse %>% 
  dplyr::filter(var_type=="streamflow") %>% 
  dplyr::arrange(desc(NSE_value))

this_q <- happy_nse_q$run[1]
this_q2 <- happy_nse_q$run[2]



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Plot variables

# ET
happy_plot <- happy_wy %>% 
  dplyr::filter(var_type == c("et"))
top_et <- happy_plot %>%
  dplyr::filter(run == this_et | run==this_et2)
top_q <- happy_plot %>%
  dplyr::filter(run == this_q | run==this_q2)
x <- ggplot() + 
  geom_line(data=happy_plot, aes(x=wy, y=avg_value, group=run)) +
  geom_hline(yintercept=c(580,780), col = "gray") +
  geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)

# Q
happy_plot <- happy_wy %>% 
  dplyr::filter(var_type == c("streamflow"))
top_et <- happy_plot %>%
  dplyr::filter(run == this_et | run==this_et2)
top_q <- happy_plot %>%
  dplyr::filter(run == this_q | run==this_q2)
x <- ggplot() + 
  geom_line(data=happy_plot, aes(x=wy, y=avg_value, group=run)) +
  geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)


# Trans
happy_plot <- happy_wy %>% 
  dplyr::filter(var_type == c("trans"))
top_et <- happy_plot %>%
  dplyr::filter(run == this_et | run==this_et2)
top_q <- happy_plot %>%
  dplyr::filter(run == this_q | run==this_q2)
x <- ggplot() + 
  geom_line(data=happy_plot, aes(x=wy, y=avg_value, group=run)) +
  geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)

# Evap
happy_plot <- happy_wy %>% 
  dplyr::filter(var_type == c("evap"))
top_et <- happy_plot %>%
  dplyr::filter(run == this_et | run==this_et2)
top_q <- happy_plot %>%
  dplyr::filter(run == this_q | run==this_q2)
x <- ggplot() + 
  geom_line(data=happy_plot, aes(x=wy, y=avg_value, group=run)) +
  geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)


# Unsat storage
happy_plot <- happy_wy %>% 
  dplyr::filter(var_type == c("unsat_stor"))
top_et <- happy_plot %>%
  dplyr::filter(run == this_et | run==this_et2)
top_q <- happy_plot %>%
  dplyr::filter(run == this_q | run==this_q2)
x <- ggplot() + 
  geom_line(data=happy_plot, aes(x=wy, y=avg_value, group=run)) +
  geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)

# Rooting zone storage
happy_plot <- happy_wy %>% 
  dplyr::filter(var_type == c("rz_storage"))
top_et <- happy_plot %>%
  dplyr::filter(run == this_et | run==this_et2)
top_q <- happy_plot %>%
  dplyr::filter(run == this_q | run==this_q2)
x <- ggplot() + 
  geom_line(data=happy_plot, aes(x=wy, y=avg_value, group=run)) +
  geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)

# Sat Def
happy_plot <- happy_wy %>% 
  dplyr::filter(var_type == c("sat_def"))
top_et <- happy_plot %>%
  dplyr::filter(run == this_et | run==this_et2)
top_q <- happy_plot %>%
  dplyr::filter(run == this_q | run==this_q2)
x <- ggplot() + 
  geom_line(data=happy_plot, aes(x=wy, y=avg_value, group=run)) +
  geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)





# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Make dotty plots


ggplot(happy_nse) + 
  geom_point(aes(x=po,y=NSE_value)) +
  ylim(-5,1) +
  NULL

ggplot(happy_nse) + 
  geom_point(aes(x=pa,y=NSE_value)) +
  ylim(-5,1) +
  NULL

ggplot(happy_nse) + 
  geom_point(aes(x=gw1,y=NSE_value)) +
  ylim(-5,1) +
  NULL

ggplot(happy_nse) + 
  geom_point(aes(x=gw2,y=NSE_value)) +
  ylim(-5,1) +
  NULL

ggplot(happy_nse) + 
  geom_point(aes(x=k,y=NSE_value)) +
  ylim(-5,1) +
  NULL

ggplot(happy_nse) + 
  geom_point(aes(x=m,y=NSE_value)) +
  ylim(-5,1) +
  NULL

ggplot(happy_nse) + 
  geom_point(aes(x=k_v,y=NSE_value)) +
  ylim(-5,1) +
  NULL

ggplot(happy_nse) + 
  geom_point(aes(x=m_v,y=NSE_value)) +
  ylim(-5,1) +
  NULL




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Look at daily stores to see year to year carryover




x <- happy %>% 
  #dplyr::filter(var_type %in% c("rz_storage")) %>% 
  dplyr::filter(var_type %in% c("trans")) %>% 
  dplyr::filter(run == this_et | run==this_et2) %>% 
  as_tibble() %>% 
  ggplot(.) + 
  geom_line(aes(x=dates, y=value, group=run)) +
  #xlim("2004-01-01", "2005-01-01") +
  #geom_line(data=top_et, aes(x=wy, y=avg_value, group=run), color="blue") +
  #geom_line(data=top_q, aes(x=wy, y=avg_value, group=run), color="red") +
  NULL
plot(x)

happy %>% 
  dplyr::filter(var_type %in% c("sat_def")) %>% 
  dplyr::filter(run == this_et | run==this_et2)



