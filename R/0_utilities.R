# Utilities for KREW streamflow analysis
# Includes files/directories and functions


# ---------------------------------------------------------------------
# Libraries

library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(broom)
library(lubridate)
library(beepr)
library(RHESSysIOinR)


# ---------------------------------------------------------------------
# Files and Directories - Paired Watersheds

# Original data

# Generated data
PAIR_MONTHLY_CSV <- "output/pair_monthly.csv"
PAIR_MONTHLY_RDS <- "output/pair_monthly.rds"

PAIR_SEASONAL_CSV <- "output/pair_seasonal.csv"
PAIR_SEASONAL_RDS <- "output/pair_seasonal.rds"

PAIR_WY_CSV <- "output/pair_wy.csv"
PAIR_WY_RDS <- "output/pair_wy.rds"

# ---------------------------------------------------------------------
# Files and Directories - RHESSys


RHESSYS_OUT_DIR_10_P300 <- "ws_p300/out/10_p300_patch_simulation"
RHESSYS_ALLSIM_DIR_10_P300 <- file.path(RHESSYS_OUT_DIR_10_P300, "allsim")
RHESSYS_ALL_OPTION_10_P300 <- file.path(RHESSYS_OUT_DIR_10_P300, "patch_sim_all_options.csv")


# ---------------------------------------------------------------------
# Functions

y_to_wy = function(year, month, start.month=10){
  wateryear <- ifelse(month >= start.month, year + 1, year)
  return(wateryear)
}




