# Utilities for KREW streamflow analysis
# Includes files/directories and functions


# ---------------------------------------------------------------------
# Libraries

library(tidyverse)
library(broom)
library(lubridate)
library(beepr)
library(lfstat)
library(MCMCglmm)
library(RHESSysIOinR)
library(RHESSysPreprocessing)
library(raster)
library(sp)
library(sf)
library(rgdal)
library(hydroGOF)

# ---------------------------------------------------------------------
# Files and Directories - Empirical Analysis

# Original data

# ----
# 2.1_paired_processing

path <- "output/2.1_paired_processing"
PAIR_MAM7_CSV <- file.path(path, "pair_mam7.csv")
PAIR_MAM7_RDS <- file.path(path, "pair_mam7.rds")

PAIR_Q95_CSV <- file.path(path, "pair_q95.csv")
PAIR_Q95_RDS <- file.path(path, "pair_q95.rds")

PAIR_MONTHLY_CSV <- file.path(path, "pair_monthly.csv")
PAIR_MONTHLY_RDS <- file.path(path, "pair_monthly.rds")

PAIR_SEASONAL_CSV <- file.path(path, "pair_seasonal.csv")
PAIR_SEASONAL_RDS <- file.path(path, "pair_seasonal.rds")

PAIR_WY_CSV <- file.path(path, "pair_wy.csv")
PAIR_WY_RDS <- file.path(path, "pair_wy.rds")

QP_WY_CSV <- file.path(path, "qp_wy.csv")
QP_WY_RDS <- file.path(path, "qp_wy.rds")

QPT_WY_CSV <- file.path(path, "qpt_wy.csv")
QPT_WY_RDS <- file.path(path, "qpt_wy.rds")

# ----
# 2.3_paired_mixed_model

path <- "output/2.3_mixed_model"



# ----
# 2.5_time_trend_analysis





# ---------------------------------------------------------------------
# Files and Directories - RHESSys


RHESSYS_OUT_DIR_21_P301_1 <- "ws_p301/out/21_p301_soil1"
RHESSYS_ALLSIM_DIR_21_P301_1 <- file.path(RHESSYS_OUT_DIR_21_P301_1, "allsim")
RHESSYS_ALL_OPTION_21_P301_1 <- file.path(RHESSYS_OUT_DIR_21_P301_1, "p301_simulation_all_options.csv")

RHESSYS_OUT_DIR_21_P301_2 <- "ws_p301/out/21_p301_soil3"
RHESSYS_ALLSIM_DIR_21_P301_2 <- file.path(RHESSYS_OUT_DIR_21_P301_2, "allsim")
RHESSYS_ALL_OPTION_21_P301_2 <- file.path(RHESSYS_OUT_DIR_21_P301_2, "p301_simulation_all_options.csv")

RHESSYS_OUT_DIR_21_P301_3 <- "ws_p301/out/21_p301_soil5"
RHESSYS_ALLSIM_DIR_21_P301_3 <- file.path(RHESSYS_OUT_DIR_21_P301_3, "allsim")
RHESSYS_ALL_OPTION_21_P301_3 <- file.path(RHESSYS_OUT_DIR_21_P301_3, "p301_simulation_all_options.csv")


# ---------------------------------------------------------------------
# Functions

# Function to change year to wateryear
y_to_wy = function(year, month, start.month=10){
  wateryear <- ifelse(month >= start.month, year + 1, year)
  return(wateryear)
}


lfobj_function <- function(x){
  
  # Create lfobj for functions that require lfstat library
  q_p301_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=P301) %>% 
    createlfobj(baseflow=FALSE)
  
  q_p303_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=P303) %>% 
    createlfobj(baseflow=FALSE)
  
  q_p304_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=P304) %>% 
    createlfobj(baseflow=FALSE)
  
  q_d102_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=D102) %>% 
    createlfobj(baseflow=FALSE)
  
  q_b201_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=B201) %>% 
    createlfobj(baseflow=FALSE)
  
  q_b203_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=B203) %>% 
    createlfobj(baseflow=FALSE)
  
  q_b204_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=B204) %>% 
    createlfobj(baseflow=FALSE)

  q_t003_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=T003) %>% 
    createlfobj(baseflow=FALSE)
  
  q_lfobj <- list(p301=q_p301_lfobj,p303=q_p303_lfobj,
                  p304=q_p304_lfobj,d102=q_d102_lfobj,
                  b201=q_b201_lfobj,b203=q_b203_lfobj,
                  b204=q_b204_lfobj,t003=q_t003_lfobj)
  
  return(q_lfobj)
}


lfstat_MAM<- function(x){
  x %>% 
    reduce(full_join, by="hyear") %>% 
    dplyr::rename(WY=hyear,
                  P301=MAn.x,
                  P303=MAn.y,
                  P304=MAn.x.x,
                  D102=MAn.y.y,
                  B201=MAn.x.x.x,
                  B203=MAn.y.y.y,
                  B204=MAn.x.x.x.x,
                  T003=MAn.y.y.y.y)
}

lfstat_Qxx<- function(x){
  x %>% 
    reduce(full_join, by="hyear") %>% 
    dplyr::rename(WY=hyear,
                  P301=flow.x,
                  P303=flow.y,
                  P304=flow.x.x,
                  D102=flow.y.y,
                  B201=flow.x.x.x,
                  B203=flow.y.y.y,
                  B204=flow.x.x.x.x,
                  T003=flow.y.y.y.y)
}

