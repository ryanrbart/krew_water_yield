# Utilities for KREW streamflow analysis
# Includes files/directories and functions


# ---------------------------------------------------------------------
# Libraries

library(tidyverse)
library(beepr)
library(lfstat)
library(MCMCglmm)
library(RHESSysIOinR)
library(RHESSysPreprocessing)
library(raster)
library(sp)
library(sf)
library(rgdal)



# ---------------------------------------------------------------------
# Files and Directories - Paired Watersheds

# Original data

# Generated data
PAIR_MAM7_CSV <- "output/pair_mam7.csv"
PAIR_MAM7_RDS <- "output/pair_mam7.rds"

PAIR_Q95_CSV <- "output/pair_q95.csv"
PAIR_Q95_RDS <- "output/pair_q95.rds"

PAIR_MONTHLY_CSV <- "output/pair_monthly.csv"
PAIR_MONTHLY_RDS <- "output/pair_monthly.rds"

PAIR_SEASONAL_CSV <- "output/pair_seasonal.csv"
PAIR_SEASONAL_RDS <- "output/pair_seasonal.rds"

PAIR_WY_CSV <- "output/pair_wy.csv"
PAIR_WY_RDS <- "output/pair_wy.rds"

# ---------------------------------------------------------------------
# Files and Directories - RHESSys


RHESSYS_OUT_DIR_10_P300 <- "ws_p300_patch/out/10_p300_patch_simulation"
RHESSYS_ALLSIM_DIR_10_P300 <- file.path(RHESSYS_OUT_DIR_10_P300, "allsim")
RHESSYS_ALL_OPTION_10_P300 <- file.path(RHESSYS_OUT_DIR_10_P300, "patch_sim_all_options.csv")

RHESSYS_OUT_DIR_21_P301 <- "ws_p301/out/21_p301_year1"
RHESSYS_ALLSIM_DIR_21_P301 <- file.path(RHESSYS_OUT_DIR_21_P301, "allsim")
RHESSYS_ALL_OPTION_21_P301 <- file.path(RHESSYS_OUT_DIR_21_P301, "p301_simulation_all_options.csv")



# ---------------------------------------------------------------------
# Functions

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
  
  q_p300_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=P300) %>% 
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
  
  q_b200_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=B200) %>% 
    createlfobj(baseflow=FALSE)
  
  q_t003_lfobj <- q_daily %>% 
    dplyr::select(year=Year, month=Month, day=Day, hyear=WY, flow=T003) %>% 
    createlfobj(baseflow=FALSE)
  
  q_lfobj <- list(p301=q_p301_lfobj,p303=q_p303_lfobj,
                  p304=q_p304_lfobj,p300=q_p300_lfobj,
                  d102=q_d102_lfobj,b201=q_b201_lfobj,
                  b203=q_b203_lfobj,b204=q_b204_lfobj,
                  b200=q_b200_lfobj,t003=q_t003_lfobj)
  
  return(q_lfobj)
}


lfstat_MAM<- function(x){
  x %>% 
    reduce(full_join, by="hyear") %>% 
    dplyr::rename(WY=hyear,
                  P301=MAn.x,
                  P303=MAn.y,
                  P304=MAn.x.x,
                  P300=MAn.y.y,
                  D102=MAn.x.x.x,
                  B201=MAn.y.y.y,
                  B203=MAn.x.x.x.x,
                  B204=MAn.y.y.y.y,
                  B200=MAn.x.x.x.x.x,
                  T003=MAn.y.y.y.y.y)
}

lfstat_Qxx<- function(x){
  x %>% 
    reduce(full_join, by="hyear") %>% 
    dplyr::rename(WY=hyear,
                  P301=flow.x,
                  P303=flow.y,
                  P304=flow.x.x,
                  P300=flow.y.y,
                  D102=flow.x.x.x,
                  B201=flow.y.y.y,
                  B203=flow.x.x.x.x,
                  B204=flow.y.y.y.y,
                  B200=flow.x.x.x.x.x,
                  T003=flow.y.y.y.y.y)
}

