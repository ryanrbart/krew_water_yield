# Summarize NDVI and ET Data

# This script summarizes raster ndvi/et data to watershed scale and into tibble
# so that it can be analyzed statistically

source("R/0_utilities.R")


# ---------------------------------------------------------------------
# Import processed NDVI/ET data

# Table of NDVI files
ndvi_table_final <- read_rds("output/3.3/ndvi_table_final.rds")

# KREW watersheds: Raster 
prov_rast <- read_rds("output/3.1/prov_rast.rds")
bull_rast <- read_rds("output/3.1/bull_rast.rds")

# Rasterbricks of ndvi and et
ndvi_prov_final <- read_rds("output/3.3/ndvi_prov_final.rds")
ndvi_bull_final <- read_rds("output/3.3/ndvi_bull_final.rds")
et_prov_final <- read_rds("output/3.4/et_prov_final.rds")
et_bull_final <- read_rds("output/3.4/et_bull_final.rds")

# Table describing which watersheds are designated as control/treated pairs
treat_control <-read_csv("data/treated_control.csv")

# Treatment summary by wateryear 
treatment_dummy <-read_csv("data/treatment_dummy.csv")
treatment_wy <-read_csv("data/treatment_wy.csv")
thinning_dummy <-read_csv("data/thinning_dummy.csv")
thinning_wy <-read_csv("data/thinning_wy.csv")
prescribed_fire_dummy <-read_csv("data/prescribed_fire_dummy.csv")
prescribed_fire_wy <-read_csv("data/prescribed_fire_wy.csv")


# ---------------------------------------------------------------------
# Convert to tibble

raster_to_tibble <- function(raster, watershed){
  out <- raster %>% 
    raster::stack(watershed, .) %>%     # Add layer that differentiates watersheds
    raster::rasterToPoints() %>% 
    as_tibble()
  return(out)
}

ndvi_prov_tib <- raster_to_tibble(raster=ndvi_prov_final, watershed=prov_rast)
ndvi_bull_tib <- raster_to_tibble(raster=ndvi_bull_final, watershed=bull_rast)
et_prov_tib <- raster_to_tibble(raster=et_prov_final, watershed=prov_rast)
et_bull_tib <- raster_to_tibble(raster=et_bull_final, watershed=bull_rast)


# ---------------------------------------------------------------------
# Add watershed (row) names in tibble and gather

# Watershed names
ndvi_prov_tib <- dplyr::mutate(ndvi_prov_tib, shed = case_when(layer==1~"P301",
                                                               layer==2~"P303",
                                                               layer==3~"P304",
                                                               layer==4~"D102"))

ndvi_bull_tib <- dplyr::mutate(ndvi_bull_tib, shed = case_when(layer==1~"B201",
                                                               layer==2~"B203",
                                                               layer==3~"T003",
                                                               layer==4~"B204"))

et_prov_tib <- dplyr::mutate(et_prov_tib, shed = case_when(layer==1~"P301",
                                                           layer==2~"P303",
                                                           layer==3~"P304",
                                                           layer==4~"D102"))

et_bull_tib <- dplyr::mutate(et_bull_tib, shed = case_when(layer==1~"B201",
                                                           layer==2~"B203",
                                                           layer==3~"T003",
                                                           layer==4~"B204"))

# Gather tibbles
ndvi_prov_tib2 <- ndvi_prov_tib %>% 
  dplyr::select(-layer) %>% 
  tidyr::gather(key="scene", value="ndvi", -x, -y, -shed)
ndvi_bull_tib2 <- ndvi_bull_tib %>% 
  dplyr::select(-layer) %>% 
  tidyr::gather(key="scene", value="ndvi", -x, -y, -shed)
et_prov_tib2 <- et_prov_tib %>% 
  dplyr::select(-layer) %>% 
  tidyr::gather(key="scene", value="et", -x, -y, -shed)
et_bull_tib2 <- et_bull_tib %>% 
  dplyr::select(-layer) %>% 
  tidyr::gather(key="scene", value="et", -x, -y, -shed)

# ---------------------------------------------------------------------
# Combine the NDVI with the ET data, Prov with Bull

# Stick all the pieces together
prov_tib <- dplyr::full_join(ndvi_prov_tib2, 
                             et_prov_tib2,
                             by=c("x","y","shed","scene"))
bull_tib <- dplyr::full_join(ndvi_bull_tib2, 
                             et_bull_tib2,
                             by=c("x","y","shed","scene"))
krew_tib <- dplyr::bind_rows(prov_tib, bull_tib)


krew_tib <- inner_join(krew_tib, ndvi_table_final,
                       by=c("scene"="short_name"))


# ---------------------------------------------------------------------
# Average by year (if necessary) and by watershed

krew_annual <- krew_tib %>%
  group_by(shed, year) %>% 
  dplyr::summarise(ndvi_annual=mean(ndvi),
                   et_annual=mean(et))

# Add prov/bull location and control designation (for plotting time-series)
happy_tib <- tibble(shed=c("B201","B203","B204","T003",
                           "P301","P303","P304","D102"),
                    location = c("Bull","Bull","Bull","Bull",
                                 "Prov","Prov","Prov","Prov"),
                    control = c(FALSE,FALSE,FALSE,TRUE,
                                FALSE,FALSE,TRUE,FALSE))
krew_annual <- krew_annual %>% 
  dplyr::left_join(happy_tib, by="shed")


# ---------------------------------------------------------------------
# Set up data in format for paired watershed analysis


# ---
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


# ---
# Identify control and treated ndvi and et
krew_treat <- krew_annual %>% 
  dplyr::select(-c(location, control)) %>%   # Removes the columns added for time-series plot
  dplyr::filter(shed %in% treat_control$treatment) %>% 
  dplyr::rename(shed_treated=shed,
                ndvi_treated=ndvi_annual,
                et_treated=et_annual) %>% 
  dplyr::left_join(dplyr::select(treat_control, -year_thin, -year_burn), by=c("shed_treated"="treatment"))

krew_control <- krew_annual %>% 
  dplyr::select(-c(location, control)) %>%   # Removes the columns added for time-series plot
  dplyr::filter(shed %in% treat_control$control) %>% 
  dplyr::rename(shed_control=shed,
                ndvi_control=ndvi_annual,
                et_control=et_annual)

krew_paired <- krew_treat %>% 
  dplyr::left_join(krew_control, by=c("year"="year", "control"="shed_control")) %>% 
  dplyr::left_join(treatment_sheds, by=c("year"="WY", "shed_treated"="shed_treated")) %>% 
  dplyr::rename(shed_control=control) %>% 
  dplyr::mutate(treatment_label = if_else(treatment_dummy==1,year,NA_real_))



# ---------------------------------------------------------------------
# Save krew annual data grouped by watershed

write_rds(krew_annual, "output/3.5/krew_annual.rds")
write_rds(krew_paired, "output/3.5/krew_paired.rds")

