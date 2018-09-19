# Contains scripts for evaluating vegetation patch calibration
# P301 and B204


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import and process

happy_p301 <- readin_rhessys_output("ws_p301/out/3.1_p301_cal_veg_patch/p301_cal",c=1,p=1,g=1)
happy_b204 <- readin_rhessys_output("ws_b204/out/3.1_b204_cal_veg_patch/b204_cal",c=1, p=1, g=1)

# Combine canopy data
cd <- as_tibble(dplyr::bind_rows("p301" = happy_p301$cd, "b204" = happy_b204$cd, .id = "watershed"))
cd$stratumID <- factor(cd$stratumID, levels=c(1,2))

# ---------------------------------------------------------------------
# Plot figures

# Compare height
x <- ggplot(data = cd) +
  geom_line(aes(x=wy, y=height, linetype=stratumID, color=watershed)) +
  #ylim(0,4) +
  NULL
plot(x)


# Compare LAI
x <- ggplot(data = cd) +
  geom_line(aes(x=wy, y=lai, linetype=stratumID, color=watershed)) +
  NULL
plot(x)

