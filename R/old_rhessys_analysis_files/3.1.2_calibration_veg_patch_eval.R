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



# ---------------------------------------------------------------------
# Plot test of soil C/N variables (to get estimate of spinup time in 3.2)

# Combine canopy data
bd <- as_tibble(dplyr::bind_rows("p301" = happy_p301$bdg, "b204" = happy_b204$bdg, .id = "watershed"))

# Compare Soilc
x <- ggplot(data = bd) +
  geom_line(aes(x=wy, y=soilc, linetype=watershed, color=watershed)) +
  NULL
plot(x)

# Compare Soiln
x <- ggplot(data = bd) +
  geom_line(aes(x=wy, y=soiln, linetype=watershed, color=watershed)) +
  NULL
plot(x)
