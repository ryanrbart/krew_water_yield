# Contains scripts for evaluating vegetation patch calibration
# P301 and B204


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import and process

happy_p301 <- readin_rhessys_output("ws_p301/out/3.0.1_drought_calibration/p301_cal",c=1,p=1,g=1)

happy_p301$cd$stratumID <- factor(happy_p301$cd$stratumID, levels=c(1,2))

# ---------------------------------------------------------------------
# Plot figures

# Compare height
x <- ggplot(data = happy_p301$cd) +
  geom_line(aes(x=wy, y=height, linetype=stratumID)) +
  #ylim(0,4) +
  NULL
plot(x)


# Compare LAI
x <- ggplot(data = happy_p301$cd) +
  geom_line(aes(x=wy, y=lai, linetype=stratumID)) +
  NULL
plot(x)


# Compare Fine Root C
x <- ggplot(data = happy_p301$cdg) +
  geom_line(aes(x=wy, y=frootc, group=stratumID)) +
  NULL
plot(x)

# Compare Coarse Root C
x <- ggplot(data = happy_p301$cdg) +
  geom_line(aes(x=wy, y=dead_crootc, group=stratumID)) +
  NULL
plot(x)
x <- ggplot(data = happy_p301$cdg) +
  geom_line(aes(x=wy, y=live_crootc, group=stratumID)) +
  NULL
plot(x)


# ---------------------------------------------------------------------
# Plot test of soil C/N variables (to get estimate of spinup time in 3.2)

# Compare Soilc
x <- ggplot(data = happy_p301$bdg) +
  geom_line(aes(x=wy, y=soilc)) +
  NULL
plot(x)

# Compare Soiln
x <- ggplot(data = happy_p301$bdg) +
  geom_line(aes(x=wy, y=soiln)) +
  NULL
plot(x)


# ---------------------------------------------------------------------
# Check hydrologic variables






