# Contains scripts for evaluating spinup of soil C and N
# P301 and B204


# To-do list 
# Figures showing stable soilc & soiln
# Figure showing understory growing properly.
# Code to null out vegetation stores in worldfile for 3.3.1.
# Adjust LAI spinup targets in correspond to mean RHESSys LAI.


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import and process


happy_p301 <- readin_rhessys_output("ws_p301/out/3.2_p301_spinup/p301_spinup",c=0,g=1)

happy_b204 <- readin_rhessys_output("ws_b204/out/3.2_b204_spinup/b204_spinup",c=0,g=1)


