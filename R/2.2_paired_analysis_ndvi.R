# KREW paired streamflow plots


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Paired Function


paired_function <- function(data, x, y, var, var_lab, title, x_lab, y_lab){
  
  plot_paired <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(aes_string(size = var, color = var)) +
    geom_smooth(method='lm',se=FALSE, formula=y~x) +
    scale_y_log10() +
    scale_size_continuous(name = var_lab) +  
    scale_color_continuous(name = var_lab,
                           low= "#1f78b4", high="#b2df8a") +
    guides(colour = guide_colourbar(reverse=T)) +
    labs(title=title,
         y = y_lab,
         x = x_lab) +
    NULL
  return(plot_paired)
}

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Building blocks for paired plots


# NDVI with control streamflow on x-axis
q95_NDVI_xcont <- paired_function(data=pair_q95,
                                  x="q_control",
                                  y="q_treated",
                                  var="ndvi_diff",
                                  var_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
                                  title="Q95",
                                  x_lab="Control Watershed: Q95 (mm)",
                                  y_lab="Treated Watershed:\nQ95 (mm)")

s1_NDVI_xcont <- paired_function(data=pair_seasonal_1,
                                 x="q_control",
                                 y="q_treated",
                                 var="ndvi_diff",
                                 var_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
                                 title="October-December Streamflow",
                                 x_lab="Control Watershed: Oct-Dec Streamflow (mm)",
                                 y_lab="Treated Watershed:\nOct-Dec Streamflow (mm)")

s2_NDVI_xcont <- paired_function(data=pair_seasonal_2,
                                 x="q_control",
                                 y="q_treated",
                                 var="ndvi_diff",
                                 var_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
                                 title="January-March Streamflow",
                                 x_lab="Control Watershed: Jan-Mar Streamflow (mm)",
                                 y_lab="Treated Watershed:\nJan-Mar Streamflow (mm)")

s3_NDVI_xcont <- paired_function(data=pair_seasonal_3,
                                 x="q_control",
                                 y="q_treated",
                                 var="ndvi_diff",
                                 var_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
                                 title="April-June Streamflow",
                                 x_lab="Control Watershed: Apr-Jun Streamflow (mm)",
                                 y_lab="Treated Watershed:\nApr-Jun Streamflow (mm)")

s4_NDVI_xcont <- paired_function(data=pair_seasonal_4,
                                 x="q_control",
                                 y="q_treated",
                                 var="ndvi_diff",
                                 var_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
                                 title="July-September Streamflow",
                                 x_lab="Control Watershed: Jul-Sep Streamflow (mm)",
                                 y_lab="Treated Watershed:\nJul-Sep Streamflow (mm)")

wy_NDVI_xcont <- paired_function(data=pair_wy,
                                 x="q_control",
                                 y="q_treated",
                                 var="ndvi_diff",
                                 var_lab=expression('NDVI'[diff]),
                                 title=NULL,
                                 # x_lab="Control Watershed: Annual Streamflow (mm)",
                                 # y_lab="Treated Watershed:\nAnnual Streamflow (mm)")
                                 x_lab=expression('Annual Streamflow'[control]~'(mm)'),
                                 y_lab=expression('Annual Streamflow'[treated]~'(mm)'))

NDVI_xcont <- list(q95_NDVI_xcont, s1_NDVI_xcont, s2_NDVI_xcont,
                   s3_NDVI_xcont,s4_NDVI_xcont, wy_NDVI_xcont)

# ------------------
# nNDVI with control streamflow on x-axis

# q95_nNDVI_xcont <- paired_function(data=pair_q95,
#                                    x="q_control",
#                                    y="q_treated",
#                                    var="ndvi_diff_n",
#                                    var_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                    title="Q95",
#                                    x_lab="Control Watershed: Q95 (mm)",
#                                    y_lab="Treated Watershed:\nQ95 (mm)")
# 
# s1_nNDVI_xcont <- paired_function(data=pair_seasonal_1,
#                                   x="q_control",
#                                   y="q_treated",
#                                   var="ndvi_diff_n",
#                                   var_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                   title="October-December Streamflow",
#                                   x_lab="Control Watershed: Oct-Dec Streamflow (mm)",
#                                   y_lab="Treated Watershed:\nOct-Dec Streamflow (mm)")
# 
# s2_nNDVI_xcont <- paired_function(data=pair_seasonal_2,
#                                   x="q_control",
#                                   y="q_treated",
#                                   var="ndvi_diff_n",
#                                   var_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                   title="January-March Streamflow",
#                                   x_lab="Control Watershed: Jan-Mar Streamflow (mm)",
#                                   y_lab="Treated Watershed:\nJan-Mar Streamflow (mm)")
# 
# s3_nNDVI_xcont <- paired_function(data=pair_seasonal_3,
#                                   x="q_control",
#                                   y="q_treated",
#                                   var="ndvi_diff_n",
#                                   var_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                   title="April-June Streamflow",
#                                   x_lab="Control Watershed: Apr-Jun Streamflow (mm)",
#                                   y_lab="Treated Watershed:\nApr-Jun Streamflow (mm)")
# 
# s4_nNDVI_xcont <- paired_function(data=pair_seasonal_4,
#                                   x="q_control",
#                                   y="q_treated",
#                                   var="ndvi_diff_n",
#                                   var_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                   title="July-September Streamflow",
#                                   x_lab="Control Watershed: Jul-Sep Streamflow (mm)",
#                                   y_lab="Treated Watershed:\nJul-Sep Streamflow (mm)")
# 
# wy_nNDVI_xcont <- paired_function(data=pair_wy,
#                                   x="q_control",
#                                   y="q_treated",
#                                   var="ndvi_diff_n",
#                                   var_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                   title="Annual Streamflow",
#                                   x_lab="Control Watershed: Annual Streamflow (mm)",
#                                   y_lab="Treated Watershed:\nAnnual Streamflow (mm)")
# 
# nNDVI_xcont <- list(q95_nNDVI_xcont, s1_nNDVI_xcont, s2_nNDVI_xcont,
#                     s3_nNDVI_xcont,s4_nNDVI_xcont, wy_nNDVI_xcont)

# ------------------
# ------------------
# NDVI with NDVI_diff on x-axis
# q95_NDVI_xvar <- paired_function(data=pair_q95,
#                                  x="ndvi_diff",
#                                  y="q_treated",
#                                  var="q_control",
#                                  var_lab="Q95 (mm)",
#                                  title="Q95",
#                                  x_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
#                                  y_lab="Treated Watershed:\nQ95 (mm)")
# 
# s1_NDVI_xvar <- paired_function(data=pair_seasonal_1,
#                                 x="ndvi_diff",
#                                 y="q_treated",
#                                 var="q_control",
#                                 var_lab="Oct-Dec Streamflow (mm)",
#                                 title="October-December Streamflow",
#                                 x_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
#                                 y_lab="Treated Watershed:\nOct-Dec Streamflow (mm)")
# 
# s2_NDVI_xvar <- paired_function(data=pair_seasonal_2,
#                                 x="ndvi_diff",
#                                 y="q_treated",
#                                 var="q_control",
#                                 var_lab="Jan-Mar Streamflow (mm)",
#                                 title="January-March Streamflow",
#                                 x_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
#                                 y_lab="Treated Watershed:\nJan-Mar Streamflow (mm)")
# 
# s3_NDVI_xvar <- paired_function(data=pair_seasonal_3,
#                                 x="ndvi_diff",
#                                 y="q_treated",
#                                 var="q_control",
#                                 var_lab="Apr-Jun Streamflow (mm)",
#                                 title="April-June Streamflow",
#                                 x_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
#                                 y_lab="Treated Watershed:\nApr-Jun Streamflow (mm)")
# 
# s4_NDVI_xvar <- paired_function(data=pair_seasonal_4,
#                                 x="ndvi_diff",
#                                 y="q_treated",
#                                 var="q_control",
#                                 var_lab="Jul-Sep Streamflow (mm)",
#                                 title="July-September Streamflow",
#                                 x_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
#                                 y_lab="Treated Watershed:\nJul-Sep Streamflow (mm)")
# 
# wy_NDVI_xvar <- paired_function(data=pair_wy,
#                                 x="ndvi_diff",
#                                 y="q_treated",
#                                 var="q_control",
#                                 var_lab="Annual Streamflow (mm)",
#                                 title="Annual Streamflow",
#                                 x_lab=expression('NDVI'[t]~'-'~'NDVI'[c]),
#                                 y_lab="Treated Watershed:\nAnnual Streamflow (mm)")
# 
# NDVI_xvar <- list(q95_NDVI_xvar, s1_NDVI_xvar, s2_NDVI_xvar,
#                   s3_NDVI_xvar,s4_NDVI_xvar, wy_NDVI_xvar)

# ------------------
# nNDVI with NDVI_diff_n on x-axis

# q95_nNDVI_xvar <- paired_function(data=pair_q95,
#                                   x="ndvi_diff_n",
#                                   y="q_treated",
#                                   var="q_control",
#                                   var_lab="Q95 (mm)",
#                                   title="Q95",
#                                   x_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                   y_lab="Treated Watershed:\nQ95 (mm)")
# 
# s1_nNDVI_xvar <- paired_function(data=pair_seasonal_1,
#                                  x="ndvi_diff_n",
#                                  y="q_treated",
#                                  var="q_control",
#                                  var_lab="Oct-Dec Streamflow (mm)",
#                                  title="October-December Streamflow",
#                                  x_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                  y_lab="Treated Watershed:\nOct-Dec Streamflow (mm)")
# 
# s2_nNDVI_xvar <- paired_function(data=pair_seasonal_2,
#                                  x="ndvi_diff_n",
#                                  y="q_treated",
#                                  var="q_control",
#                                  var_lab="Jan-Mar Streamflow (mm)",
#                                  title="January-March Streamflow",
#                                  x_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                  y_lab="Treated Watershed:\nJan-Mar Streamflow (mm)")
# 
# s3_nNDVI_xvar <- paired_function(data=pair_seasonal_3,
#                                  x="ndvi_diff_n",
#                                  y="q_treated",
#                                  var="q_control",
#                                  var_lab="Apr-Jun Streamflow (mm)",
#                                  title="April-June Streamflow",
#                                  x_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                  y_lab="Treated Watershed:\nApr-Jun Streamflow (mm)")
# 
# s4_nNDVI_xvar <- paired_function(data=pair_seasonal_4,
#                                  x="ndvi_diff_n",
#                                  y="q_treated",
#                                  var="q_control",
#                                  var_lab="Jul-Sep Streamflow (mm)",
#                                  title="July-September Streamflow",
#                                  x_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                  y_lab="Treated Watershed:\nJul-Sep Streamflow (mm)")
# 
# wy_nNDVI_xvar <- paired_function(data=pair_wy,
#                                  x="ndvi_diff_n",
#                                  y="q_treated",
#                                  var="q_control",
#                                  var_lab="Annual Streamflow (mm)",
#                                  title="Annual Streamflow",
#                                  x_lab=expression('nNDVI'[t]~'-'~'nNDVI'[c]),
#                                  y_lab="Treated Watershed:\nAnnual Streamflow (mm)")
# 
# nNDVI_xvar <- list(q95_nNDVI_xvar, s1_nNDVI_xvar, s2_nNDVI_xvar,
#                    s3_nNDVI_xvar,s4_nNDVI_xvar, wy_nNDVI_xvar)

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Function for plotting paired watershed plots (individually and together)

# Function
paired_all <- function(x, xcont, path_ind, path_all){
  
  # Change x scale if q_control is on x axis
  if (xcont==TRUE){ 
    x_tmp <- x %>% 
      purrr::map(., function(x)
        x +
          scale_x_log10()
      )
  } else {
    x_tmp <- x
  }
  
  # Code for individual plots
  x_ind <- x_tmp %>% 
    purrr::map(., function(x)
      x +
        facet_wrap(.~shed_treated, ncol=3) +
        coord_fixed(ratio = 1) +
        theme_bw(base_size = 12) +
        theme(legend.position="right",
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 8))
    )
  
  # Code for combined plot
  x_all <- x_tmp %>% 
    purrr::map(., function(x)
      x +
        facet_grid(.~shed_treated) +
        theme_bw(base_size = 9) +
        theme(legend.position="right",
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 8))
    )
  
  plot_legend <- get_legend(x_all[[1]])
  
  
  # --------------------
  # Create individual plots
  
  output_types <- c("q95", "s1", "s2", "s3", "s4", "wy")
  
  purrr::map2(x_ind, output_types, function(x, y)
      ggsave(paste(path_ind,"_",y,".pdf",sep=""), plot=x, width = 6, height = 5)
  )
  
  
  # --------------------
  # Create combined plot
 
  plot_x_all <- cowplot::plot_grid(x_all[[1]] + theme(legend.position="none"),
                                   x_all[[2]] + theme(legend.position="none"),
                                   x_all[[3]] + theme(legend.position="none"),
                                   x_all[[4]] + theme(legend.position="none"),
                                   x_all[[5]] + theme(legend.position="none"),
                                   x_all[[6]] + theme(legend.position="none"),
                                   nrow=6)
  
  plot_x_all <- cowplot::plot_grid(plot_x_all + theme(legend.position="none"),
                                   plot_legend,
                                   ncol=2,
                                   rel_widths = c(1,0.15))
                                   
  # Save the plot
  cowplot::save_plot(path_all,
                     plot_x_all,
                     # ncol=1,
                     # nrow=7,
                     base_height=10,
                     base_width=8)
}


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Make the plots


paired_all(x=NDVI_xcont,
           xcont=TRUE,
           path_ind="output/2.2_paired_analysis/plot_NDVI_xcont_ind",   # Don't include extension
           path_all="output/2.2_paired_analysis/plot_NDVI_xcont_all.pdf")

# paired_all(x=nNDVI_xcont,
#            xcont=TRUE,
#            path_ind="output/2.2_paired_analysis/plot_nNDVI_xcont_ind",   # Don't include extension
#            path_all="output/2.2_paired_analysis/plot_nNDVI_xcont_all.pdf")

# Note: Legend for the combined plot is wrong for the following two plots (need a legend for each row).

# paired_all(x=NDVI_xvar,
#            xcont=FALSE,
#            path_ind="output/2.2_paired_analysis/plot_NDVI_xvar_ind",   # Don't include extension
#            path_all="output/2.2_paired_analysis/plot_NDVI_xvar_all.pdf")
# 
# paired_all(x=nNDVI_xvar,
#            xcont=FALSE,
#            path_ind="output/2.2_paired_analysis/plot_nNDVI_xvar_ind",   # Don't include extension
#            path_all="output/2.2_paired_analysis/plot_nNDVI_xvar_all.pdf")





