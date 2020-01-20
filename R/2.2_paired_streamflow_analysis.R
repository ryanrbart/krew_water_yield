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
# Paired streamflow plots


watershed_id <- c(
  "P301" = "P301: Thinning & Prescribed Fire", "P303" = "P303: Prescribed Fire",
  "P304" = "P304", "D102" = "D102: Thinning",
  "B201" = "B201: Thinning", "B203" = "B203: Prescribed Fire",
  "B204" = "B204: Thinning & Prescribed Fire", "T003" = "T003"
)

paired_function <- function(data, title1, x_label){
  
  plot_paired <- ggplot(data, aes(x = q_control, y = q_treated)) +
    geom_point(aes(size = ndvi_diff, color = ndvi_diff)) +
    geom_smooth(method='lm',se=FALSE, formula=y~x) +
    scale_x_log10(limits = c(40,1600)) +
    scale_y_log10(limits = c(10,2000)) +
    scale_size_continuous(name = expression('NDVI'[diff]),
                          limits = c(-0.128694160, -0.003139498),
                          breaks = c(-0.125, -0.1, -0.075, -0.05, -0.025, 0)) +  
    scale_color_continuous(name = expression('NDVI'[diff]),
                           low= "#b2df8a", high="#1f78b4",
                           limits = c(-0.128694160, -0.003139498),
                           breaks = c(-0.125, -0.1, -0.075, -0.05, -0.025, 0)) +
    guides(colour = guide_legend(),size=guide_legend()) +
    #guides(color= guide_legend(), size=guide_legend())
    labs(title=title1,
         x = x_label,
         y = "Annual Streamflow in\nTreated Watershed (mm)") +
    facet_wrap(.~shed_treated, ncol=3, labeller = labeller(shed_treated = watershed_id),
               strip.position = "left") +
    coord_fixed(ratio = 1) +
    theme_bw(base_size = 12) +
    theme(legend.position="bottom") +
    NULL
  return(plot_paired)
}


wy_NDVI_xcont <- paired_function(data = pair_wy,
                                 title1 = NULL,
                                 x_label = expression('Annual Streamflow'[control]~'(mm)'))

wy_NDVI_xcont_bull <- paired_function(data = dplyr::filter(pair_wy, location=="Bull"),
                                      title1 = "Bull",
                                      x_label = "Annual Streamflow in Control Watershed (T003) (mm)")

wy_NDVI_xcont_prov <- paired_function(data = dplyr::filter(pair_wy, location=="Prov"),
                                      title1 = "Providence",
                                      x_label = "Annual Streamflow in Control Watershed (P304) (mm)")


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Cowplot

plot_legend <- get_legend(wy_NDVI_xcont)


plot_paired_q <- cowplot::plot_grid(wy_NDVI_xcont_bull + theme(legend.position="none"),
                                    wy_NDVI_xcont_prov + theme(legend.position="none"),
                                    plot_legend,
                                    nrow=3,
                                    rel_heights = c(1, 1, 0.15),
                                    align = "v")


save_plot("output/2.2_paired_analysis/plot_paired_q.pdf",
          plot=plot_paired_q,
          base_height = 7.1,
          base_width =  7.5)



