# KREW NDVI and ET mixed-model analysis



source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Import data

out_ndvi_int_bull <- read_rds("output/1.7/out_ndvi_int_bull.rds")
out_ndvi_int_prov <- read_rds("output/1.7/out_ndvi_int_prov.rds")
out_ndvi_n_int_bull <- read_rds("output/1.7/out_ndvi_n_int_bull.rds")
out_ndvi_n_int_prov <- read_rds("output/1.7/out_ndvi_n_int_prov.rds")


# ---------------------------------------------------------------------
# Process mixed modeling output

generate_draws <- function(out_q, normal){
  # Generate the draws for each parameter
  if (normal == TRUE){
    out_q_draws <- out_q %>%
      tidybayes::spread_draws(`(Intercept)`, `ndvi_control_n`, `treatment_dummy1`, `ndvi_control_n:treatment_dummy1`)
  } else {
    out_q_draws <- out_q %>%
      tidybayes::spread_draws(`(Intercept)`, `ndvi_control`, `treatment_dummy1`, `ndvi_control:treatment_dummy1`)
  }
  
  out_q_draws$.chain <- factor(out_q_draws$.chain)
  out_q_draws$.iteration <- factor(out_q_draws$.iteration)
  out_q_draws$.draw <- factor(out_q_draws$.draw)
  
  # Change draws to effect size
  # out_q_draws <- out_q_draws %>%
  #   dplyr::mutate(treatment_effect_size_percent = (exp(treatment_dummy1 * 1) /  exp(treatment_dummy1 * 0))*100 - 100)
  
  return(out_q_draws)
}

out_ndvi_int_bull_draws <- generate_draws(out_ndvi_int_bull, normal=FALSE)
out_ndvi_int_prov_draws <- generate_draws(out_ndvi_int_prov, normal=FALSE)
out_ndvi_n_int_bull_draws <- generate_draws(out_ndvi_n_int_bull, normal=TRUE)
out_ndvi_n_int_prov_draws <- generate_draws(out_ndvi_n_int_prov, normal=TRUE)


# ----
# Combine Bull and Prov

# Regular NDVI
out_ndvi_int_combine_draws <- bind_rows(Bull=out_ndvi_int_bull_draws,
                                        Providence=out_ndvi_int_prov_draws,
                                        .id="pair_type")
out_ndvi_int_combine_draws$pair_type <- factor(out_ndvi_int_combine_draws$pair_type,
                                               levels=c("Providence","Bull"))

# Normalized NDVI
out_ndvi_n_int_combine_draws <- bind_rows(Bull=out_ndvi_n_int_bull_draws,
                                          Providence=out_ndvi_n_int_prov_draws,                                          
                                          .id="pair_type")
out_ndvi_n_int_combine_draws$pair_type <- factor(out_ndvi_n_int_combine_draws$pair_type,
                                                 levels=c("Providence","Bull"))

# ---------------------------------------------------------------------
# Modeling Checks

# # What variables are there?
# get_variables(out_ndvi_dummy)
# 
# 
# # Plot chains through time
# out_ndvi_dummy_draws %>%      
#   ggplot(data=.) +
#   geom_line(aes(y = treatment_dummy1, x = .iteration, group = .chain, col=.chain)) +
#   NULL
# 
# # Evaluate chains
# shinystan::launch_shinystan(out_ndvi_dummy[[1]])
# 
# # Possibly helpful functions
# rstantools::prior_summary(out_ndvi_dummy[[1]])
# rstantools::posterior_interval(out_ndvi_dummy[[1]])
# rstantools::posterior_predict(out_ndvi_dummy[[1]])


# ---------------------------------------------------------------------
# What is the effect size?

broom::tidy(out_ndvi_int_bull)


# ---------------------------------------------------------------------
# Plot mixed modeling output

watershed_id <- c(Bull = "Bull",
                  Providence1 = "Providence")


# Mixed model results: Interaction variable (beta) NDVI
x <- out_ndvi_int_combine_draws %>%      
  ggplot(data=., aes(y = pair_type, x = `ndvi_control:treatment_dummy1`)) +
  ggridges::geom_density_ridges(scale = 0.9, fill="#b2df8a", color=NA, alpha=0.7) +
  stat_pointintervalh(.width = c(0.9, 0.95)) +
  #tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(labels = c(watershed_id)) +
  labs(title = "Paired NDVI Model",
       x = expression('Treatment*NDVI'[control]~'Coefficient ('*beta[0]*')'),
       y = "Watershed Group") +  
  theme_bw(base_size = 13) +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5, vjust=1)) +
  panel_border() + 
  background_grid() +
  #xlim(-12,10) +
  NULL
ggsave("output/1.8/plot_ndvi_int_combine.pdf",plot=x, width = 3.5, height = 3.7)


# Mixed model results: Interaction variable (beta) nNDVI
# x <- out_ndvi_n_int_combine_draws %>%      
#   ggplot(data=., aes(y = pair_type, x = `ndvi_control_n:treatment_dummy1`)) +
#   tidybayes::geom_halfeyeh(.width = c(0.9, 0.95)) +
#   geom_vline(xintercept = 0) +
#   scale_y_discrete(labels = c(watershed_id)) +
#   labs(title = "Change in Paired nNDVI Slope", x = expression('Coefficient ('*beta*')'), y = "Watershed Group") + 
#   theme_tidybayes() +
#   panel_border() + 
#   background_grid() +
#   NULL
# ggsave("output/1.8/plot_ndvi_n_int_combine.jpg",plot=x, width = 5, height = 3)




# ---------------------------------------------------------------------
# Save output
write_rds(out_ndvi_int_combine_draws, "output/1.8/out_ndvi_int_combine_draws.rds")
# write_rds(out_ndvi_n_int_combine_draws, "output/1.8/out_ndvi_n_int_combine_draws.rds")



