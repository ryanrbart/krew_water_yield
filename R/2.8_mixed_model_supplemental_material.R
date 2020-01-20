# KREW mixed-model results: convergence figures and table summaries


source("R/0_utilities.R")

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Import data

# Mixed model input data
pair_q95 <- read_rds(PAIR_Q95_RDS)
pair_seasonal <- read_rds(PAIR_SEASONAL_RDS)
pair_wy <- read_rds(PAIR_WY_RDS)

pair_seasonal_1 <- dplyr::filter(pair_seasonal, Season==1)
pair_seasonal_2 <- dplyr::filter(pair_seasonal, Season==2)
pair_seasonal_3 <- dplyr::filter(pair_seasonal, Season==3)
pair_seasonal_4 <- dplyr::filter(pair_seasonal, Season==4)


# Import mixed model results
out_ndvi_int_bull <- read_rds("output/1.7/out_ndvi_int_bull.rds")
out_ndvi_int_prov <- read_rds("output/1.7/out_ndvi_int_prov.rds")

out_wy_diff_bull <- read_rds("output/2.4_mixed_model/out_wy_diff_bull.rds")
out_wy_diff_prov <- read_rds("output/2.4_mixed_model/out_wy_diff_prov.rds")

out_QP2_bull <- read_rds("output/2.4_mixed_model/out_QP2_bull.rds")
out_QP2_prov <- read_rds("output/2.4_mixed_model/out_QP2_prov.rds")

# Import mixed model draws
out_ndvi_int_combine_draws <- read_rds("output/1.8/out_ndvi_int_combine_draws.rds")
out_q_diff_draws <- read_rds("output/2.5_mixed_model_betas/out_q_diff_draws.rds")
out_QP2_draws <- read_rds("output/2.5_mixed_model_betas/out_QP2_draws.rds")



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate mixed model traceplots (convergence figures)


# ------------------------------
# Paired NDVI Model

# Stack the parameters
out_ndvi_int_combine_draws2 <- out_ndvi_int_combine_draws %>% 
  tidyr::pivot_longer(cols = c(`(Intercept)`, ndvi_control, treatment_dummy1, `ndvi_control:treatment_dummy1`),
                      names_to = "parameter", values_to = "value")
out_ndvi_int_combine_draws2$.draw <- as.double(out_ndvi_int_combine_draws2$.draw)


# Needed to add levels to get expressions to properly work https://stackoverflow.com/questions/37089052/
out_ndvi_int_combine_draws2$parameter <- factor(out_ndvi_int_combine_draws2$parameter)
levels(out_ndvi_int_combine_draws2$parameter) <- c("Intercept",
                                                   expression('NDVI'[c]),
                                                   expression('NDVI'[c]*'*T'),
                                                   "T")                           # Note that this order has to match factoring order

# Plot chains through time
x <- out_ndvi_int_combine_draws2 %>%
  ggplot(data=.) +
  geom_line(aes(x = .draw, y = value, group=parameter)) +
  facet_grid(rows = vars(parameter), cols = vars(pair_type),
             labeller = labeller(.rows = label_parsed),
             scales = "free_y") +
  labs(title = "Paired NDVI Model",
       x="Iteration",
       y="Parameter Value") +
  theme_bw(base_size = 12) +
  NULL
ggsave("output/2.8_sup_material/plot_chains_paired_ndvi_model.pdf", plot=x, width = 8, height = 6)



# ------------------------------
# Paired Streamflow Model

watershed_id <- c(
  "bull_data" = "Bull",
  "prov_data" = "Providence"
)

# Stack the parameters
out_q_diff_draws2 <- out_q_diff_draws %>% 
  tidyr::pivot_longer(cols = c(`(Intercept)`,`log(q_control)`,ndvi_diff),
                      names_to = "parameter", values_to = "value")
out_q_diff_draws2$.draw <- as.double(out_q_diff_draws2$.draw)


# Needed to add levels to get expressions to properly work https://stackoverflow.com/questions/37089052/
out_q_diff_draws2$parameter <- factor(out_q_diff_draws2$parameter)
levels(out_q_diff_draws2$parameter) <- c(`(Intercept)` = "Intercept",
                                         `log(q_control)` = expression('Q'[c]),
                                         ndvi_diff = expression('NDVI'[diff]))          # Note that this order has to match factoring order

# Plot chains through time
x <- out_q_diff_draws2 %>%
  ggplot(data=.) +
  geom_line(aes(x = .draw, y = value, group=parameter)) +
  facet_grid(rows = vars(parameter), cols = vars(model),
             labeller = labeller(.rows = label_parsed, .cols=watershed_id),
             scales = "free_y") +
  labs(title = "Paired Streamflow Model",
       x="Iteration",
       y="Parameter Value") +
  theme_bw(base_size = 12) +
  NULL
ggsave("output/2.8_sup_material/plot_chains_paired_q_model.pdf", plot=x, width = 8, height = 5)



# ------------------------------
# Longitudinal Model


# Stack the parameters
out_QP2_draws2 <- out_QP2_draws %>% 
  tidyr::pivot_longer(cols = c(`(Intercept)`,`log(precip)`,`log(p_lag)`, ndvi_annual),
                      names_to = "parameter", values_to = "value")
out_QP2_draws2$.draw <- as.double(out_QP2_draws2$.draw)


# Needed to add levels to get expressions to properly work https://stackoverflow.com/questions/37089052/
out_QP2_draws2$parameter <- factor(out_QP2_draws2$parameter)
levels(out_QP2_draws2$parameter) <- c("Intercept",
                                      expression('P'[t_lag]),
                                      expression('P'[t]),
                                      expression('NDVI'[t]))          # Note that this order has to match factoring order

# Plot chains through time
x <- out_QP2_draws2 %>%
  ggplot(data=.) +
  geom_line(aes(x = .draw, y = value, group=parameter)) +
  facet_grid(rows = vars(parameter), cols = vars(model),
             labeller = labeller(.rows = label_parsed, .cols=watershed_id),
             scales = "free_y") +
  labs(title = "Longitudinal Model",
       x="Iteration",
       y="Parameter Value") +
  theme_bw(base_size = 12) +
  NULL
ggsave("output/2.8_sup_material/plot_chains_longitudinal_model.pdf", plot=x, width = 8, height = 6)



# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Generate summary tables of results


make_results_table <- function(out_file, path_file, model, shed){
  
  tmp <- out_file$stan_summary %>% 
    as_tibble() %>% 
    bind_cols(tibble(name = rownames(out_file$stan_summary)), .) %>% 
    dplyr::select(c(name, mean, sd, `2.5%`, `97.5%`)) %>% 
    dplyr::slice(-((nrow(.)-4):nrow(.)))
  
  if (model == 1 && shed == "Bull"){
    tmp$name = c("Intercept","NDVIc","T","NDVIc*T",
                 "b[B201]","b[B203]","b[B204]")
  }
  if (model == 1 && shed == "Prov"){
    tmp$name = c("Intercept","NDVIc","T","NDVIc*T",
                 "b[D102]","b[P301]","b[P303]")
  }
  if (model == 2 && shed == "Bull"){
    tmp$name = c("Intercept","log(Qc)","NDVIdiff",
                 "b[B201]","b[B203]","b[B204]")
  }
  if (model == 2 && shed == "Prov"){
    tmp$name = c("Intercept","log(Qc)","NDVIdiff",
                 "b[D102]","b[P301]","b[P303]")
  }
  if (model == 3 && shed == "Bull"){
    tmp$name = c("Intercept","log(Pt)","log(Pt_lag)","NDVIt",
                 "b[B201]","b[B203]","b[B204]")
  }
  if (model == 3 && shed == "Prov"){
    tmp$name = c("Intercept","log(Pt)","log(Pt_lag)","NDVIt",
                 "b[D102]","b[P301]","b[P303]")
  }
  
  ft <- tmp %>% 
    flextable::flextable() %>% 
    flextable::align(align = "left", part = "all") %>% 
    flextable::set_header_labels(name = "Coefficient Name",
                                 mean = "Mean",
                                 sd = "Standard Deviation") %>% 
    # See https://cran.r-project.org/web/packages/flextable/vignettes/layout.html in section 'Groups as row titles' for below code
    compose(i = ~ !is.na(mean), j = "mean", value = as_paragraph(as_chunk(mean, formater = function(x) sprintf("%.3f", x)))) %>% 
    compose(i = ~ !is.na(sd), j = "sd", value = as_paragraph(as_chunk(sd, formater = function(x) sprintf("%.3f", x)))) %>% 
    compose(i = ~ !is.na(`2.5%`), j = "2.5%", value = as_paragraph(as_chunk(`2.5%`, formater = function(x) sprintf("%.3f", x)))) %>% 
    compose(i = ~ !is.na(`97.5%`), j = "97.5%", value = as_paragraph(as_chunk(`97.5%`, formater = function(x) sprintf("%.3f", x)))) %>% 
    autofit(add_w = 0, add_h = 0)
  print(ft)
  
  doc <- officer:::read_docx()
  doc <- flextable::body_add_flextable(doc, value = ft)
  print(doc, target = path_file)
}


make_results_table(out_file = out_ndvi_int_bull, 
                   path_file = "output/2.8_sup_material/table_paired_ndvi_model_bull.docx",
                   model=1,
                   shed="Bull")
make_results_table(out_file = out_ndvi_int_prov, 
                   path_file = "output/2.8_sup_material/table_paired_ndvi_model_prov.docx",
                   model=1,
                   shed="Prov")

make_results_table(out_file = out_wy_diff_bull, 
                   path_file = "output/2.8_sup_material/table_paired_q_model_bull.docx",
                   model=2,
                   shed="Bull")
make_results_table(out_file = out_wy_diff_prov, 
                   path_file = "output/2.8_sup_material/table_paired_q_model_prov.docx",
                   model=2,
                   shed="Prov")

make_results_table(out_file = out_QP2_bull, 
                   path_file = "output/2.8_sup_material/table_longitudinal_model_bull.docx",
                   model=3,
                   shed="Bull")
make_results_table(out_file = out_QP2_prov, 
                   path_file = "output/2.8_sup_material/table_longitudinal_model_prov.docx",
                   model=3,
                   shed="Prov")



# summary(out_ndvi_int_bull, digits=3, probs = c(0.025, 0.975))
# summary(out_ndvi_int_prov, digits=3, probs = c(0.025, 0.975))
# 
# summary(out_wy_diff_bull, digits=3, probs = c(0.025, 0.975))
# summary(out_wy_diff_prov, digits=3, probs = c(0.025, 0.975))
# 
# summary(out_QP2_bull, digits=3, probs = c(0.025, 0.975))
# summary(out_QP2_prov, digits=3, probs = c(0.025, 0.975))


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Comparison of observed values and average predicted values
# This is an evaluation of dependent variable fit, not strength of NDVI variable.


pp_check(out_QP2_bull, plotfun = "scatter_avg") 




# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Autocorrelation







# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Extra-ness



# What variables are there?
#get_variables(out_wy_diff_prov)

# Evaluate chains
shinystan::launch_shinystan(out_wy_diff_prov)

# Possibly helpful functions
rstantools::prior_summary(out_wy_diff_prov)
rstantools::posterior_interval(out_wy_diff_prov)
rstantools::posterior_predict(out_wy_diff_prov)

out_wy_diff_prov



