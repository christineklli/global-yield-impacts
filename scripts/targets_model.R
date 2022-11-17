
packages <- c("rlang", "mice")


tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

# pipeline to fit and compare models

targets_model_gam <- list(
    # fit models on data with different model specifications
    tar_target(models, fit_models(
    data=crop_imputed_rst_data,
    spec=model_spec)),
    # plot multiply imputed fit as pooled prediction fitted lines
    # note multiply_fit is then a non-nested list of 4 crops x 5 imp = 20 model estimates
    tar_target(multiply_fit_gam, multiply_imp_for_plot_gam(
      data=crop_list_imp
    )),
    # expand grid for new prediction pooled lines data
    # cannot fit fit_lines() on nd, so ignore for now
    tar_target(nd, new_data(
      data=AGIMPACTS_bs
    )),
    # fit lines
    tar_target(fitted_lines_gam, fit_lines(
      fit=multiply_fit_gam,
      data=AGIMPACTS_bs
    )),
    tar_target(grouped_lines_gam, group_fitted_lines(
      fitted_lines=fitted_lines_gam
    )),
    tar_target(grouped_lines_df_gam, rbind_grouped_lines(
      grouped_lines=grouped_lines_gam
    )),
    # extend these plots further to include precipitation change
    # and f_CO2 change
    # also, unconvinced about the prediction confidence intervals - so narrow!
    # at least relative to the actual data points
    # do they actually capture the full within-m variance?
    # though, this is similar to previous plots
    tar_target(pooled_fit_plot_gam,
               plot_pooled_lines(
                 predictions=grouped_lines_df_gam,
                 data=AGIMPACTS_bs
               )),
    # save plot
    tar_target(saved_response_functions_gam,
               save_plots_reponse_functions(
                 plots=pooled_fit_plot_gam,
                 path="results/figures/pooled_fit_plot_gam.png"
               ))

  
)

# fit LM model to imputed data

targets_model_lm <- list(
  tar_target(lm_models, fit_lm(
    data=crop_imputed_rst_data,
    spec=lm_model_spec
  )),
  # plot multiply imputed fit as pooled prediction fitted lines
  # note multiply_fit is then a non-nested list of 4 crops x 5 imp = 20 model estimates
  tar_target(multiply_fit_lm, multiply_imp_for_plot_lm(
    data=crop_list_imp
  )),
  # fit lines
  tar_target(fitted_lines_lm, fit_lines(
    fit=multiply_fit_lm,
    data=AGIMPACTS_bs
  )),
  tar_target(grouped_lines_lm, group_fitted_lines(
    fitted_lines=fitted_lines_lm
  )),
  tar_target(grouped_lines_df_lm, rbind_grouped_lines(
    grouped_lines=grouped_lines_lm
  )),
  # extend these plots further to include precipitation change
  # and f_CO2 change
  # also, unconvinced about the prediction confidence intervals - so narrow!
  # at least relative to the actual data points
  # do they actually capture the full within-m variance?
  # though, this is similar to previous plots
  tar_target(pooled_fit_plot_lm,
             plot_pooled_lines(
               predictions=grouped_lines_df_lm,
               data=AGIMPACTS_bs
             )),
  # save plot
  tar_target(saved_response_functions_lm,
             save_plots_reponse_functions(
               plots=pooled_fit_plot_lm,
               path="results/figures/pooled_fit_plot_lm.png"
             ))
)

# cross validation
