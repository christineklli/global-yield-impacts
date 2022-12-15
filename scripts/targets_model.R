
packages <- c("rlang", "mice")


tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

# pipeline to fit and compare models

# see whether these could be a list of 4 targets instead? listed? need to edit structure

# Random intercepts AND random slopes -------------------------------------


# GAM RS

targets_model_gam_glm <- list(
    # fit models on data with different model specifications
    tar_target(models, fit_models(
    data=crop_imputed_rst_data,
    spec=model_spec)),
    # plot multiply imputed fit as pooled prediction fitted lines
    # note multiply_fit is then a non-nested list of 4 crops x 5 imp = 20 model estimates
    # NOTE THAT WITH::MIDS does NOT take spec[[k]]
    # GAM RS
    tar_target(multiply_fit_gam_RS, multiply_imp_for_plot_gam_RS(
      data=crop_list_imp
    )),
    # GAM RI
    tar_target(multiply_fit_gam_RI, multiply_imp_for_plot_gam_RI(
      data=crop_list_imp
    )),
    # GLM RS
    tar_target(multiply_fit_glm_RS, multiply_imp_for_plot_glm_RS(
      data=crop_list_imp
    )),
    # GLM RI
    tar_target(multiply_fit_glm_RI, multiply_imp_for_plot_glm_RI(
      data=crop_list_imp
    )),
    # LM
    tar_target(multiply_fit_lm, multiply_imp_for_plot_lm(
      data=crop_list_imp
    )),
    # list
    tar_target(multiply_fit_list,
               list_multiply_fit(
                 list1=multiply_fit_gam_RS,
                 list2=multiply_fit_gam_RI,
                 list3=multiply_fit_glm_RS,
                 list4=multiply_fit_glm_RI,
                 list5=multiply_fit_lm
               )),
    # expand grid for new prediction pooled lines data
    # cannot fit fit_lines() on nd, so ignore for now
    #tar_target(nd, new_data(
    #  data=AGIMPACTS_bs
    #)),
    # fit lines
    tar_target(fitted_lines, fit_lines( 
      fit=multiply_fit_list,
      data=AGIMPACTS_bs
    )),
    tar_target(grouped_lines, group_fitted_lines(
      fitted_lines=fitted_lines
    )),
    tar_target(grouped_lines_df, rbind_grouped_lines(
      grouped_lines=grouped_lines
    )),
    # extend these plots further to include precipitation change
    # and f_CO2 change
    # also, unconvinced about the prediction confidence intervals - so narrow!
    # at least relative to the actual data points
    # do they actually capture the full within-m variance?
    # though, this is similar to previous plots
    
    tar_target(pooled_fit_plot,
               plot_pooled_lines(
                 predictions=grouped_lines_df,
                 data=AGIMPACTS_bs
               )),
    # save plot
    tar_target(saved_response_functions_gam_RS,
               save_plots_reponse_functions(
                 plots=pooled_fit_plot,
                 spec_no=1,
                 path="results/figures/pooled_fit_plot_gam_RS.png"
               )),
    tar_target(saved_response_functions_gam_RI,
               save_plots_reponse_functions(
                 plots=pooled_fit_plot,
                 spec_no=2,
                 path="results/figures/pooled_fit_plot_gam_RI.png"
               )),
    tar_target(saved_response_functions_glm_RS,
               save_plots_reponse_functions(
                 plots=pooled_fit_plot,
                 spec_no=3,
                 path="results/figures/pooled_fit_plot_glm_RS.png"
               )),
    tar_target(saved_response_functions_glm_RI,
               save_plots_reponse_functions(
                 plots=pooled_fit_plot,
                 spec_no=4,
                 path="results/figures/pooled_fit_plot_glm_RI.png"
               )),
    tar_target(saved_response_functions_lm,
               save_plots_reponse_functions(
                 plots=pooled_fit_plot,
                 spec_no=5,
                 path="results/figures/pooled_fit_plot_lm.png"
               )),
    
    
    # rbindlist grouped_lines_df by model_spec
    tar_target(grouped_lines_tbl,
               rbindlist(grouped_lines_df, idcol="model_spec")),
    
    # plot all response functions on one plot
    tar_target(all_response_function_plots,
               plot_all_response_functions(
                 predictions=grouped_lines_tbl,
                 path="results/figures/all_response_function_plots.png"
               )),
    # plot with data
    tar_target(all_response_functions_plots_with_data,
               plot_all_response_functions_with_data(
                 predictions=grouped_lines_tbl,
                 data=AGIMPACTS_bs,
                 path="results/figures/all_response_function_plots_with_data.png"
               ))
  
)
