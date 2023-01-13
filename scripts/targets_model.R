
packages <- c("rlang", "mice", "dplyr")


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
             )),
  
  # adjust response function prediction data to force response through the origin
  tar_target(adj_grouped_lines_df,
             {
               lapply(1:5, function(model){
                 
                 fit_zero <- grouped_lines_df[[model]] %>% 
                   group_by(crop) %>% 
                   filter(x == 0.00) %>% 
                   dplyr::select(fit_bar) %>% 
                   rename(fit_bar_zero=fit_bar)
                 
                 grouped_lines_df[[model]] %>% 
                   left_join(fit_zero, by=c("crop")) %>% 
                   mutate(fit_bar_adj=fit_bar-fit_bar_zero,
                          lwr_p_adj=lwr_p-fit_bar_zero,
                          upr_p_adj=upr_p-fit_bar_zero) %>% 
                   dplyr::select(!c("fit_bar", "lwr_p", "upr_p")) %>% 
                   rename(fit_bar=fit_bar_adj,
                          lwr_p=lwr_p_adj,
                          upr_p=upr_p_adj) # rename so that we can use the same plot function
                 
                 # adjust confidence intervals
                 
               })
               
             }),
  
  tar_target(adj_grouped_lines_tbl,
             rbindlist(adj_grouped_lines_df, idcol="model_spec")),
  
  tar_target(adj_all_response_functions_plots,
             plot_all_response_functions(
               predictions=adj_grouped_lines_tbl,
               path="results/figures/adj_all_response_function_plots.png"
             )),
  
  tar_target(lit_responses_1k, # per temperature degree estimates of yield impact
             {lit <- dplyr::tribble(
               ~Study, ~Maize, ~Rice, ~Soy, ~Wheat,
               "Liu et al. 2016", NA, NA, NA, -5.7,
               "Wilcox & Makowski 2014", NA, NA, NA, -3.9,
               "Fischer et al. 2014", NA, NA, NA, -5.9,
               "Wang et al. 2020", -7.1, -5.6, -10.6, -2.9)
             
             lit <- lit %>% 
               tidyr::pivot_longer(!Study, names_to="crop_name", values_to="fit_bar")
             
             crops_concord <- data.frame(crop_name=crops,
                                         crop=seq(1,4,1)) 
             
             lit %>% 
               left_join(crops_concord, by=c("crop_name")) %>% 
               mutate(x=1)
             
             }),
  # and then plot as red diamonds with text labels, faceted by crop
  tar_target(adj_all_response_functions_plots_with_lit,
             plot_all_response_functions_with_lit(
               data=lit_responses_1k,
               predictions=adj_grouped_lines_tbl,
               path="results/figures/adj_all_response_function_plots_with_lit.png"
             ))
  
)
