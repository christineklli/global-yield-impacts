

packages <- c("rlang", "mice")


tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

targets_cross_validation <- list(
  # cross validation of 3 models - GAMM RS vs GLMM RS vs LM
  tar_target(model_cv, apply_cv(
    data = crop_imputed_rst_data)),
  # rbind m
  tar_target(model_cv_crop_dt, rbind_model_cv(
    list=model_cv
  )),
  # rbind crop
  tar_target(model_cv_dt, rbind_model_cv_crop(
    list=model_cv_crop_dt
  )),


# tar_read(model_cv_dt) %>% readr::write_csv(here("processed", "model_cv_dt.csv")) # 17/11/22 11.59AM

# for maize - GAMM best on 5/5 m with GLMM in 2nd place
# for rice - GLMM best on 4/5m with GAMM in 2nd place on 4/5m, GAMM best on 1/5 with GLMM in 2nd place
# for soy - GAMM best on 3/5 with GLMM in 2nd place; GLMM best in 2/5 with GAMM in 2nd place
# for wheat - GLMM best on 3/5 with GAMM in 2nd place; GAMM best on 2/5 with GLMM in 2nd place

# ok justification to use GAMM/GLMM

# repeat cv but on all 5 models

tar_target(model_cv_1to5, apply_cv_all_models(
  data = crop_imputed_rst_data)),
# rbind m
tar_target(model_cv_1to5_crop_dt, rbind_model_cv(
  list=model_cv_1to5
)),
# rbind crop
tar_target(model_cv_1to5_dt, rbind_model_cv_crop(
  list=model_cv_1to5_crop_dt
)),
# plot mean RMSE across imputations
tar_target(cv_plot,
           plot_cv(
             data=model_cv_1to5_dt,
             path="results/figures/model_cv_1to5.png"
           ))


)

#tar_read(model_cv_1to5_dt) %>% readr::write_csv(here("processed", "model_cv_1to5_dt.csv")) # 15/12/22 2.18PM
