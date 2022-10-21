
packages <- c("rlang")


tar_option_set(packages = packages,
               memory = "transient", # activate transient memory
               garbage_collection = TRUE, # activate garbage collection
               format = "qs" # efficient storage format, need to first install qs
) 

# pipeline to fit and compare models

targets_model <- list(
  
    tar_target(models, fit_models(
    data=crop_imputed_rst_data,
    spec=model_spec))

  
  
)
