library(targets)

tar_option_set(packages = c("dplyr"),  format = "rds")
options(clustermq.scheduler = "multicore")
future::plan(future.callr::callr)

list(
  tar_target(cropsx, paste0('crop_', 1:4)),
  tar_target(time_periodsx, paste0('time_', 1:4)),
  tar_target(gcmsx, paste0('gcm_', 1:24)),
  #tar_target(model_specs, paste0('model_', 1:5)),
  tar_target(imputationsx, paste0('imputation_', 1:5)),
  tar_target(
    data_currentx, 
    # This is where you'd construct your model-fitting (current climate, current
    # time) dataset for each combination of crop x spec x imputation method. 
    # Here I just create some random x and y, and add columns for crop, spec, 
    # and imputation to keep track of what's what (instead of adding columns, 
    # since they're constants for each combination.. you could add crop, spec,
    # and impute method as attributes, but then you can't store as fst (would
    # need to use the default 'rds' or 'qs' format). 
    tibble(x=runif(100), y=runif(100), crop=crops, 
           imputation=imputations),
    pattern=cross(crops, imputations), 
    iteration='list',
    format='fst_tbl'
  ),
  tar_target(
    data_futurex,
    # This is where you construct the future conditions to which you want to 
    # predict the models. I think the best way to do this, to accommodate
    # dynamic branching, is to have long format tables including all time 
    # periods x GCMs. We again have one table (and so one branch) for each
    # combination of crop x spec x imputation. 
    replicate(
      24*4, 
      tibble(x=runif(10), y=runif(10), crop=crops, 
             model_spec=model_specs, imputation=imputations),
      simplify=FALSE
    ) %>% bind_rows() %>% 
      mutate(time_period=rep(time_periods, each=240),
             gcm=rep(gcms, each=40)), 
    # ^ add time period and gcm as columns so that we know what we're predicting
    # to.
    pattern=cross(crops, model_specs, imputations), 
    # ^ important that the order of variables here matches the order for
    # data_current
    iteration='list',
    format='fst_tbl'
  ),
  tar_target(model_specsx, c('y ~ x', 'y ~ x + a')),
  tar_target(fitx, 
             lm(formula(model_specs), data=data_current), 
             pattern=cross(model_specs, data_current), 
             iteration='list'),
  #tar_target(
  #  fit, {
  #    m <- lm(y ~ x, data_current)
  #    attr(m, 'crop') <- data_current$crop[1]
  #    attr(m, 'model_spec') <- data_current$model_spec[1]
  #    attr(m, 'imputation') <- data_current$imputation[1]
  #    # ^ add attributes so we know what crop etc. states the model is using
  #    m
  #  }, pattern=map(data_current),
    # ^ here we map over the elements of data_current (i.e. the individual
    # model-fitting datasets) and fit a simple model
  #  iteration='list'
  #),
  # you'll have to think a bit about how to make sure that the predict target 
  #is predicting the correct models to correct datasets
  tar_target(
    predict, 
    mutate(data_future, pred=predict(fit, data_future)), 
    pattern=cross(fit, data_future),
    # ^ map over fit and data_future together, predicting each model
    # (element/branch of fit) to the corresponding future dataset
    # (element/branch of data_future). This is why it's important that the order
    # of variables in the `cross` for data_future and data_current is
    # consistent.
    iteration='vector',
    format='fst_tbl'
  )
)