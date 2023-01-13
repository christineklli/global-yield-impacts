
create_crop_production_df <- function(raster){
  lapply(1:4, function(crop){
    raster::rasterToPoints(
      raster[[crop]]
    ) %>% 
      as.data.frame() %>% 
      rename(production=3)
  })
}

calc_global_weighted_mean_prediction <- function(prediction, crop_production_df, crop_no, model_specs){
  
  t <- prediction %>%
    left_join(crop_production_df[[crop_no]], by=c("lon"="x","lat"="y")) %>%
    filter(!is.infinite(prediction.fit)) %>% 
    group_by(crop_pooled, model_spec, time_period, gcm, m) %>%
    summarise(weighted_mean=weighted.mean(prediction.fit, production, na.rm=TRUE)) 
  
  df <- data.frame(model_spec=model_specs,
                   model=c("gam_RS",
                           "gam_RI",
                           "glm_RS",
                           "glm_RI",
                           "lm"))
  t %>% left_join(df, by=c("model_spec")) %>% 
    relocate(model) %>% 
    dplyr::select(!model_spec)
  
}

# distributions of each model spec x m pooled across 23 GCMs
plot_model_m_distributions <- function(predictions, time_periods, path, crops){
  
  lapply(1:4, function(crop){
    
    p <- lapply(1:4, function(i){ 
      
      predictions[[crop]] %>% 
        filter(time_period %in% time_periods[[i]]) %>% 
        ggplot( 
          aes(x=weighted_mean, 
              group=interaction(model, m), 
              colour=model)) +
        geom_density() +
        theme_bw()
      
    })
    
    p_grid <- cowplot::plot_grid(
      p[[1]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()), 
      p[[2]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()), 
      p[[3]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()), 
      p[[4]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()),
      ncol=2,
      labels=c("2021-2040","2041-2060","2061-2080","2081-2100"),
      label_fontface="plain",
      align="v"
    )
    
    legend <- ggpubr::get_legend(p[[1]] + theme(legend.box.margin=margin(0,0,0,12)))
    x.grob <- grid::textGrob("Global crop production-weighted mean yield change (%)",
                             gp=gpar(fontsize=15))
    y.grob <- grid::textGrob("density",
                             gp=gpar(fontsize=15))
    
    plot <- gridExtra::grid.arrange(arrangeGrob(p_grid, right=legend, left=y.grob, bottom=x.grob))
    
    outfile <- sprintf(path, 
                       crops[[crop]])
    
    ggsave(outfile, plot)
    
    
  })
  
}

# distributions of each model spec pooled across 5 m x 23 GCMs
plot_model_pooled_m_distributions <- function(predictions, time_periods, path, crops){
  
  lapply(1:4, function(crop){
    
    p <- lapply(1:4, function(i){
      
      predictions[[crop]] %>% 
        filter(time_period %in% time_periods[[i]]) %>% 
        ggplot( 
          aes(x=weighted_mean, 
              group=model, # here it changes 
              colour=model,
              fill=model)) +
        geom_density(alpha=0.4) +
        theme_bw()
      
    })
    
    p_grid <- cowplot::plot_grid(
      p[[1]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()), 
      p[[2]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()), 
      p[[3]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()), 
      p[[4]] + theme(legend.position="none",
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank()),
      ncol=2,
      labels=c("2021-2040","2041-2060","2061-2080","2081-2100"),
      label_fontface="plain",
      align="v"
    )
    
    legend <- ggpubr::get_legend(p[[1]] + theme(legend.box.margin=margin(0,0,0,12)))
    x.grob <- grid::textGrob("Global crop production-weighted mean yield change (%)",
                             gp=gpar(fontsize=15))
    y.grob <- grid::textGrob("density",
                             gp=gpar(fontsize=15))
    
    plot <- gridExtra::grid.arrange(arrangeGrob(p_grid, right=legend, left=y.grob, bottom=x.grob))
    
    outfile <- sprintf(path, 
                       crops[[crop]])
    
    ggsave(outfile, plot)
    
    
  })
  
}



# sampling uncertainty ----------------------------------------------------


create_block_bootstrap_samples <- function(data, ncores){ # crop_imputed_rst_data[[crop]][[m]]
  
  future::plan(future::multisession, workers = ncores)
  
  # for each crop and m
  future.apply::future_lapply(1:4, function(crop){
    lapply(1:5, function(m){
      
      # create 1000 random block samples of past data to fit model to 
      block_data <- data[[crop]][[m]] %>% 
        mutate(imputation=m) %>% 
        group_by(Reference_fact) %>% 
        nest()
      
      rsample::bootstraps(block_data, times = 100) 
      
      
      # repeat on each crop and m combination    
    })
  }, future.seed=TRUE)
  
}


fit_block_bootstrap <- function(block_bootstrap_samples_unnested, 
                                model_specs, ncores){
  
  future::plan(future::multisession, workers = ncores)
  # dynamically map over 20 crop x m combination fit datasets
  block_bootstrap_samples_unnested[[1]] %>% 
    # for each combination dataset, map over each of 100 bootstrapped samples
    mutate(data=furrr::future_map(splits, ~{
      as_tibble(rsample::analysis(.x)) %>% # this manages to force class(boot_split, rsplit) into data.frame() 
        unnest(cols=c(data))
    }),
    # to fit 5 model specifications
    model=purrr::map(data, ~{mgcv::gam(formula(str_replace_all(model_specs, "[\r\n]", "")),
                                       method = 'REML',
                                       family = 'gaussian',
                                       data=.x)}),
    #model_tidy=map(model,tidy), # this is nice but unnecessary for our prediction purposes
    # add model attributes to identify crop/model/m states 
    crop=purrr::map(data, ~{.x$crop_pooled[1]}),
    model_specs=purrr::map(data, ~{model_specs[1]}),
    imputation=purrr::map(data, ~{.x$imputation[1]}))  %>% 
    unnest(c(crop,model_specs,imputation))
  
  
}

predict_block_bootstrap <- function(fit,
                                    data_og,
                                    data_adj
                                    #ncores
                                    # then add data_adj
){
  #future::plan(future::multisession, workers = ncores)
  
  fit %>% 
    mutate(
      predict_og = purrr::map(model, ~{
        gammit::predict_gamm(
          .x, # model 
          data_og, 
          re_form = c("s(Country2_fact)"),
          keep_prediction_data = FALSE,
          newdata.guaranteed = TRUE,
          se.fit = FALSE) 
      }),
      predict_adj = purrr::map(model, ~{
        gammit::predict_gamm(
          .x,
          data_adj,
          re_form = c("s(Country2_fact)"),
          keep_prediction_data = FALSE,
          newdata.guaranteed = TRUE,
          se.fit = FALSE)
        
      })) %>% 
    dplyr::select(!c(model)) %>% 
     unnest(c(predict_og, predict_adj), names_repair="unique") %>% # prediction duplicate 
    mutate(predict = .[[5]]-.[[6]]) %>% 
    dplyr::select(!c("prediction...5", "prediction...6"))
  
}
# 
# fit <- tar_read(block_bootstrap_fit_only_15124995)
# fit <- fit %>% head(., 2)
# 
# predict <- predict_block_bootstrap(
#   fit=fit,
#   data_og=tar_read(data_future_maize_nested)[[4]], # only predict on 2081-2100 data
#   data_adj=tar_read(int_adj_future_maize)[[4]]
# )

# from here we can add predict_zero in the same mutate
# and then deselect model
# and then mutate predict_adj by subtracting predict_zero from predict_og
# and then left join each bootstrap id prediction tibble with crop production data
# then do summarise weighted mean
# also weight by area
# left join; worldmap_clean@data$area_sqkm <- raster::area(worldmap_clean)/1000000
#data.frame(ADMIN=worldmap_clean@data$ADMIN,
#           ISO_A2=worldmap_clean@data$ISO_A2,
#           area_sqkm=worldmap_clean@data$area_sqkm
# then we would be left with 100 bootstrap id rows x 23 gcm weighted means per 25 batches per 4 crops
# so each batch would only have 2300 rows; tiny!
# then rbind them all together so that we have a df of predictions with 2300 x 25 x 4 = 230,000 rows


# then group_by() for distributions
# plot another function
# calculate modelling & sampling uncertainty in another function

