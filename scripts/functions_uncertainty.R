
create_crop_production_df <- function(raster){
  lapply(1:4, function(crop){
    raster::rasterToPoints(
      raster[[crop]]
    ) %>% 
      as.data.frame() %>% 
      rename(production=3) %>% 
      # so that they will round and left join with predictions
      mutate(x = round(x,2),
             y = round(y,2))
    
    
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
# 

fit_block_bootstrap_nested_first <- function(
    block_bootstrap_samples,
    model_specs, ncores){
  
  future::plan(future::multisession, workers = ncores)
  # dynamically map over 20 crop x m combination fit datasets
  lapply(1:5, function(i) {
    block_bootstrap_samples[[1]][[i]] %>% 
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
  })
}


options(dplyr.summarise.inform = FALSE)

predict_block_bootstrap_nested <- function(fit,
                                           data_og,
                                           data_adj,
                                           crop_production_df){
  
  lapply(1:5, function(i){
    
    fit[[i]] %>% 
      mutate(
        predict_og = purrr::map(model, ~{
          gammit::predict_gamm(
            .x, # model 
            data_og[[1]], 
            re_form = c("s(Country2_fact)"),
            keep_prediction_data = FALSE,
            newdata.guaranteed = TRUE,
            se.fit = FALSE)}),
        
        predict_adj = purrr::map(model, ~{
          gammit::predict_gamm(
            .x,
            data_adj[[1]],
            re_form = c("s(Country2_fact)"),
            keep_prediction_data = FALSE,
            newdata.guaranteed = TRUE,
            se.fit = FALSE) %>% 
            cbind(lon=data_adj[[1]]$lon, 
                  lat=data_adj[[1]]$lat, 
                  gcm=data_adj[[1]]$gcm)
          
        })) %>%
      dplyr::select(!c(model)) %>%
      unnest(c(predict_og, predict_adj), 
             names_repair="unique_quiet") %>% # prediction duplicate
      mutate(predict = .[[5]]-.[[6]]) %>%
      dplyr::select(!c("prediction...5", "prediction...6")) %>%
      left_join(crop_production_df, by=c("lon"="x","lat"="y")) %>%
      filter(!is.infinite(predict)) %>% 
      group_by(id, model_spec, crop, imputation, gcm) %>%
      summarise(weighted_mean=weighted.mean(predict, production, na.rm=TRUE))
    
    
    
  }) 
}


rbind_block_bootstrap_predictions <- function(block_bootstrap_prediction_maize,
                                              block_bootstrap_prediction_rice,
                                              block_bootstrap_prediction_soy,
                                              block_bootstrap_prediction_wheat){
  maize <- reshape2::melt(block_bootstrap_prediction_maize, id.vars=c("id", "model_spec", "crop", "gcm"))
  rice <- reshape2::melt(block_bootstrap_prediction_rice, id.vars=c("id", "model_spec", "crop", "gcm"))
  soy <- reshape2::melt(block_bootstrap_prediction_soy, id.vars=c("id", "model_spec", "crop", "gcm"))
  wheat <- reshape2::melt(block_bootstrap_prediction_wheat, id.vars=c("id", "model_spec", "crop", "gcm"))
  
  rbind(maize,rice,soy,wheat) %>% 
    pivot_wider(., 
                id_cols=c("id", "model_spec", "crop", "gcm"),
                names_from=variable,
                values_from=value,
                values_fn=list,
                names_expand=TRUE) %>% 
    unnest(c(imputation, weighted_mean))
}
# 
# 
# # plot distributions
# 
plot_bootstrap_distributions_gcm <- function(predictions, path){
  
  
  p <- lapply(1:4, function(i){
    predictions[[i]] %>%
      ggplot(
        aes(x=weighted_mean,
            group=interaction(
              model_spec,
              imputation,
              gcm,
              crop
            ),
            col=model_spec)
      ) +
      geom_density(alpha=0.1,
                   linewidth=0.1) +
      theme_bw()
  })
  
  p_grid <- cowplot::plot_grid(
    p[[1]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Maize"),
    p[[2]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Rice"),
    p[[3]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Soybean"),
    p[[4]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Wheat"),
    ncol=2,
    align="v"
  )
  
  legend <- ggpubr::get_legend(p[[1]] + theme(legend.box.margin=margin(0,0,0,12)))
  x.grob <- grid::textGrob("Global production-weighted mean yield change (%)",
                           gp=gpar(fontsize=13))
  y.grob <- grid::textGrob("density",
                           gp=gpar(fontsize=13))
  
  
  plot <- gridExtra::grid.arrange(arrangeGrob(p_grid, right=legend, left=y.grob, bottom=x.grob))
  
  
  outfile <- sprintf(path)
  
  ggsave(outfile, plot)
  
  
}

plot_bootstrap_distributions <- function(predictions, path){
  
  
  p <- lapply(1:4, function(i){
    predictions[[i]] %>%
      ggplot(
        aes(x=weighted_mean,
            group=interaction(
              model_spec,
              imputation,
              #gcm,
              crop
            ),
            col=model_spec)
      ) +
      geom_density(alpha=0.1,
                   linewidth=0.1) +
      theme_bw()
  })
  
  p_grid <- cowplot::plot_grid(
    p[[1]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Maize"),
    p[[2]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Rice"),
    p[[3]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Soybean"),
    p[[4]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Wheat"),
    ncol=4,
    align="v"
  )
  
  legend <- ggpubr::get_legend(p[[1]] + theme(legend.box.margin=margin(0,0,0,12)))
  x.grob <- grid::textGrob("Global production-weighted mean yield change (%)",
                           gp=gpar(fontsize=13))
  y.grob <- grid::textGrob("density",
                           gp=gpar(fontsize=13))
  
  
  plot <- gridExtra::grid.arrange(arrangeGrob(p_grid, right=legend, left=y.grob, bottom=x.grob))
  
  
  outfile <- sprintf(path)
  
  ggsave(outfile, plot, width=8, height=4)
  
  
}


plot_bootstrap_distributions_grid <- function(predictions, path){
  
  
  p <- lapply(1:4, function(i){
    predictions[[i]] %>%
      ggplot(
        aes(x=weighted_mean,
            group=interaction(
              model_spec,
              imputation,
              #gcm,
              crop
            ),
            col=model_spec)
      ) +
      geom_density(alpha=0.1,
                   linewidth=0.1) +
      theme_bw()
  })
  
  p_grid <- cowplot::plot_grid(
    p[[1]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Maize"),
    p[[2]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Rice"),
    p[[3]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Soybean"),
    p[[4]] + theme(legend.position="none",
                   axis.title.y = element_blank(),
                   axis.title.x = element_blank()) +
      labs(subtitle="Wheat"),
    ncol=2,
    align="v"
  )
  
  legend <- ggpubr::get_legend(p[[1]] + theme(legend.box.margin=margin(0,0,0,12)))
  x.grob <- grid::textGrob("Global production-weighted mean yield change (%)",
                           gp=gpar(fontsize=13))
  y.grob <- grid::textGrob("density",
                           gp=gpar(fontsize=13))
  
  
  plot <- gridExtra::grid.arrange(arrangeGrob(p_grid, right=legend, left=y.grob, bottom=x.grob))
  
  
  outfile <- sprintf(path)
  
  ggsave(outfile, plot, width=8, height=4)
  
  
}

# calculate proportions of missing data
# 
# tar_read(AGIMPACTS_bs_tp) %>% 
#   group_by(crop_pooled) %>% 
#   summarise(temp=sum(!is.na(Temp.Change))/n(),
#             yield=sum(!is.na(Yield.Change))/n(),
#             precip=sum(!is.na(Precipitation.change))/n(),
#             co2=sum(!is.na(CO2.Change))/n())
#   
