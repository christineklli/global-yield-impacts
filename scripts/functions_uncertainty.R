
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

