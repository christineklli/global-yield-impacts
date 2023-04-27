

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
           )),
tar_target(cv_plot_all_m,
           plot_all_m_cv(
             data=model_cv_1to5_dt,
            path="results/figures/model_cv_1to5_all.png"
           )),

# extract fit estimates into datatable by attr
tar_target(fit_estimates_tbl,
           
           {
             t <- lapply(1:length(fit_attr), function(i){ # do this for all 25 models of fit_attr
               
               data.frame(
                 crop=attr(fit_attr[[i]],"crop"),
                 imputation=attr(fit_attr[[i]],"imputation"),
                 model_spec=attr(fit_attr[[i]],"model_specs"),
                 r.sq=summary(fit_attr[[i]])$r.sq,
                 dev.expl=summary(fit_attr[[i]])$dev.expl,
                 n=summary(fit_attr[[i]])$n
               ) 
             })
             
             t <- rbindlist(t)
             
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
),

# plot in a faceted grid with full distribution
tar_target(dev_explained_plot_all,
           {
             
             mean <- fit_estimates_tbl %>% 
               group_by(crop, model) %>% 
               summarise(mean_r.sq=mean(r.sq),
                         mean_dev.expl=mean(dev.expl)) 
             
             dat <- fit_estimates_tbl %>% 
               left_join(mean, by=c("crop", "model"))
             
             plot <- ggplot(data=dat) +
               geom_point(
                 aes(x=model, y=dev.expl,
                     col=model), size=2, alpha=0.7) +
               geom_point(
                 aes(x=model, y=mean_dev.expl), 
                 col="black", shape=21, size = 2, alpha=1) +
              
               facet_wrap(~crop, ncol=4
               ) +
              

               theme(axis.title.x=element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks = element_blank()
               ) +
               scale_colour_manual(
                 name="Model ID",
                 labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
                 breaks=c("gam_RS","gam_RI","glm_RS","glm_RI","lm"),
                 values=c("darksalmon","darkkhaki","aquamarine3","deepskyblue","darkorchid1")
               ) +
               labs(y="Deviance Explained") 
             

             ggplot2::ggsave(filename="results/figures/dev_explained_plot_all.png",
                             plot=plot,
                             width=7, height=6)

             plot
           }
),

# not just mean RMSE and dev.expl but all m
tar_target(cv_devexp_plot_all,
           {library(patchwork)
             
             plot <-  cv_plot_all_m / dev_explained_plot_all
             ggplot2::ggsave(filename="results/figures/combined_cv_devexp_plot_all.png",
                             plot=plot,
                             width=10, height=7
                             )
             
             plot
           }
)


)
