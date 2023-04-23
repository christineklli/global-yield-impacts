

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

# plot dev.expl for temperature and precipitation
tar_target(dev_explained_plot,
           {
             dat <- fit_estimates_tbl %>% 
               group_by(crop, model) %>% 
               summarise(mean_r.sq=mean(r.sq),
                         mean_dev.expl=mean(dev.expl)) 
             
             plot <- ggplot(data=dat) +
               geom_point(
                 aes(x=crop, y=mean_dev.expl,
                     shape=model, col=model), size = 4, alpha=0.7) +
               # I want these to match with cv_plot in targets_cross_validation.R
               # so that we can plot these on same plot
               scale_shape_manual(
                 name="Model ID",
                 labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
                 breaks=c("gam_RS","gam_RI","glm_RS","glm_RI","lm"),
                 values=c(16,17,15,3,7)
               ) +
               scale_colour_manual(
                 name="Model ID",
                 labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
                 breaks=c("gam_RS","gam_RI","glm_RS","glm_RI","lm"),
                 values=c("darksalmon","darkkhaki","aquamarine3","deepskyblue","darkorchid1")
               ) +
               labs(x="Crop", y="Mean Deviance Explained") 
             
             ggplot2::ggsave(filename="results/figures/dev_explained_plot.png",
                             plot=plot,
                             width=7, height=6)
             
             plot
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
               # I want these to match with cv_plot in targets_cross_validation.R
               # # so that we can plot these on same plot
               # scale_shape_manual(
               #   name="Model ID",
               #   labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
               #   breaks=c("gam_RS","gam_RI","glm_RS","glm_RI","lm"),
               #   values=c(16,17,15,3,7)
               # ) +
               facet_wrap(~crop, ncol=4
               ) +
               # scale_colour_discrete(
               #   name="Model ID",
               #   labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM")#,
               #   #breaks=c("1","2","3","4","5")
               # ) +

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


# combine cv_plot and dev_explained_plot
tar_target(cv_devexp_plot,
           {library(patchwork)
             
             # remove duplicate legend as model colour + shape scale values now match
             
             dat <- model_cv_1to5_dt %>% 
               group_by(crop_no, `Model ID`, Dependent, Fixed) %>% 
               summarise(mean_RMSE=mean(RMSE),
                         mean_MAE=mean(MAE)) %>% 
               mutate(`Model ID`=as.factor(`Model ID`)) %>% 
               mutate(crop_no=as.factor(crop_no))
             
             cv_plot <- ggplot(data=dat) +
               geom_point(
                 aes(x=crop_no, y=mean_RMSE,
                     shape=`Model ID`, col=`Model ID`), size = 4, alpha=0.5) +
               scale_shape_discrete(
                 name="Model ID",
                 labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
                 breaks=c("1","2","3","4","5")
               ) +
               scale_colour_discrete(
                 name="Model ID",
                 labels=c("GAM RS", "GAM RI", "GLM RS", "GLM RI", "LM"),
                 breaks=c("1","2","3","4","5")
               ) +
               labs(y="Mean Model RMSE from k-fold CV") +
               scale_x_discrete(name="Crop",
                                labels=c("Maize","Rice","Soy","Wheat"),
                                breaks=c("1","2","3","4")) + 
               theme(legend.position="none") # REMOVE HERE
             
             plot <-  cv_plot + dev_explained_plot
             ggplot2::ggsave(filename="results/figures/combined_cv_devexp_plot.png",
                             plot=plot,
                             width=7, height=6)
             
             plot
           }
),
# not just mean RMSE and dev.expl but all of them by m
tar_target(cv_devexp_plot_all,
           {library(patchwork)
             
             # remove duplicate legend as model colour + shape scale values now match
             
            
             
             plot <-  cv_plot_all_m / dev_explained_plot_all
             ggplot2::ggsave(filename="results/figures/combined_cv_devexp_plot_all.png",
                             plot=plot,
                             width=10, height=7
                             )
             
             plot
           }
)


)

#tar_read(model_cv_1to5_dt) %>% readr::write_csv(here("processed", "model_cv_1to5_dt.csv")) # 15/12/22 2.18PM
