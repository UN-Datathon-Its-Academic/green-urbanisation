# FRK model 

rm(list=ls())
source("R/packages.R")

#Import all data from the Data folder 
datalist <- list.files(path = "Data/", pattern='_merged.csv$', all.files= T, full.names= T)
mod_list <- list()

for ( i in 1:length(datalist)){
  
 X <- read.csv(datalist[i]) %>%
    mutate(City = str_match(datalist[i], "/\\s*(.*?)\\s*_")[2])
  
  ## Convert to sf
  grid_pred <- st_as_sfc(X$geometry)
  st_geometry(X) <- grid_pred
  
  # Making STObj - response variable 
  X_Obj <- X %>% dplyr::select(temperature_anomaly) #%>%
  # sample_frac(0.8)
  
  # Making BAUs - other covariates
  ST_BAUs <- as_Spatial(X %>% dplyr::select(!temperature_anomaly)%>% 
                          mutate(dplyr::across(c(building_count:greenness), ~ as.numeric(scale(.)))))
  
  ST_BAUs$fs <- 1                   # scalar fine-scale variance matrix

  # Create basis functions
  basis <- auto_basis(manifold = plane(),      # functions on sphere
                      data=ST_BAUs, 
                      nres = 3,                # use three DGGRID resolutions
                      prune= 0                 # prune basis functions
  )  
  
  show_basis(basis)
  
  ### Model 
  mod <- FRK(f = temperature_anomaly ~ 1 + greenness + population_density + total_area, 
             data = list(as_Spatial(X_Obj)), 
             BAUs = ST_BAUs, 
             basis = basis, 
             response = "gaussian", 
             K_type = "precision", 
             method = "TMB", 
             est_error = TRUE)

  pred <- predict(mod, type = c("mean"))

  # Extracting posterior distributions of predictive locations 
  
  post_dist_df <- as.data.frame(pred$MC$mu_samples) %>% 
    mutate(system.index = ST_BAUs@data$system.index) %>%
    tidyr::pivot_longer(!system.index,
                        names_to = "draw", 
                        values_to = "pred"
    )
  
  # Summary predictions at grid_index 
  
  pred_sum_sf <- post_dist_df %>% group_by(system.index) %>% 
    ggdist::median_hdci(pred)%>%
    inner_join(X %>% dplyr::select(geometry,system.index)) %>% 
    st_as_sf(sf_column_name = "geometry") %>%
    mutate(Unc = .upper - .lower) %>%
    mutate(City = str_match(datalist[i], "/\\s*(.*?)\\s*_")[2])
  
  mod_list[[i]] <- mod
}

saveRDS(mod_list, file = "Data/models.rds")
