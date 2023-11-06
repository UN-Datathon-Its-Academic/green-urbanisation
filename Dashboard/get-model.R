library(FRK)
library(sp)
library(tidyverse)
library(sf)

# Read model rds file
mod_list <- readRDS('models.rds')
names(mod_list) <- sapply(mod_list, \(x) unique(x@BAUs$City))

# R function for new predictions
predict_temp <-
  function(model,
           mult_total_area = 1,
           mult_population_density = 1,
           mult_greenness = 1,
           newdata = NULL,
           ...) {
    if (is.null(newdata)) {
      newdata <- model@BAUs
      newdata$total_area <- newdata$total_area * mult_total_area
      newdata$population_density <-
        newdata$population_density * mult_population_density
      newdata$greenness <- newdata$greenness * mult_greenness
    }
    pred <- predict(model, type = c("mean"), newdata)
    
    return(pred)
  }

pred_hdci <- function(model, pred) {
  # Extracting posterior distributions of predictive locations
  post_dist_df <- as.data.frame(pred$MC$mu_samples) %>%
    mutate(system.index = model@BAUs@data$system.index) %>%
    tidyr::pivot_longer(!system.index,
                        names_to = "draw",
                        values_to = "pred"
    )
  # Summary predictions at grid_index
  pred_sum_sf <- post_dist_df %>% group_by(system.index) %>%
    ggdist::median_hdci(pred)
  return(pred_sum_sf)
}

# test predictions
# pred <- predict_temp(mod_list[[1]], changefactor = c(1.5,1.0,1.0))
# tibble(pred_hdci(mod_list[[1]], pred))
# pred <- predict_temp(mod_list[[2]], changefactor = c(1.5,1.5,1.0))
# tibble(pred_hdci(mod_list[[2]], pred))
# pred <- predict_temp(mod_list[[3]], changefactor = c(1.5,1.0,1.5))
# tibble(pred_hdci(mod_list[[3]], pred))
