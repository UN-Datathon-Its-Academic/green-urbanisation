install.packages('FRK')
install.packages('TMB', type = 'source')
library(FRK)
library(sp)
library(tidyverse)
install.packages('sf')
library(sf)

# clear workspace
rm(list = ls())

# Read model rds file
mod_list <- readRDS('data/models.rds')

# R function for new predictions
predict_changefactor <- function(model, changefactor = c(1.0,1.0,1.0), newdata = NULL, ...) {
    # changefactor is a vector of length 3, with the first element being the
    # factor by which the total area is changed, the second element being the
    # factor by which the population density is changed, and the third element
    # being the factor by which the population is changed.
    print(paste("changing total_area by",changefactor[1]))
    print(paste("changing population_density by",changefactor[2]))
    print(paste("changing greenness by",changefactor[3]))
    if (is.null(newdata)) {
        newdata <- model@BAUs
        newdata$total_area <- newdata$total_area * changefactor[1]
        newdata$population_density <- newdata$population_density * changefactor[2]
        newdata$greenness <- newdata$greenness * changefactor[3]
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
pred <- predict_changefactor(mod_list[[1]], changefactor = c(1.5,1.0,1.0))
tibble(pred_hdci(mod_list[[1]], pred))
pred <- predict_changefactor(mod_list[[2]], changefactor = c(1.5,1.5,1.0))
tibble(pred_hdci(mod_list[[2]], pred))
pred <- predict_changefactor(mod_list[[3]], changefactor = c(1.5,1.0,1.5))
tibble(pred_hdci(mod_list[[3]], pred))
