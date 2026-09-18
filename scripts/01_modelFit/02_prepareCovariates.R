# Predictor raster


if(!file.exists(file.path(dataFolder, 
                          "predictor_stack_scaled.tif"))){
  # ==============================================================================
  # Load predictors
  # ==============================================================================
  
  predictors <- terra::rast(
    file.path(dataFolder, "predictor_stack.tif")
  )
  
  
  # ==============================================================================
  # Identify binary and continuous predictors
  # ==============================================================================
  
  binary_predictors <- grep(
    "(down|up)$",
    names(predictors),
    value = TRUE
  )
  
  continuous_predictors <- setdiff(
    names(predictors),
    binary_predictors
  )
  
  
  # Check classification
  data.frame(
    predictor = names(predictors),
    type = ifelse(
      names(predictors) %in% binary_predictors,
      "binary",
      "continuous"
    )
  )
  
  # ==============================================================================
  # Calculate scaling parameters for continuous predictors
  # ==============================================================================
  
  predictor_means <- terra::global(
    predictors[[continuous_predictors]],
    fun = "mean",
    na.rm = TRUE
  )[, 1]
  
  predictor_sds <- terra::global(
    predictors[[continuous_predictors]],
    fun = "sd",
    na.rm = TRUE
  )[, 1]
  
  
  # Check for zero variance
  if (any(
    is.na(predictor_sds) |
    predictor_sds == 0
  )) {
    
    stop(
      "One or more continuous predictors have zero or undefined variance."
    )
  }
  
  
  # ==============================================================================
  # Scale continuous predictors
  # ==============================================================================
  
  predictors_scaled <- predictors
  
  for (i in seq_along(continuous_predictors)) {
    
    lyr <- continuous_predictors[i]
    
    message("Scaling: ", lyr)
    
    predictors_scaled[[lyr]] <- (
      predictors[[lyr]] - predictor_means[i]
    ) / predictor_sds[i]
  }
  
  terra::crs(predictors_scaled) <- "EPSG:25833"
  
  
  # ==============================================================================
  # Write scaled predictor stack
  # ==============================================================================
  
  scaled_predictor_file <- file.path(
    dataFolder,
    "predictor_stack_scaled.tif"
  )
  
  terra::writeRaster(
    predictors_scaled,
    scaled_predictor_file,
    overwrite = TRUE
  )
  
  
  # ==============================================================================
  # Save scaling parameters
  # ==============================================================================
  
  scaling_parameters <- data.frame(
    predictor = continuous_predictors,
    mean = predictor_means,
    sd = predictor_sds
  )
  
  write.csv(
    scaling_parameters,
    file.path(
      dataFolder,
      "predictor_scaling_parameters.csv"
    ),
    row.names = FALSE
  )
  
}


predictors <- terra::rast(file.path(dataFolder, 
                                    "predictor_stack_scaled.tif")) 
  


# Select predictors for the corresponding species of Interest
spp_prefix <- tolower(gsub(" ", "_", sppOfInt))

selected_predictors <- predictors[[
  c(
    1:3,
    grep(
      paste0("^", spp_prefix, "_"),
      names(predictors)
    )
  )
]]

names(selected_predictors) <- c("temperature",
                                "HFI",
                                "distanceToRoad",
                                "popnDown",
                                "popnProx",
                                "popnUp")
