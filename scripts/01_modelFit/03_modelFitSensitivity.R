# ==============================================================================
# 1. Function to build and fit one PointedSDM model
# ==============================================================================

source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "00_fitModelFunction.R"))


# ==============================================================================
# 2. Define prior sensitivity scenarios
# ==============================================================================
source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "003_priorSensitivityVars.R"))


# ==============================================================================
# 3. Check CRS match for all datasets
# ==============================================================================
all_predictors <- c(
  "temperature",
  "HFI",
  "popnProx",
  "popnDown",
  "popnUp"
)

# Check of the crs for all the data components match
st_crs(dataForModelling) == target_crs
st_crs(dataForSpp_gbif) == target_crs
st_crs(dataForSpp_edna) == target_crs
st_crs(lake_boundary) == target_crs
st_crs(study_region) == target_crs
terra::crs(selected_predictors) == target_crs$wkt

occ_points <- c(
  st_geometry(dataForSpp_gbif),
  st_geometry(dataForSpp_edna)
)

# ==============================================================================
# 4. Create mesh
# ==============================================================================
# Make sure you have one point for each lake
mesh_pts <- dataForSpp %>%
  sf::st_point_on_surface() %>%
  dplyr::select(lake_id, geometry) %>%
  dplyr::distinct(lake_id, .keep_all = TRUE)

mesh <- fm_mesh_2d(
  loc = st_geometry(mesh_pts),
  loc.domain = lake_boundary,
  max.edge = c(10000, 15000),
  cutoff = 1000,
  crs = target_crs
)

# Inspect mesh
plot(mesh)
plot(
  st_geometry(lake_boundary),
  add = TRUE,
  border = "red"
)
points(
  st_coordinates(mesh_pts),
  pch = 16,
  cex = 0.5
)


# ==============================================================================
# 5. Build the integrated dataset list
# ==============================================================================

datasets <- list(
  GBIF = dataForSpp_gbif,
  eDNA = dataForSpp_edna
)


# Check
lapply(
  datasets,
  nrow
)

# ==============================================================================
# 6. Define GBIF sampling-bias covariates
# ==============================================================================

# IMPORTANT:
#
# The distribution/environmental process and the GBIF observation process
# are different.
#
# We therefore allow the GBIF observation process to have its own
# sampling-bias component.
#
# A reasonable first candidate from your predictors is:
#
#   human_access_index
#   distance_to_road
#
# because GBIF observations are likely to be spatially biased toward
# accessible/human-influenced locations.
#
# Replace these names below with the exact names returned by:
#
#     names(predictors)

#names(predictors)


# Example:
#

# If no appropriate bias variables are found:
bias_names <- c("distanceToRoad")
if (length(bias_names) == 0) {
  
  message(
    "No human/road predictor automatically identified. ",
    "Using no explicit GBIF bias covariate."
  )
  
  bias_formula <- NULL
  
} else {
  
  bias_formula <- as.formula(
    paste(
      "~",
      paste(
        bias_names,
        collapse = " + "
      )
    )
  )
  
}

# ==============================================================================
# 7. Run sensitivity analysis for the prior specification
# ==============================================================================
source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "003_runPriorSensitivity.R"))


# Select the prior from the model with the lowest WAIC
selected_prior_settings <-
  prior_scenarios[[best_prior_name]]

# ==============================================================================
# 8. Run variable selection
# ==============================================================================
source(file.path(here::here(), 
                 "scripts",
                 "01_modelFit", 
                 "003_variableSelection.R"))


# ==============================================================================
# 9. Select final model
# ==============================================================================

best_model_result <-
  variable_results[[best_variable_id]]


esox_model <-
  best_model_result$model


esox_fit <-
  best_model_result$fit


summary(esox_fit)


# ==============================================================================
# 10. Save final model
# ==============================================================================

saveRDS(
  esox_fit,
  file = file.path(
    pathToResults,
    "modelFit_final.rds"
  )
)

saveRDS(
  esox_model,
  file = file.path(
    pathToResults,
    "modelDescription_final.rds"
  )
)


final_model_info <- list(

  selected_prior = best_prior_name,

  selected_predictors = best_predictors,

  WAIC = best_model_result$waic,

  prior_comparison = prior_comparison,

  variable_comparison = variable_comparison
)


saveRDS(
  final_model_info,
  file = file.path(
    pathToResults,
    "final_model_selection.rds"
  )
)