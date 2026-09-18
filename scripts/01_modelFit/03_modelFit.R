
# ==============================================================================
# 13. Create spatial mesh
# ==============================================================================

# Mesh resolution should reflect the spatial scale at which you expect
# residual spatial autocorrelation.
#
# Because your raster resolution is 250 m, we start with a relatively
# conservative mesh.
#
# max.edge is in metres because the data are in UTM.

# Checks
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

# Force mesh CRS to exactly EPSG:25833
#mesh$crs <- target_crs$wkt

# Assign CRS
#mesh$crs <- st_crs(selected_predictors)$wkt


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
# 14. Build the integrated dataset list
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
# 15. Specify the covariate formula
# ==============================================================================

# Use all 15 predictor layers initially.
#
# PointedSDMs will use the spatial raster layers as covariates.

bias_names <- c("distanceToRoad")

predictor_names <- names(selected_predictors)
predictor_names <- setdiff(predictor_names, bias_names)

predictor_formula <- as.formula(
  paste(
    "~",
    paste(
      predictor_names,
      collapse = " + "
    )
  )
)

predictor_formula



# ==============================================================================
# 16. Define GBIF sampling-bias covariates
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


# bias_formula


# ==============================================================================
# 17. Initialise the PointedSDMs model
# ==============================================================================

esox_model <- startISDM(
  
  datasets,
  
  spatialCovariates = selected_predictors,
  
  Projection = target_crs$wkt,
  
  Mesh = mesh,
  
  Boundary = lake_boundary,
  
  responsePA = "present",
  
  pointsIntercept = TRUE,
  
  #Offset = "lake_area_m2",
  
  pointsSpatial = "shared",
  
  Formulas = list(
    covariateFormula = ~ temperature + HFI + popnProx + popnDown + popnUp,
    biasFormula = bias_formula
  )
)


# Inspect model
esox_model$changeComponents()


# ==============================================================================
# 18. Check PointedSDMs interpretation of the datasets
# ==============================================================================

print(esox_model)


# You should see something similar to:
#
# Summary of presence absence datasets:
#
# eDNA       Present absence
#
#
# Summary of presence only datasets:
#
# GBIF       Present only
#
#
# This is an important check before fitting.
#


# ==============================================================================
# 19. Specify spatial prior
# ==============================================================================

# PC prior:
#
# prior.range = c(r, p)
#
# means:
# P(range < r) = p
#
# prior.sigma = c(s, p)
#
# means:
# P(sigma > s) = p
#
# Here we use:
#
# P(range < 5 km) = 0.5
# P(sigma > 1) = 0.01

esox_model$specifySpatial(
  
  sharedSpatial = TRUE,
  constr = TRUE,
  
  prior.range = c(
    50,
    0.01
  ),
  
  prior.sigma = c(
    1,
    0.01
  )
)


# ==============================================================================
# 20. Priors for intercepts and predictor names
# ==============================================================================

# With only ~13 GBIF presences and ~10 eDNA observations,
# weakly informative intercept priors can help numerical stability.

esox_model$priorsFixed(
  
  Effect = "Intercept",
  
  mean.linear = 0,
  
  prec.linear = 1
)

# Weakly informative priors for all predictor effects
esox_model$priorsFixed(
  Effect = 'temperature',
  mean.linear = -5,
  prec.linear = 10
)

# Weakly informative priors for all predictor effects
esox_model$priorsFixed(
  Effect = 'HFI',
  mean.linear = 0,
  prec.linear = 1
)

esox_model$priorsFixed(
  Effect = 'popnProx',
  mean.linear = 0,
  prec.linear = 1
)

esox_model$priorsFixed(
  Effect = 'popnDown',
  mean.linear = 0,
  prec.linear = 1
)

esox_model$priorsFixed(
  Effect = 'popnUp',
  mean.linear = 0,
  prec.linear = 1
)


#for (pred in predictor_names) {
#  
#  esox_model$priorsFixed(
#    Effect = pred,
#    mean.linear = 0,
#    prec.linear = 1
#  )
#}


# ==============================================================================
# 21. Plot the model data
# ==============================================================================

model_plot <- esox_model$plot(
  Boundary = TRUE
)

model_plot


# ==============================================================================
# 22. Fit the model
# ==============================================================================

# Start with conservative INLA settings.
#
# Do not immediately use an expensive integration strategy.

esox_fit <- fitISDM(
  
  esox_model,
  
  options = list(
    
    control.inla = list(
      int.strategy = "eb"
    ),
    
    control.compute = list(
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE,
      return.marginals.predictor = TRUE
    ),
    
    verbose = TRUE
    
  )
)


# ==============================================================================
# 23. Model summary
# ==============================================================================

summary(esox_fit)




# ==============================================================================
# 27. Save model
# ==============================================================================

saveRDS(
  esox_fit,
  file = file.path(pathToResults, "modelFit.rds")
)

saveRDS(
  esox_model,
  file = file.path(pathToResults, "modelDescription.rds")
)
