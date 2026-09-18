################################################################################
# Convert PointedSDMs linear predictor to probability and plot as raster
################################################################################
# ==============================================================================
# One prediction point per lake
# ==============================================================================

lake_prediction_points <- dataForModelling %>%
  dplyr::filter(scntfcN %in% sppOfInt) %>%
  dplyr::select(lake_id, geometry) %>%
  dplyr::distinct(lake_id, .keep_all = TRUE) %>%
  st_point_on_surface()

esox_prediction <- predict(
  esox_fit,
  data = lake_prediction_points,
  predictor = TRUE,
  fun = "linear"
)

# ==============================================================================
# 1. Extract predictions
# ==============================================================================

predictions <- esox_prediction$predictions

predictions <- sf::st_as_sf(predictions)

# Check
names(predictions)
nrow(predictions)
st_crs(predictions)


# ==============================================================================
# 2. Convert linear predictor to occurrence probability
# ==============================================================================

# PointedSDMs model uses a complementary log-log link.
#
# Inverse cloglog:
#
#   p = 1 - exp(-exp(eta))
#
# where eta is the linear predictor.

predictions <- predictions %>%
  mutate(
    probability = 1 - exp(-exp(mean))
  )


# Check probability
summary(predictions$probability)

range(
  predictions$probability,
  na.rm = TRUE
)


# ==============================================================================
# 3. Create one polygon per lake
# ==============================================================================

lakes <- dataForModelling %>%
  dplyr::filter(
    scntfcN %in% sppOfInt
  ) %>%
  dplyr::select(
    lake_id,
    geometry
  ) %>%
  dplyr::distinct(
    lake_id,
    .keep_all = TRUE
  )


# ==============================================================================
# 4. Make sure CRS is identical
# ==============================================================================

lakes <- st_transform(
  lakes,
  st_crs(predictions)
)


# ==============================================================================
# 5. Join predictions to lake polygons
# ==============================================================================

lakes_pred <- lakes %>%
  left_join(
    predictions %>%
      st_drop_geometry() %>%
      dplyr::select(
        lake_id,
        mean,
        sd,
        q0.025,
        q0.5,
        q0.975,
        probability
      ),
    by = "lake_id"
  )


# ==============================================================================
# 6. Check prediction coverage
# ==============================================================================

prediction_coverage <- lakes_pred %>%
  sf::st_drop_geometry() %>%
  summarise(
    total_lakes = n(),
    predicted_lakes = sum(!is.na(probability)),
    missing_lakes = sum(is.na(probability)),
    proportion_predicted =
      mean(!is.na(probability))
  )

print(prediction_coverage)


# ==============================================================================
# 7. Create raster template
# ==============================================================================

prediction_template <- selected_predictors[[1]]

# Ensure template has the same CRS
# terra::crs(prediction_template)


# ==============================================================================
# 8. Convert lake polygons to SpatVector
# ==============================================================================

lakes_vect <- terra::vect(lakes_pred)


# ==============================================================================
# 9. Rasterize occurrence probability
# ==============================================================================

probability_raster <- terra::rasterize(
  lakes_vect,
  prediction_template,
  field = "probability",
  fun = "mean",
  background = NA
)

# names(probability_raster) <- "Esox_lucius_probability"


# ==============================================================================
# 10. Save probability raster
# ==============================================================================

probability_file <- file.path(
  pathToResults,
  "occurrence_probability.tif"
)

terra::writeRaster(
  probability_raster,
  probability_file,
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW")
  )
)

cat(
  "\nProbability raster saved to:\n",
  probability_file,
  "\n"
)


# ==============================================================================
# 11. Rasterize linear predictor as well
# ==============================================================================

linear_raster <- terra::rasterize(
  lakes_vect,
  prediction_template,
  field = "mean",
  fun = "mean",
  background = NA
)

names(linear_raster) <- "linear_predictor"


# ==============================================================================
# 12. Save linear predictor raster
# ==============================================================================

linear_file <- file.path(
  pathToResults,
  "linear_predictor.tif"
)

terra::writeRaster(
  linear_raster,
  linear_file,
  overwrite = TRUE,
  wopt = list(
    datatype = "FLT4S",
    gdal = c("COMPRESS=LZW")
  )
)

cat(
  "\nLinear predictor raster saved to:\n",
  linear_file,
  "\n"
)

# ==============================================================================
# 11. Plot raster
# ==============================================================================
p <- ggplot() +
  
  tidyterra::geom_spatraster(
    data = probability_raster
  ) +
  
  scale_fill_viridis_c(
    name = "Occurrence\nprobability",
    limits = c(0, 1),
    na.value = "transparent"
  ) +
  
  # Rogaland boundary
  # geom_sf(
  #   data = study_region,
  #   fill = NA,
  #   colour = "black",
  #   linewidth = 0.6
  # ) +
  
  # Individual lake boundaries
  # geom_sf(
  #   data = lakes,
  #   fill = NA,
  #   colour = "grey60",
  #   linewidth = 0.15
  # ) +
  
  coord_sf(
    crs = st_crs(25833),
    expand = FALSE
  ) +
  
  labs(
    title = paste(sppOfInt,"predicted occurrence probability"),
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  
  theme(
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.2
    ),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(
      face = "bold",
      size = 14
    )
  )


ggsave(plot = p,
       filename = file.path(pathToResults,
                            "occ_plot.pdf"),
       units = "in",
       height = 7,
       width = 8)

ggsave(plot = p,
       filename = file.path(pathToResults,
                            "occ_plot.png"),
       units = "in",
       height = 7,
       width = 8)

# ==============================================================================
# 13. Inspect probability raster
# ==============================================================================

# probability_raster
# 
# global(
#   probability_raster,
#   c("min", "max", "mean"),
#   na.rm = TRUE
# )
# 
# 
# # ==============================================================================
# # 14. Quick plot
# # ==============================================================================
# 
# plot(
#   probability_raster,
#   main = "Esox lucius occurrence probability"
# )
# 
# plot(
#   terra::vect(lakes),
#   add = TRUE,
#   border = "grey30",
#   lwd = 0.2
# )
