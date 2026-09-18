# ==============================================================================
# 1. Check input data
# ==============================================================================

# Species data
dataForModelling <- sf::st_read(file.path(dataFolder, 
                                          "presence_absence.shp")) 

dataForSpp <- dataForModelling %>%
  dplyr::filter(scntfcN %in% sppOfInt) %>%
  sf::st_transform(target_crs)

# ----------------------------
# Create lake area
# ------------------------------

library(sf)
library(dplyr)
library(units)

lakes <- dataForModelling %>%
  dplyr::filter(scntfcN %in% sppOfInt) %>%
  dplyr::select(lake_id, geometry) %>%
  dplyr::distinct(lake_id, .keep_all = TRUE)

# Calculate area in m2
lakes <- lakes %>%
  mutate(
    lake_area_m2 = as.numeric(st_area(geometry))
  )

summary(lakes$lake_area_m2)


# ==============================================================================
# 2. Create GBIF presence-only dataset
# ==============================================================================

# Any record containing GBIF is considered a GBIF occurrence.
#
# This includes records labelled:
#   "GBIF"
#   "eDNA; GBIF"
#
# Only confirmed/known presences are retained.

dataForSpp_gbif <- dataForSpp %>%
  filter(
    grepl("GBIF", source),
    obsrvt_ == "known_presence",
    presenc == 1
  ) %>%
  dplyr::left_join(
    lakes %>%
      st_drop_geometry() %>%
      dplyr::select(
        lake_id,
        lake_area_m2
      ),
    by = "lake_id"
  ) %>%
  dplyr::select(scntfcN, lake_id, lake_area_m2) %>%
  sf::st_point_on_surface()


# Give the dataset a simple identifier
dataForSpp_gbif$dataset <- "GBIF"



# ==============================================================================
# 3. Create eDNA presence-absence dataset
# ==============================================================================

# IMPORTANT:
# Only records explicitly generated from eDNA are included here.
#
# "eDNA; GBIF" records are NOT included in the eDNA dataset.
# They are being treated as GBIF observations above.
#
# If the same biological observation is genuinely represented independently
# in both datasets, this decision can be changed later.

dataForSpp_edna <- dataForSpp %>%
  filter(
    source == "eDNA",
    obsrvt_ %in% c(
      "known_presence",
      "confirmed_absence"
    )
  ) %>%
  mutate(
    present = as.integer(presenc)
  ) %>%
  dplyr::left_join(
    lakes %>%
      st_drop_geometry() %>%
      dplyr::select(
        lake_id,
        lake_area_m2
      ),
    by = "lake_id"
  )   %>%
  dplyr::select(scntfcN, lake_id, present, lake_area_m2) %>%
  st_point_on_surface()


dataForSpp_edna$dataset <- "eDNA"



# ==============================================================================
# 7. Create the lake boundary / prediction domain
# ==============================================================================

# Your full dataForModelling contains the background lakes.
#
# We do NOT treat these background lakes as absences.
#
# Instead, they define the set of lakes over which the model is intended
# to operate.

lake_boundary <- dataForModelling %>%
  filter(scntfcN %in% sppOfInt) %>%
  st_geometry() %>%
  st_union() %>%
  st_as_sf() %>%
  sf::st_transform(target_crs)


# Transform boundary if necessary
# lake_boundary <- st_transform(
#   lake_boundary,
#   crs = predictor_crs
# )


# Check
plot(st_geometry(lake_boundary))
plot(st_geometry(dataForSpp_gbif),
     add = TRUE,
     col = "red",
     pch = 16)

plot(st_geometry(dataForSpp_edna),
     add = TRUE,
     col = "blue",
     pch = 17)
