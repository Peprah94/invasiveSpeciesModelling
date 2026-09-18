# ==============================================================================
# 10. DEFINE BIOGEOGRAPHIC CLASSIFICATION REGIONS
# ==============================================================================

## ------------------------------------------------------------------
## Path to Norwegian county geodatabase
## ------------------------------------------------------------------

gdb <- paste0(
  "R:/GeoSpatialData/AdministrativeUnits/",
  "Norway_AdministrativeUnits/Original/Norway_County/",
  "versjon2025/Administrative enheter fylker FGDB-format/",
  "Basisdata_0000_Norge_25833_Fylker_FGDB/",
  "Basisdata_0000_Norge_25833_Fylker_FGDB.gdb"
)


## ------------------------------------------------------------------
## Read county polygons
## ------------------------------------------------------------------

counties <- sf::st_read(
  gdb,
  layer = "fylke"
)

study_region <- counties %>%
  dplyr::filter(fylkesnavn %in% "Rogaland") %>%
  st_transform(., target_crs) %>%
  st_geometry()


## ------------------------------------------------------------------
## Assign counties to nature regions
## ------------------------------------------------------------------
