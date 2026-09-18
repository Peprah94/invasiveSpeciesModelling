# ==============================================================================
# 4. Summarise occurrence records by species and data source
# ==============================================================================

species_occurrence_summary <- tibble::tibble(
  scntfcN = allSpecies
) %>%
  
  # --------------------------------------------------------------------------
  # eDNA occurrences
  # --------------------------------------------------------------------------
  dplyr::left_join(
    
    dataForModelling %>%
      dplyr::filter(
        scntfcN %in% allSpecies,
        source == "eDNA",
    obsrvt_ %in% c(
      "known_presence",
      "confirmed_absence"
    )
      ) %>%
      dplyr::count(
        scntfcN,
        name = "eDNA"
      ),
    
    by = "scntfcN"
  ) %>%
  
  # --------------------------------------------------------------------------
  # GBIF occurrences
  # --------------------------------------------------------------------------
  dplyr::left_join(
    
    dataForModelling %>%
      dplyr::filter(
        scntfcN %in% allSpecies,
        grepl("GBIF", source),
        obsrvt_ == "known_presence",
        presenc == 1
      ) %>%
      dplyr::count(
        scntfcN,
        name = "GBIF"
      ),
    
    by = "scntfcN"
  ) %>%
  
  # --------------------------------------------------------------------------
  # Replace missing counts with zero and calculate total
  # --------------------------------------------------------------------------
  dplyr::mutate(
    eDNA = tidyr::replace_na(eDNA, 0L),
    GBIF = tidyr::replace_na(GBIF, 0L),
    Total = eDNA + GBIF
  ) %>%
  
  # --------------------------------------------------------------------------
  # Format table
  # --------------------------------------------------------------------------
  dplyr::select(
    Species = scntfcN,
    eDNA,
    GBIF,
    Total
  )


write.csv(species_occurrence_summary,
file = "species_occurrence_summary.csv")
