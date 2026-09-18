prior_results <- vector(
  "list",
  length(prior_scenarios)
)

names(prior_results) <- names(prior_scenarios)


for (i in seq_along(prior_scenarios)) {

  scenario_name <- names(prior_scenarios)[i]

  message(
    "\n====================================================\n",
    "Fitting prior scenario: ",
    scenario_name,
    "\n===================================================="
  )

  prior_results[[i]] <- fit_esox_model(

    predictor_names = all_predictors,

    prior_settings = prior_scenarios[[i]],

    spatial_prior = spatial_prior,

    datasets = datasets,

    selected_predictors = selected_predictors,

    mesh = mesh,

    lake_boundary = lake_boundary,

    target_crs = target_crs,

    bias_formula = bias_formula
  )
}


# ==============================================================================
# 20. Compare prior scenarios
# ==============================================================================

prior_comparison <- dplyr::bind_rows(

  lapply(
    names(prior_results),

    function(x) {

      data.frame(

        prior = x,

        WAIC = prior_results[[x]]$waic,

        predictors = paste(
          prior_results[[x]]$predictors,
          collapse = " + "
        ),

        stringsAsFactors = FALSE
      )
    }
  )
) %>%

  dplyr::arrange(WAIC)


prior_comparison


# Select best prior names
best_prior_name <- prior_comparison$prior[1]

best_prior_name

best_prior <- prior_results[[best_prior_name]]

best_prior$waic

write.csv(
  prior_comparison,
  file = file.path(
    pathToResults,
    "prior_sensitivity_WAIC.csv"
  ),
  row.names = FALSE
)