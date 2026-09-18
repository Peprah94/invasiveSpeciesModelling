predictor_combinations <- unlist(
  lapply(
    seq_along(all_predictors),
    function(k) {
      combn(
        all_predictors,
        k,
        simplify = FALSE
      )
    }
  ),
  recursive = FALSE
)

length(predictor_combinations)


# ==============================================================================
# 22. Variable-selection analysis
# ==============================================================================

variable_results <- vector(
  "list",
  length(predictor_combinations)
)


for (i in seq_along(predictor_combinations)) {

  current_predictors <- predictor_combinations[[i]]

  predictor_label <- paste(
    current_predictors,
    collapse = " + "
  )

  message(
    "\n====================================================\n",
    "Model ",
    i,
    " / ",
    length(predictor_combinations),
    "\nPredictors: ",
    predictor_label,
    "\n===================================================="
  )

  variable_results[[i]] <- fit_esox_model(

    predictor_names = current_predictors,

    prior_settings = selected_prior_settings,

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
# 23. Compare variable combinations
# ==============================================================================

variable_comparison <- dplyr::bind_rows(

  lapply(

    seq_along(variable_results),

    function(i) {

      predictors_i <-
        variable_results[[i]]$predictors


      data.frame(

        model_id = i,

        predictors = if (
          length(predictors_i) == 0
        ) {

          "Intercept only"

        } else {

          paste(
            predictors_i,
            collapse = " + "
          )
        },

        n_predictors = length(predictors_i),

        WAIC = variable_results[[i]]$waic,

        stringsAsFactors = FALSE
      )
    }
  )
) %>%

  dplyr::arrange(WAIC)


best_variable_id <-
  variable_comparison$model_id[1]


best_predictors <-
  variable_results[[best_variable_id]]$predictors


variable_comparison <- variable_comparison %>%

  dplyr::mutate(

    delta_WAIC =
      WAIC - min(WAIC, na.rm = TRUE)

  ) %>%

  dplyr::arrange(WAIC)



write.csv(
  variable_comparison,
  file = file.path(
    pathToResults,
    "variable_selection_WAIC.csv"
  ),
  row.names = FALSE
)
