fit_esox_model <- function(
    predictor_names,
    prior_settings,
    spatial_prior = list(
      prior.range = c(50, 0.01),
      prior.sigma = c(1, 0.01)
    ),
    datasets,
    selected_predictors,
    mesh,
    lake_boundary,
    target_crs,
    bias_formula
) {

  # --------------------------------------------------------------------------
  # Construct predictor formula
  # --------------------------------------------------------------------------

  if (length(predictor_names) == 0) {

    predictor_formula <- ~ 1

  } else {

    predictor_formula <- as.formula(
      paste(
        "~",
        paste(
          predictor_names,
          collapse = " + "
        )
      )
    )
  }


  # --------------------------------------------------------------------------
  # Initialise model
  # --------------------------------------------------------------------------

  model <- startISDM(

    datasets,

    spatialCovariates = selected_predictors,

    Projection = target_crs$wkt,

    Mesh = mesh,

    Boundary = lake_boundary,

    responsePA = "present",

    pointsIntercept = TRUE,

    pointsSpatial = "shared",

    Formulas = list(
      covariateFormula = predictor_formula,
      biasFormula = bias_formula
    )
  )


  # --------------------------------------------------------------------------
  # Spatial prior
  # --------------------------------------------------------------------------

  model$specifySpatial(

    sharedSpatial = TRUE,

    constr = TRUE,

    prior.range = spatial_prior$prior.range,

    prior.sigma = spatial_prior$prior.sigma
  )


  # --------------------------------------------------------------------------
  # Intercept prior
  # --------------------------------------------------------------------------

  model$priorsFixed(

    Effect = "Intercept",

    mean.linear = prior_settings$Intercept$mean,

    prec.linear = prior_settings$Intercept$prec
  )


  # --------------------------------------------------------------------------
  # Predictor priors
  # --------------------------------------------------------------------------

  if (length(predictor_names) > 0) {

    for (pred in predictor_names) {

      # Only apply a prior if one has been specified
      if (pred %in% names(prior_settings)) {

        model$priorsFixed(

          Effect = pred,

          mean.linear = prior_settings[[pred]]$mean,

          prec.linear = prior_settings[[pred]]$prec
        )
      }
    }
  }


  # --------------------------------------------------------------------------
  # Fit model
  # --------------------------------------------------------------------------

  fit <- fitISDM(

    model,

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

      verbose = FALSE
    )
  )


  # --------------------------------------------------------------------------
  # Extract WAIC
  # --------------------------------------------------------------------------

  waic_value <- NA_real_

  if (!is.null(fit$waic$waic)) {
    waic_value <- fit$waic$waic
  }


  list(
    model = model,
    fit = fit,
    waic = waic_value,
    predictors = predictor_names,
    priors = prior_settings
  )
}