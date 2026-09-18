# ---------------------------------
# Set prior sensitivity variables
# -----------------------------------


prior_scenarios <- list(

  # --------------------------------------------------------------------------
  # Prior 1: relatively weak / diffuse
  # --------------------------------------------------------------------------

  weak = list(

    Intercept = list(
      mean = 0,
      prec = 0.1
    ),

    temperature = list(
      mean = 0,
      prec = 0.1
    ),

    HFI = list(
      mean = 0,
      prec = 0.1
    ),

    popnProx = list(
      mean = 0,
      prec = 0.1
    ),

    popnDown = list(
      mean = 0,
      prec = 0.1
    ),

    popnUp = list(
      mean = 0,
      prec = 0.1
    )
  ),


  # --------------------------------------------------------------------------
  # Prior 2: your current priors
  # --------------------------------------------------------------------------

  current = list(

    Intercept = list(
      mean = 0,
      prec = 1
    ),

    temperature = list(
      mean = 0,
      prec = 1
    ),

    HFI = list(
      mean = 0,
      prec = 1
    ),

    popnProx = list(
      mean = 0,
      prec = 1
    ),

    popnDown = list(
      mean = 0,
      prec = 1
    ),

    popnUp = list(
      mean = 0,
      prec = 1
    )
  ),


  # --------------------------------------------------------------------------
  # Prior 3: moderately informative
  # --------------------------------------------------------------------------

  moderate = list(

    Intercept = list(
      mean = 0,
      prec = 10
    ),

    temperature = list(
      mean = 0,
      prec = 10
    ),

    HFI = list(
      mean = 0,
      prec = 10
    ),

    popnProx = list(
      mean = 0,
      prec = 10
    ),

    popnDown = list(
      mean = 0,
      prec = 10
    ),

    popnUp = list(
      mean = 0,
      prec = 10
    )
  ),


  # --------------------------------------------------------------------------
  # Prior 4: stronger shrinkage
  # --------------------------------------------------------------------------

  strong = list(

    Intercept = list(
      mean = 0,
      prec = 0.001
    ),

    temperature = list(
      mean = 0,
     prec = 0.001
    ),

    HFI = list(
      mean = 0,
     prec = 0.001
    ),

    popnProx = list(
      mean = 0,
     prec = 0.001
    ),

    popnDown = list(
      mean = 0,
     prec = 0.001
    ),

    popnUp = list(
      mean = 0,
     prec = 0.001
    )
  )
)


spatial_prior <- list(

  prior.range = c(
    50,
    0.01
  ),

  prior.sigma = c(
    1,
    0.01
  )
)