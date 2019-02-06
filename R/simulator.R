library(rstan)

functions_to_expose <-
  '
functions {
int ordered_logit_rng(real eta, vector c) {
  return ordered_logistic_rng(eta, c);
}
}
'
functions_to_expose <- stanc(model_code = functions_to_expose, model_name = "functions_to_expose")
# NOT RUN {
expose_stan_functions(functions_to_expose)

# }


simulate_data <- function(N, ncat) {
  intercept_sigma = 1

  b <- rnorm(1, 0, 1)
  intercept <- sort(rt(ncat - 1, 3) * intercept_sigma)

  X <- rnorm(N, 0, 1)

  mu = X * b;



  data = list(
    observed = list(
      N = N,
      ncat = ncat,
      X = X,
      Y = Y,
      intercept_sigma = 1
    ),
    true = list(
      b = b,
      intercept = intercept
    )
  )
}
