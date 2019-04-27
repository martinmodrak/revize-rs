library(rstan)
library(mvtnorm)

functions_to_expose <- stanc(file = here("stan","ordered_logit_rng.stan"), model_name = "functions_to_expose")
expose_stan_functions(functions_to_expose)

# Taken from https://github.com/rmcelreath/rethinking/blob/master/R/distributions.r
# Ben's rLKJ function
# K is dimension of matrix
rlkjcorr <- function ( n , K , eta = 1 ) {

  stopifnot(is.numeric(K), K >= 2, K == as.integer(K))
  stopifnot(eta > 0)
  #if (K == 1) return(matrix(1, 1, 1))

  f <- function() {
    alpha <- eta + (K - 2)/2
    r12 <- 2 * rbeta(1, alpha, alpha) - 1
    R <- matrix(0, K, K) # upper triangular Cholesky factor until return()
    R[1,1] <- 1
    R[1,2] <- r12
    R[2,2] <- sqrt(1 - r12^2)
    if(K > 2) for (m in 2:(K - 1)) {
      alpha <- alpha - 0.5
      y <- rbeta(1, m / 2, alpha)

      # Draw uniformally on a hypersphere
      z <- rnorm(m, 0, 1)
      z <- z / sqrt(crossprod(z)[1])

      R[1:m,m+1] <- sqrt(y) * z
      R[m+1,m+1] <- sqrt(1 - y)
    }
    return(crossprod(R))
  }
  R <- replicate( n , f() )
  if ( dim(R)[3]==1 ) {
    R <- R[,,1]
  } else {
    # need to move 3rd dimension to front, so conforms to array structure that Stan uses
    R <- aperm(R,c(3,1,2))
  }
  return(R)
}


simulate_data <- function(N, ncat, N_questions, N_fixed, N_random_groups, N_monotonic, N_monotonic_cat = 4)  {
  intercept_sigma <- 1
  intercept <- sort(rt(ncat - 1, 3) * intercept_sigma)

  questions <- rep(1:N_questions, each = N)

  corr_matrix <- rlkjcorr(1, N_questions, eta = 1)
  corr_matrix_chol <- t(chol(corr_matrix)) #Transpose to lower triangular to match Stan
  #Only the upper triangular to check with Stan
  q_corr_vec <- array(NA_real_, (N_questions * (N_questions-1)) / 2)
  i <- 1
  for(n1 in 1:(N_questions - 1)) {
    for(n2 in (n1 +1) : N_questions) {
      q_corr_vec[i] <- corr_matrix[n1,n2]
      i <- i + 1
    }
  }

  #"Fixed" effects correlate across questions and there is an sd of how much the effect differs between questions
  b_raw <- matrix(rnorm(N_fixed * N_questions, 0, 1), nrow = N_questions, ncol = N_fixed)
  b_sd <- abs(rnorm(N_fixed, 0, 1))
  b <- t(rmvnorm(N_fixed, sigma = corr_matrix)) %*% diag(b_sd)


  X_base <- matrix(rnorm(N * N_fixed, 0, 1), nrow = N, ncol = N_fixed)
  X <- rep(X_base, N_questions) %>% matrix(nrow = N*N_questions, ncol = N_fixed, byrow = TRUE)

  #Ensure full range of monotonic effects
  if(N_monotonic > 0) {
    X_monotonic_base <- array(sample(1:N_monotonic_cat, N * N_monotonic, replace = TRUE), c(N, N_monotonic))
    for(n in 1:N_monotonic) {
      X_monotonic_base[1:N_monotonic_cat, n] <- sample(1:N_monotonic_cat, size = N_monotonic_cat)
    }
    X_monotonic <- rep(X_monotonic_base, N_questions) %>% matrix(nrow = N*N_questions, ncol = N_monotonic, byrow = TRUE)
  } else {
    X_monotonic <- matrix(0, nrow = N* N_questions, ncol = 0)
  }


  b_monotonic <- rnorm(N_monotonic, 0, 1)
  zeta_monotonic <- MCMCpack::rdirichlet(N_monotonic, rep(1, length.out = N_monotonic_cat - 1))
  zeta_monotonic_trans <- cbind(rep(0, N_monotonic), zeta_monotonic)

  N_random <- length(N_random_groups)
  random_hyper_sigma <- rep(1, N_random)
  sigma_random <- abs(rnorm(N_random, 0, random_hyper_sigma))

  b_random_group_start <- cumsum(N_random_groups) - N_random_groups + 1
  b_random_to_group <- integer(sum(N_random_groups))
  for(i in 1:N_random) {
    for(k in 1:N_random_groups[i]) {
      b_random_to_group[b_random_group_start[i] + k - 1] <- i
    }
  }

  b_random <- rnorm(sum(N_random_groups), 0, sigma_random[b_random_to_group])

  X_random_groups_base <- array(NA_real_, c(N, N_random))
  for(i in 1:N_random) {
    X_random_groups_base[,i] <- sample(1:N_random_groups[i], N, replace = TRUE)
  }
  #Ensure full range of random groups
  for(n in 1:N_random) {
    X_random_groups_base[1:(N_random_groups[i]), n] <- sample(1:(N_random_groups[i]), size = N_random_groups[i])
  }


  X_random_groups <- rep(X_random_groups_base, N_questions) %>% matrix(nrow = N*N_questions, ncol = N_random, byrow = TRUE)

  X_random_base <- array(rbinom(N_random * N, 1, prob = 0.3), c(N, N_random))
  X_random <- rep(X_random_base, N_questions) %>% matrix(nrow = N*N_questions, ncol = N_random, byrow = TRUE)

  b_random_index <- array(-1, c(N * N_questions, N_random))
  for(n in 1:(N * N_questions)) {
    b_random_index[n, ] = b_random_group_start + X_random_groups[n, ] - 1;
  }

  mu <- array(NA_real_, N * N_questions)
  for(n in 1:(N * N_questions)) {
    mu[n] <- X[n,] %*% b[questions[n],]
    if(N_monotonic > 0) {
      for(N_m in 1:N_monotonic) {
        if(X_monotonic[n, N_m] > 1) {
          mu[n] <- mu[n] + b_monotonic[N_m] * sum(zeta_monotonic_trans[N_m, 1:X_monotonic[n,N_m]])
        }
      }
    }
    mu[n] <- mu[n] + sum(X_random[n, ] * b_random[b_random_index[n,]])
  }
  Y <- purrr::map_dbl(mu, ordered_logit_rng, intercept)


  data = list(
    observed = list(
      N = N * N_questions,
      N_questions = N_questions,
      questions = questions,
      ncat = ncat,
      X = X,
      N_fixed = N_fixed,
      N_monotonic = N_monotonic,
      N_monotonic_cat = N_monotonic_cat,
      X_monotonic = X_monotonic,
      N_random = N_random,
      N_random_groups = N_random_groups,
      X_random = X_random,
      X_random_groups = X_random_groups,
      random_hyper_sigma = random_hyper_sigma,
      Y = Y,
      intercept_sigma = 1
    ),
    true = list(
      q_corr_vec = q_corr_vec,
      b_sd = b_sd,
      b = b,
      b_monotonic = b_monotonic,
      zeta_monotonic = zeta_monotonic,
      b_random = b_random,
      sigma_random = sigma_random,
      intercept = intercept
    )
  )
}
