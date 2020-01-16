## Helper functions

#' Normalize rows
#'
#' @param x Values needed to be normalized.
#'
#' @export
#'
normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}

#' Get factors
#'
#' @param x Values from which we obtain factors.
#'
#' @export
#'
#'
vals_from_factor <- function(x) {
  sort(unique(x))
}

#' Make SL of multivariate learners
#'
#' @param learners List of learners supporting multivariate prediction.
#'
#' @export
#'
create_mv_learners <- function(learners) {
  mv_learners <- lapply(learners, function(learner) sl3::make_learner(sl3::Lrnr_multivariate, learner))
  mv_stack <- sl3::make_learner(sl3::Stack, mv_learners)

  mv_metalearner <- sl3::make_learner(sl3::Lrnr_solnp,
    loss_function = sl3::loss_squared_error_multivariate,
    learner_function = sl3::metalearner_linear_multivariate
  )
  b_learner <- sl3::make_learner(sl3::Lrnr_sl, mv_stack, mv_metalearner)
  return(mv_learner = b_learner)
}

#' Q learning wrapper
#'
#' @param tmle_spec_Q TMLE Spec initializing Q learning.
#' @param learner_list List of algorithms used to fit Q
#' @param B Number of bootstraps
#' @param data Dataset used
#' @param node_list List of nodes corresponding to Y, A and W.
#'
#' @export
#'
Q_learning <- function(tmle_spec_Q, learner_list, B = 1000, data, node_list) {

  ## Estimate the parameter
  bst <- function(i) {

    # Resample
    data_new <- dplyr::sample_n(data, replace = TRUE, size = nrow(data))

    # Define data with tmle3
    tmle_task <- tmle_spec_Q$make_tmle_task(data_new, node_list)

    # Define likelihood
    initial_likelihood <- tmle_spec_Q$make_initial_likelihood(tmle_task, learner_list)

    # Define updater and targeted likelihood
    updater <- tmle_spec_Q$make_updater()
    targeted_likelihood <- tmle_spec_Q$make_targeted_likelihood(
      initial_likelihood,
      updater
    )
    tmle_params <- tmle_spec_Q$make_params(tmle_task, targeted_likelihood)
    psi <- tmle_spec_Q$estimate(tmle_params, tmle_task)[[1]]
    updater <- targeted_likelihood <- NULL
    return(psi = psi)
  }

  ## Bootstrap
  bootstrap_results <- lapply(seq_len(B), function(iter) bst(iter))

  ## Get the CI
  results <- do.call(rbind, bootstrap_results)
  psi <- mean(results)
  var <- var(results)
  CI <- quantile(results, prob = c(0.025, 0.975))

  return(list(psi = psi, variance = var, CI = CI, all_psi = results))
}
