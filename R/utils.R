## Helper functions

#' Make SL of multivariate learners:
#' 
#' @param learners List of learners supporting multivariate prediction.
#' 
#' @export
#' 

create_mv_learners <- function(learners){
  mv_learners <- lapply(learners, function(learner) sl3::make_learner(sl3::Lrnr_multivariate, learner))
  mv_stack <- sl3::make_learner(sl3::Stack, mv_learners)
  
  mv_metalearner <- sl3::make_learner(sl3::Lrnr_solnp,
                                 loss_function = sl3::loss_squared_error_multivariate,
                                 learner_function = sl3::metalearner_linear_multivariate
  )
  b_learner <- sl3::make_learner(sl3::Lrnr_sl, mv_stack, mv_metalearner)
  return(mv_learner=b_learner)
}

#' Q learning wrapper
#' 
#' @param tmle_spec_Q TMLE Spec initializing Q learning.
#' @param initial_likelihood Initial likelihood object as defined and obtained by tmle3.
#' @param tmle_task TMLE task object as defined and obtained by tmle3.
#' 
#' @export
#' 

Q_learning <- function(tmle_spec_Q, initial_likelihood, tmle_task){
  
  # Define updater and targeted likelihood:
  updater <- tmle_spec_Q$make_updater()
  targeted_likelihood <- tmle_spec_Q$make_targeted_likelihood(initial_likelihood, 
                                                              updater)
  tmle_params <- tmle_spec_Q$make_params(tmle_task, targeted_likelihood)
  psi<-tmle_spec_Q$estimate(tmle_params,tmle_task)[[1]]

  return(psi=psi)
}


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
