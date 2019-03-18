## Helper functions

#Make SL of multivariate learners:
#' @export
create_mv_learners <- function(learners){
  mv_learners <- lapply(learners, function(learner) make_learner(Lrnr_multivariate, learner))
  mv_stack <- make_learner(Stack, mv_learners)
  
  mv_metalearner <- make_learner(Lrnr_solnp,
                                 loss_function = loss_squared_error_multivariate,
                                 learner_function = metalearner_linear_multivariate
  )
  b_learner <- make_learner(Lrnr_sl, mv_stack, mv_metalearner)
  return(mv_learner=b_learner)
}

#Q learning
#' @export
Q_learning <- function(tmle_spec_Q, initial_likelihood, tmle_task){
  
  # Define updater and targeted likelihood:
  updater <- tmle_spec_Q$make_updater()
  targeted_likelihood <- tmle_spec_Q$make_targeted_likelihood(initial_likelihood, 
                                                              updater)
  tmle_params <- tmle_spec_Q$make_params(tmle_task, targeted_likelihood)
  psi<-tmle_spec_Q$estimate(tmle_params,tmle_task)[[1]]

  return(psi=psi)
}


#Normalize rows:
#' @export
normalize_rows <- function(x) {
  sweep(x, 1, rowSums(x), "/")
}

#Get factors:
#' @export
vals_from_factor <- function(x) {
  sort(unique(x))
}
