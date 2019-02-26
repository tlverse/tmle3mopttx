## Helper functions

#Make SL of multivariate learners:
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
