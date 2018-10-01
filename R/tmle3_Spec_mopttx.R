#' Defines a tmle for the mean under the optimal individualized treatment with categorical treatment
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_mopttx <- R6Class(
  classname = "tmle3_Spec_mopttx",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(V, type, b_learner, ...) {
      options <- list(V = V, type=type, b_learner=b_learner)
      do.call(super$initialize, options)
    },
  
    vals_from_factor = function(x) {
      sort(unique(x))
    },
    
    make_updater = function(){
      updater <- tmle3_cv_Update$new()
    },
    
    make_targeted_likelihood = function(likelihood, updater){
      
      targeted_likelihood <- Targeted_Likelihood$new(likelihood, 
                                                     updater)
      return(c(targeted_likelihood, likelihood))
    },
    
    make_params = function(tmle_task, likelihood) {
      
      #Learn the rule
      opt_rule <- Optimal_Rule$new(tmle_task, likelihood[[2]], "split-specific", 
                                   blip_library=private$.options$b_learner)
      opt_rule$fit_blip()
      
      #Define a dynamic Likelihood factor:
      lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
      tsm_rule <- Param_TSM$new(likelihood[[1]], lf_rule)

      return(list(tsm_rule))
    }
    
  ),
  active = list(),
  private = list(
  )
)

#'
#' O=(W,A,Y)
#' W=Covariates
#' A=Treatment (binary or categorical)
#' Y=Outcome (binary or bounded continuous)
#' V=Covariates the rule depends on
#' @importFrom sl3 make_learner Lrnr_mean
#' @export
tmle3_mopttx <- function(V, type, b_learner) {
  tmle3_Spec_mopttx$new(V=V, type=type, b_learner=b_learner)
}

#' @import data.table
NULL