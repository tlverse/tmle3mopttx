#' Defines the Mean Under the Optimal Individualized Rule with Categorical Treatment, estimated using Q learning. 
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_mopttx_Q <- R6Class(
  classname = "tmle3_Spec_mopttx_Q",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(V, type, b_learner, ...) {
      options <- list(V = V, type = type, b_learner = b_learner)
      do.call(super$initialize, options)
    },
    
    vals_from_factor = function(x) {
      sort(unique(x))
    },
    
    make_updater = function() {
      updater <- tmle3_cv_Update$new()
    },
    
    make_params = function(tmle_task, likelihood) {
      
      # Learn the rule
      opt_rule <- Optimal_Rule$new(tmle_task, likelihood, "split-specific",
                                   blip_library = private$.options$b_learner
      )
      opt_rule$fit_blip()
      
      # Define a dynamic Likelihood factor:
      lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
      tsm_rule <- Param_TSM$new(likelihood, lf_rule)
      
      return(list(tsm_rule))
    }
  ),
  active = list(),
  private = list()
)

#' Mean under the Optimal Individualized Treatment Rule, using Q learning
#'
#' O=(W,A,Y)
#' W=Covariates
#' A=Treatment (binary or categorical)
#' Y=Outcome (binary or bounded continuous)
#'
#' @param V Covariates the rule depends on
#' @param type One of three psudo-blip versions developed to accommodate categorical treatment. "Blip1"
#' corresponds to chosing a reference category, and defining the blip for all other categories relative to the
#' specified reference. Note that in the case of binary treatment, "blip1" is just the usual blip.
#' "Blip2$ corresponds to defining the blip relative to the average of all categories. Finally,
#' "Blip3" corresponds to defining the blip relative to the weighted average of all categories.
#' @param b_learner Library for blip estimation.
#'
#' @export
#'

tmle3_mopttx_Q <- function(V, type, b_learner) {
  tmle3_Spec_mopttx_Q$new(V = V, type = type, b_learner = b_learner)
}
