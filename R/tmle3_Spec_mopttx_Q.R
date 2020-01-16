#' Defines the Mean Under the Optimal Individualized Rule with Categorical Treatment,
#' estimated using Q learning (single step).
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
    initialize = function(maximize = TRUE, ...) {
      options <- list(maximize = maximize, ...)
      do.call(super$initialize, options)
    },

    vals_from_factor = function(x) {
      sort(unique(x))
    },

    # Shrinkage based Q
    make_initial_likelihood_glm = function(tmle_task) {
      # TO DO
    },

    make_params = function(tmle_task, likelihood) {

      # Learn the rule
      opt_rule <- Optimal_Rule_Q_learning$new(tmle_task, likelihood,
        maximize = private$.options$maximize
      )
      opt_rule$fit_blip()

      # Define a dynamic Likelihood factor:
      lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
      tsm_rule <- Param_TSM$new(likelihood, lf_rule)

      return(list(tsm_rule))
    },

    estimate = function(tmle_params, tmle_task) {
      est <- lapply(tmle_params, function(tmle_param) {
        mean(tmle_param$cf_likelihood$get_likelihood(tmle_task = tmle_task, node = "Y"))
      })
      return(est)
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
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#'
#' @export
#'

tmle3_mopttx_Q <- function(maximize) {
  tmle3_Spec_mopttx_Q$new(maximize = maximize)
}
