#' Defines a TMLE for the Mean Under the Optimal Individualized Rule with Categorical Treatment
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_rule <- R6Class(
  classname = "tmle3_Spec_rule",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(rule, ...) {
      options <- list(rule=rule)
      do.call(super$initialize, options)
    },

   
    make_params = function(tmle_task, likelihood) {
      
        # Define a dynamic Likelihood factor:
        lf_rule <- define_lf(LF_rule, "A", rule_fun = self$options$rule)
        tsm <- Param_TSM$new(likelihood, v = v, lf_rule)
        return(list(tsm))
    }
  ),
  active = list(),
  private = list(
  )
)

#' Mean under the Optimal Individualized Treatment Rule
#'
#' O=(W,A,Y)
#' W=Covariates
#' A=Treatment (binary or categorical)
#' Y=Outcome (binary or bounded continuous)
#'
#' @param V Covariates the rule depends on.
#' @param type One of three psudo-blip versions developed to accommodate categorical treatment. "Blip1"
#' corresponds to chosing a reference category, and defining the blip for all other categories relative to the
#' specified reference. Note that in the case of binary treatment, "blip1" is just the usual blip.
#' "Blip2$ corresponds to defining the blip relative to the average of all categories. Finally,
#' "Blip3" corresponds to defining the blip relative to the weighted average of all categories.
#' @param b_learner Library for blip estimation.
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#' @param complex If \code{TRUE}, learn the rule using the specified covariates \code{V}. If
#' \code{FALSE}, check if a less complex rule is better.
#'
#' @export
#'

tmle3_mopttx_blip_revere <- function(V, type = "blip1", b_learner, maximize = TRUE, complex = TRUE) {
  tmle3_Spec_mopttx_blip_revere$new(V = V, type = type, b_learner = b_learner, maximize = maximize, complex = complex)
}
