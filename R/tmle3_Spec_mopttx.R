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
    initialize = function(V, type, ...) {
      options <- list(V = V, type=type)
      do.call(super$initialize, options)
    },
  
    vals_from_factor = function(x) {
      sort(unique(x))
    },
    
    make_params = function(tmle_task, likelihood) {
      baseline_level <- self$options$baseline_level
      intervention <- define_lf(LF_static, "A", value = baseline_level)
      tsm <- Param_TSM$new(likelihood, intervention)
      mean_param <- Param_mean$new(likelihood)
      par <- Param_delta$new(likelihood, delta_param_PAR, list(tsm, mean_param))
      paf <- Param_delta$new(likelihood, delta_param_PAF, list(tsm, mean_param))
      rr <- Param_delta$new(likelihood, delta_param_PAF, list(tsm, mean_param))
      tmle_params <- list(tsm, mean_param, par, paf, rr)

      return(tmle_params)
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
tmle3_mopttx <- function(V, type) {
  tmle3_Spec_mopttx$new(V=V, type=type)
}

#' @import data.table
NULL