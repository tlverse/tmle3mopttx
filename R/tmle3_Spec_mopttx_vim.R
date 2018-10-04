#' Defines a TMLE for the Mean Under the Optimal Individualized Rule with Categorical Treatment and contrast with
#' Observed mean
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_mopttx_vim <- R6Class(
  classname = "tmle3_Spec_mopttx_vim",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(V, type, b_learner, contrast = "linear", maximize = TRUE...) {
      options <- list(V = V, type = type, b_learner = b_learner, contrast = contrast, maximize = maximize)
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
      opt_rule <- Optimal_Rule_Q_learning$new(tmle_task, likelihood, "split-specific",
                                   V = private$.options$V,         
                                   blip_library = private$.options$b_learner,
                                   maximize = private$.options$maximize
                                  )
      
      opt_rule$fit_blip()

      # Define a dynamic Likelihood factor:
      lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
      tsm_rule <- Param_TSM$new(likelihood, lf_rule)
      mean_param <- Param_mean$new(likelihood)
      if(private$.options$contrast == "linear"){
        contrast_delta <- delta_param_ATE
      } else{
        contrast_delta <- delta_param_RR
      }
      
      contrast_param <- Param_delta$new(likelihood, contrast_delta, list(mean_param, tsm_rule))
      
      return(list(tsm_rule, mean_param, contrast_param))
    }
  ),
  active = list(),
  private = list()
)

#' Mean under the Optimal Individualized Treatment Rule
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

tmle3_mopttx_vim <- function(V, type, b_learner, contrast, maximize) {
  tmle3_Spec_mopttx_vim$new(V = V, type = type, b_learner = b_learner, contrast = contrast,maximize = maximize)
}
