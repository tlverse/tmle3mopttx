#' Defines a TMLE for the Mean Under the Optimal Individualized Rule with Categorical Treatment and contrast with
#' the Mean under Observed Treatment.
#'
#' @importFrom R6 R6Class
#'
#' @param V Covariates the rule depends on.
#' @param type One of three psudo-blip versions developed to accommodate categorical treatment. "Blip1"
#' corresponds to chosing a reference category, and defining the blip for all other categories relative to the
#' specified reference. Note that in the case of binary treatment, "blip1" is just the usual blip.
#' "Blip2$ corresponds to defining the blip relative to the average of all categories. Finally,
#' "Blip3" corresponds to defining the blip relative to the weighted average of all categories.
#' @param b_learner Library for blip estimation.
#' @param method Specifies which methodology to use for learning the rule. Options are "Q" for Q-learning, and
#' "SL" for the Super-Learner approach using split-specific estimates.
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#'
#' @export
#

tmle3_Spec_mopttx_vim <- R6Class(
  classname = "tmle3_Spec_mopttx_vim",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(V = NULL, type = "blip2", method = "SL", b_learner = NULL,
                              contrast = "linear", maximize = TRUE...) {
      options <- list(
        V = V, type = type, method = method, b_learner = b_learner,
        contrast = contrast, maximize = maximize
      )
      do.call(super$initialize, options)
    },

    vals_from_factor = function(x) {
      sort(unique(x))
    },

    make_tmle_task = function(data, node_list, ...) {
      # bound Y if continuous
      Y_node <- node_list$Y
      Y_vals <- unlist(data[, Y_node, with = FALSE])
      Y_variable_type <- variable_type(x = Y_vals)
      if (Y_variable_type$type == "continuous") {
        min_Y <- min(Y_vals)
        max_Y <- max(Y_vals)
        range <- max_Y - min_Y
        lower <- min_Y # - 0.1 * range
        upper <- max_Y # + 0.1 * range
        Y_variable_type <- variable_type(
          type = "continuous",
          bounds = c(lower, upper)
        )
      }

      # todo: export and use sl3:::get_levels
      A_node <- node_list$A
      A_vals <- unlist(data[, A_node, with = FALSE])
      if (is.factor(A_vals)) {
        A_levels <- sort(unique(A_vals))
        A_levels <- factor(A_levels, A_levels)
      } else {
        A_levels <- sort(unique(A_vals))
      }
      A_variable_type <- variable_type(
        type = "categorical",
        levels = A_levels
      )

      # make tmle_task
      npsem <- list(
        define_node("W", node_list$W),
        define_node("A", node_list$A, c("W"), A_variable_type),
        define_node("Y", node_list$Y, c("A", "W"), Y_variable_type)
      )

      if (!is.null(node_list$id)) {
        tmle_task <- tmle3_Task$new(data, npsem = npsem, id = node_list$id, ...)
      } else {
        tmle_task <- tmle3_Task$new(data, npsem = npsem, ...)
      }

      return(tmle_task)
    },
    make_updater = function() {
      updater <- tmle3_cv_Update$new()
    },
    
    set_B_rule = function(opt){
      private$B_rule<-opt
    },

    make_params = function(tmle_task, likelihood) {
      method <- private$.options$method

      if (method == "Q") {
        # Learn the rule using Q-learning:
        opt_rule <- Optimal_Rule_Q_learning$new(tmle_task, likelihood,
          maximize = private$.options$maximize
        )
      } else if (method == "SL") {
        # Learn the rule using split-specific methodology:
        opt_rule <- Optimal_Rule$new(tmle_task, likelihood,
          cv_fold = "split-specific",
          V = private$.options$V,
          blip_library = private$.options$b_learner,
          maximize = private$.options$maximize
        )
      }

      opt_rule$fit_blip()
      self$set_B_rule(opt_rule)

      # Define a dynamic Likelihood factor:
      lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
      tsm_rule <- Param_TSM$new(likelihood, lf_rule)
      mean_param <- Param_mean$new(likelihood)

      if (private$.options$contrast == "linear") {
        contrast_delta <- delta_param_ATE
      } else if (private$.options$contrast == "multiplicative") {
        contrast_delta <- delta_param_RR
      } else {
        stop("Contrast can be either linear or multiplicative")
      }

      contrast_param <- Param_delta$new(likelihood, contrast_delta, list(mean_param, tsm_rule))

      return(list(tsm_rule, mean_param, contrast_param))
    }
  ),
  active = list(),
  private = list(
    B_rule=list()
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
#' @param method Specifies which methodology to use for learning the rule. Options are "Q" for Q-learning, and
#' "SL" for the Super-Learner approach using split-specific estimates.
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#'
#' @export
#'

tmle3_mopttx_vim <- function(V = NULL, type = "blip2", method = "SL", b_learner = NULL,
                             contrast = "linear", maximize = TRUE) {
  tmle3_Spec_mopttx_vim$new(
    V = V, type = type, method = method, b_learner = b_learner,
    contrast = contrast, maximize = maximize
  )
}
