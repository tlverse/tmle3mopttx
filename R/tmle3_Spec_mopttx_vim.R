#' Variable Importance with the Mean Under the Optimal Individualized Rule
#'
#' The functions contained in the class define a Variable Importance
#' metric for the TMLE of the Mean Under the Optimal Individualized Rule with
#' Categorical Treatment, learned and estimated under Revere CV-TMLE. For learning
#' the Optimal Rule, see 'Optimal_Rule_Revere' class.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @keywords data
#'
#' @return A tmle3 object inheriting from \code{\link[tmle3]{tmle3_Spec}} with
#' methods for obtaining the Variable Importance metric for the TMLE of the
#' Mean Under the Optimal Individualized Rule. For a full list of the available
#' functionality, see the complete documentation of \code{\link[tmle3]{tmle3_Spec}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link[tmle3]{tmle3_Spec}}.
#'  
#'
#' @section Parameters:
#'   - \code{V}: User-specified list of covariates used to define the rule.
#'   - \code{type}: Blip type, corresponding to different ways of defining the
#'   reference category in learning the blip; mostly applies to categorical treatment.
#'   Available categories include "blip1" (reference level of treatment), "blip2"
#'   (average level of treatment) and "blip3" (weighted average level of treatment).
#'   - \code{method}: Either "SL" (for the TMLE estimate) or "Q" (for Q-learning).
#'   - \code{learners}: List of user-defined learners for relevant parts of the
#'   likelihood.
#'   - \code{contrast}: Defined either a "linear" or "multiplicative" contrast for the delta method.
#'   - \code{maximize}: Should the average outcome be maximized of minimized? Default is
#'   maximize=TRUE.
#'   - \code{complex}: If \code{TRUE}, the returned mean under the Optimal Rule is based on the
#'   full set of covariates provided by the user (parameter "V"). If \code{FALSE}, simpler rules
#'   (including the static rules), are evaluated as well; the returned mean under the Optimal
#'   Rule is then a potentially more parsimonious rule, if the mean performance is similar.
#'   - \code{realistic}: If \code{TRUE}, the optimal rule returned takes into account the
#'   probability of treatment given covariates.
#'   - \code{resource}: Indicates the percent of initially estimated individuals who should be given 
#'   treatment that get treatment, based on their blip estimate. If resource = 1 all estimated 
#'   individuals to benefit from treatment get treatment, if resource = 0 none get treatment. 
#'
#' @examples
#' \dontrun{
#' library(sl3)
#' library(tmle3)
#' library(data.table)
#'
#' data("data_cat_vim")
#' data <- data_cat_vim
#' data$A <- as.integer(data$A)
#' 
#' lrn1 <- Lrnr_mean$new()
#' lrn2 <- Lrnr_glm_fast$new()
#' lrn3 <- Lrnr_glmnet$new()
#' 
#' Q_learner <- Lrnr_sl$new(learners = list(lrn1, lrn2, lrn3),
#' metalearner = Lrnr_nnls$new()
#' )
#' 
#' mn_metalearner <- make_learner(Lrnr_solnp,
#' loss_function = loss_loglik_multinomial,
#' learner_function = metalearner_linear_multinomial
#' )
#' g_learner <- make_learner(Lrnr_sl, list(lrn1, lrn3), 
#' mn_metalearner)
#' 
#' b_learner <- create_mv_learners(learners = list(lrn1, lrn2))
#' 
#' learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)
#' 
#' node_list <- list(W = c("W2", "W3", "W4"), 
#' A = c("A", "W1"), Y = "Y")
#'
#' tmle_spec <- tmle3_mopttx_vim(
#' V = "W3", learners = learner_list, type = "blip2",
#' contrast = "multiplicative", maximize = FALSE,
#' method = "SL", complex = TRUE, realistic = FALSE
#' )
#'}
tmle3_Spec_mopttx_vim <- R6Class(
  classname = "tmle3_Spec_mopttx_vim",
  portable = TRUE,
  class = TRUE,
  lock_objects = FALSE,
  inherit = tmle3_Spec_mopttx_blip_revere,
  public = list(
    initialize = function(V = NULL, type = "blip2", method = "SL", learners = NULL,
                          contrast = "linear", maximize = TRUE, complex = TRUE,
                          realistic = FALSE, resource = 1, reference=NULL, ...) {
      options <- list(
        V = V, type = type, method = method, learners = learners,
        contrast = contrast, maximize = maximize, complex = complex,
        realistic = realistic, resource = resource, reference=reference, ...
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
    # make_updater = function() {
    #  updater <- tmle3_cv_Update$new()
    # },

    set_opt = function(opt) {
      private$.opt <- opt
    },

    make_params = function(tmle_task, likelihood) {
      
      #Grab all parameters:
      V <- private$.options$V
      complex <- private$.options$complex
      max <- private$.options$maximize
      realistic <- private$.options$realistic
      resource <- private$.options$resource
      method <- private$.options$method

      if (method == "Q") {
        # Learn the rule using Q-learning:
        opt_rule <- Optimal_Rule_Q_learning$new(tmle_task, likelihood,
          maximize = private$.options$maximize
        )
      } else if (method == "SL") {
        # Learn the rule
        opt_rule <- Optimal_Rule_Revere$new(tmle_task,
                                            tmle_spec = self, likelihood$initial_likelihood,
                                            V =  V, options = private$.options
        )
      }

      opt_rule$fit_blip()
      self$set_opt(opt_rule)

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

      contrast_param <- Param_delta$new(
        likelihood, contrast_delta,
        list(mean_param, tsm_rule)
      )

      return(list(tsm_rule, mean_param, contrast_param))
    }
  ),
  active = list(),
  private = list(
    opt = list()
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
#' @param learners Library for Y (outcome), A (treatment), and B (blip) estimation.
#' @param method Specifies which methodology to use for learning the rule. Options are "Q" for Q-learning, and
#' "SL" for the Super-Learner approach using split-specific estimates.
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#' @param complex If \code{TRUE}, learn the rule using the specified covariates \code{V}. If
#' \code{FALSE}, check if a less complex rule is better.
#' @param realistic If \code{TRUE}, it will return a rule what is possible due to practical positivity constraints.
#' @param contrast Defined either a "linear" or "multiplicative" contrast for the delta method.
#' @param resource Indicates the percent of initially estimated individuals who should be given 
#' treatment that get treatment, based on their blip estimate. If resource = 1 all estimated 
#' individuals to benefit from treatment get treatment, if resource = 0 none get treatment. 
#' @param reference reference category for blip1. Default is the smallest numerical category or factor.
#'
#' @export
tmle3_mopttx_vim <- function(V = NULL, type = "blip2", method = "SL", learners = NULL,
                             contrast = "linear", maximize = TRUE, complex = TRUE, realistic = FALSE,
                             resource = 1, reference=NULL) {
  tmle3_Spec_mopttx_vim$new(
    V = V, type = type, method = method, learners = learners,
    contrast = contrast, maximize = maximize, complex = complex, 
    realistic = realistic, resource = resource, reference = reference
  )
}
