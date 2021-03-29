#' Learning the Optimal Rule using the Revere framework
#'
#' Functions used to learn the Optimal Rule given a tmle_task and likelihood,
#' using the Revere framework. Complements 'tmle3_Spec_mopttx_blip_revere' class.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom data.table data.table
#'
#' @export
#'
#' @keywords data
#'
#' @return A optimal rule object inheriting from \code{\link{tmle3_Spec}} with
#' methods for learning the optimal rule. For a full list of the available
#' functionality, see the complete documentation of \code{\link{tmle3_Spec}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{tmle3_Spec}}.
#'
#' @section Parameters:
#'   - \code{tmle_task}: Task object of \code{\link[keras]{tmle3}} specifying the data and
#'   node structure.
#'   - \code{likelihood}: Likelihood object of \code{\link[keras]{tmle3}}, corresponding
#'   to the current estimate of the required parts of the likelihood necessary for the target
#'   parameter.
#'   - \code{fold_number}: split-specific.
#'   - \code{V}: User-specified list of covariates used to define the rule.
#'   - \code{blip_type}: Blip type, corresponding to different ways of defining the
#'   reference category in learning the blip; mostly applies to categorical treatment.
#'   Available categories include "blip1" (reference level of treatment), "blip2"
#'   (average level of treatment) and "blip3" (weighted average level of treatment).
#'   - \code{learners}: List of user-defined learners for relevant parts of the
#'   likelihood.
#'   - \code{maximize}: Should the average outcome be maximized of minimized? Default is
#'   maximize=TRUE.
#'   - \code{realistic}: If TRUE, the optimal rule returned takes into account the
#'   probability of treatment given covariates.
#'   - \code{shift_grid}: Grid of possible values for the stochastic optimal rule.
#'   Work in progress.
#'
#' @examples
#' library(sl3)
#' library(tmle3)
#' library(data.table)
#'
#' data("data_bin")
#' data <- data_bin
#'
#' Q_lib <- make_learner_stack("Lrnr_mean", "Lrnr_glm_fast")
#' g_lib <- make_learner_stack("Lrnr_mean", "Lrnr_glm_fast")
#' B_lib <- make_learner_stack("Lrnr_glm_fast", "Lrnr_xgboost")
#'
#' metalearner <- make_learner(Lrnr_nnls)
#' Q_learner <- make_learner(Lrnr_sl, Q_lib, metalearner)
#' g_learner <- make_learner(Lrnr_sl, g_lib, metalearner)
#' B_learner <- make_learner(Lrnr_sl, B_lib, metalearner)
#'
#' learner_list <- list(Y = Q_learner, A = g_learner, B = B_learner)
#'
#' node_list <- list(W = c("W1", "W2", "W3"), A = "A", Y = "Y")
#'
#' tmle_spec <- tmle3_mopttx_blip_revere(
#'   V = c("W1", "W2", "W3"),
#'   type = "blip1", learners = learner_list, maximize = TRUE,
#'   complex = TRUE, realistic = TRUE
#' )
#'
#' fit <- tmle3(tmle_spec, data, node_list, learner_list)
#' fit$summary
Optimal_Rule_Revere <- R6Class(
  classname = "Optimal_Rule_Revere",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  lock_objects = FALSE,
  public = list(
    initialize = function(tmle_task, likelihood, fold_number = "split-specific", V = NULL,
                          blip_type = "blip2", learners, maximize = TRUE, realistic = FALSE,
                          shift_grid = seq(-1, 1, by = 0.5)) {
      private$.tmle_task <- tmle_task
      private$.likelihood <- likelihood
      private$.fold_number <- fold_number
      private$.blip_type <- blip_type
      private$.learners <- learners
      private$.maximize <- maximize
      private$.realistic <- realistic
      private$.shift_grid <- shift_grid

      if (missing(V)) {
        V <- tmle_task$npsem$W$variables
      }

      private$.V <- V
      private$.fold_number <- fold_number
    },
    factor_to_indicators = function(x, x_vals) {
      ind_mat <- sapply(x_vals, function(x_val) as.numeric(x_val == x))
      colnames(ind_mat) <- x_vals
      return(ind_mat)
    },
    V_data = function(tmle_task, fold = NULL) {
      if (is.null(fold)) {
        tmle_task$data[, self$V, with = FALSE]
      } else {
        tmle_task$data[, self$V, with = FALSE][tmle_task$folds[[fold]]$training_set, ]
      }
    },
    DR_full = function(v, indx) {
      DR <- data.frame(private$.DR_full[[v]])
      return(data.frame(DR[indx, ]))
    },

    blip_revere_function = function(tmle_task, fold_number) {
      likelihood <- self$likelihood
      A_vals <- tmle_task$npsem$A$variable_type$levels
      V <- self$V

      # Generate counterfactual tasks for each value of A:
      cf_tasks <- lapply(A_vals, function(A_val) {
        if (is.character(A_val)) {
          A_val <- as.numeric(A_val)
          # A_val<-as.factor(A_val)
        }
        newdata <- data.table(A = A_val)
        cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(), new_data = newdata)
        return(cf_task)
      })

      # DR A-IPW mapping of blip
      A <- tmle_task$get_tmle_node("A")
      Y <- tmle_task$get_tmle_node("Y")
      A_vals <- tmle_task$npsem$A$variable_type$levels
      A_ind <- self$factor_to_indicators(A, A_vals)
      Y_mat <- replicate(length(A_vals), Y)

      # Use fold_number fits for Q and g:
      Q_vals <- sapply(cf_tasks, likelihood$get_likelihood, "Y", fold_number)
      g_vals <- sapply(cf_tasks, likelihood$get_likelihood, "A", fold_number)
      DR <- (A_ind / g_vals) * (Y_mat - Q_vals) + Q_vals

      # Type of pseudo-blip:
      blip_type <- self$blip_type

      if (blip_type == "blip1") {
        blip <- DR[, 2] - DR[, 1]
      } else if (blip_type == "blip2") {
        blip <- DR - rowMeans(DR)
      } else if (blip_type == "blip3") {
        blip <- DR - (rowMeans(DR) * g_vals)
      }

      # TO DO: Nicer solutions. Do it one by one, for now
      if (is.null(V)) {
        data <- data.table(V = blip, blip = blip)
        outcomes <- grep("blip", names(data), value = TRUE)
        V <- grep("V", names(data), value = TRUE)
        revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = V, folds = tmle_task$folds)
      } else {
        V <- tmle_task$data[, self$V, with = FALSE]
        data <- data.table(V, blip = blip)
        outcomes <- grep("blip", names(data), value = TRUE)
        revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = self$V, folds = tmle_task$folds)
      }

      return(revere_task)
    },

    bound = function(cv_g) {
      cv_g[cv_g < 0.01] <- 0.01
      cv_g[cv_g > 0.99] <- 0.99
      return(cv_g)
    },

    fit_blip = function() {
      tmle_task <- self$tmle_task
      likelihood <- self$likelihood
      fold_number <- self$fold_number
      V <- self$V

      # TODO: swap arg order in sl3
      blip_revere_task <- sl3:::sl3_revere_Task$new(self$blip_revere_function, tmle_task)
      blip_fit <- self$blip_library$train(blip_revere_task)
      private$.blip_fit <- blip_fit
    },

    rule = function(tmle_task, fold_number = "full") {
      realistic <- private$.realistic
      likelihood <- self$likelihood

      # TODO: when applying the rule, we actually only need the covariates
      blip_task <- self$blip_revere_function(tmle_task, fold_number)
      blip_preds <- self$blip_fit$predict_fold(blip_task, fold_number)

      # Type of pseudo-blip:
      blip_type <- self$blip_type

      if (is.list(blip_preds)) {
        blip_preds <- unpack_predictions(blip_preds)
      }

      # flip sign if we're minimizing
      if (!private$.maximize) {
        blip_preds <- blip_preds * -1
      }

      # add an extra 0 column for blip1 so that there's always one column per A level
      if (blip_type == "blip1") {
        blip_preds <- cbind(0, blip_preds)
      }

      if (realistic) {

        # Need to grab the propensity score:
        g_learner <- likelihood$factor_list[["A"]]$learner
        g_task <- tmle_task$get_regression_task("A")
        g_preds <- unpack_predictions(g_learner$predict(g_task))
        min_g <- 0.05

        # make unrealistic rules not optimal
        g_preds <- normalize_rows(g_preds)
        blip_preds[g_preds < min_g] <- -Inf
      }

      rule_preds <- max.col(blip_preds)

      A_vals <- tmle_task$npsem$A$variable_type$levels
      rule_preds <- A_vals[rule_preds]
      return(rule_preds)
    },

    # TO DO: Think carefully as to how this should be done with folds.
    rule_stochastic = function(tmle_task, fold_number = "full") {
      likelihood <- self$likelihood
      shift_grid <- self$shift_grid
      A <- tmle_task$get_tmle_node("A")

      # TO DO: Only supports additive shifts for now.
      # Generate counterfactual tasks for each delta shift of A:
      cf_tasks <- lapply(shift_grid, function(shift) {
        newdata <- data.table(A = A + shift)
        cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(), new_data = newdata)
        return(cf_task)
      })

      Q_vals <- sapply(cf_tasks, likelihood$get_likelihood, "Y", fold_number)
      opt_col <- max.col(Q_vals)
      opt_A <- Q_vals[cbind(seq_along(opt_col), opt_col)]
      private$.opt_delta <- shift_grid[opt_col]

      private$.opt_A <- opt_A
      private$.Q_vals <- Q_vals

      return(opt_A)
    }
  ),
  active = list(
    tmle_task = function() {
      return(private$.tmle_task)
    },
    likelihood = function() {
      return(private$.likelihood)
    },
    fold_number = function() {
      return(private$.fold_number)
    },
    V = function() {
      return(private$.V)
    },
    blip_type = function() {
      return(private$.blip_type)
    },
    blip_fit = function() {
      return(private$.blip_fit)
    },
    blip_library = function() {
      return(private$.learners$B)
    },
    A_library = function() {
      return(private$.learners$A)
    },
    shift_grid = function() {
      return(private$.shift_grid)
    }
  ),
  private = list(
    .tmle_task = NULL,
    .likelihood = NULL,
    .fold_number = NULL,
    .V = NULL,
    .blip_type = NULL,
    .blip_fit = NULL,
    .learners = NULL,
    .maximize = NULL,
    .realistic = NULL,
    .shift_grid = NULL,
    .opt_delta = NULL,
    .opt_A = NULL,
    .Q_vals = NULL
  )
)
