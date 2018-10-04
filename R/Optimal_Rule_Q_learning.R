#' Learns the Optimal Rule given a tmle_task and likelihood
#'
#' @importFrom R6 R6Class
#' @importFrom data.table data.table
#'
#' @export
#
Optimal_Rule_Q_learning <- R6Class(
  classname = "Optimal_Rule_Q_learning",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(tmle_task, likelihood, cv_fold = "split-specific", V = NULL, blip_type = "blip2", blip_library, maximize = TRUE) {
      private$.tmle_task <- tmle_task
      private$.likelihood <- likelihood$initial_likelihood
      private$.cv_fold <- cv_fold
      private$.blip_type <- blip_type
      private$.blip_library <- blip_library
      private$.maximize <- maximize
      if (missing(V)) {
        V <- tmle_task$npsem$W$variables
      }
      
      private$.V <- V
      
      private$.cv_fold <- cv_fold
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
    fit_blip = function() {
      tmle_task <- self$tmle_task
      likelihood <- self$likelihood
      cv_fold <- self$cv_fold

      # todo: function
      A_levels <- tmle_task$npsem$A$variable_type$levels
      A_levels <- factor(A_levels, A_levels)
      # Generate counterfactual tasks for each value of A:
      cf_tasks <- lapply(A_levels, function(A_level) {
        newdata <- data.table(A = A_level)
        cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(), new_data = newdata)
        return(cf_task)
      })

      private$.cf_tasks <- cf_tasks
    },

    rule = function(tmle_task) {
      blip_fin <- sapply(private$.cf_tasks, private$.likelihood$get_likelihood, "Y",-1)

      if(private$.maximize){
        rule_index <- max.col(blip_fin)
      } else {
        rule_index <- max.col(-1 * blip_fin)
      }

      # todo: only if factor
      A_levels <- tmle_task$npsem$A$variable_type$levels
      A_levels <- factor(A_levels, A_levels)
      
      rule <- A_levels[rule_index]
      return(rule)
    }
  ),
  active = list(
    tmle_task = function() {
      return(private$.tmle_task)
    },
    likelihood = function() {
      return(private$.likelihood)
    },
    cv_fold = function() {
      return(private$.cv_fold)
    },
    V = function() {
      return(private$.V)
    },
    blip_type = function() {
      return(private$.blip_type)
    },
    blip_fits = function() {
      return(private$.blip_fits)
    },
    blip_fits_sl = function() {
      return(private$.blip_fits_sl)
    },
    blip_library = function() {
      return(private$.blip_library)
    }
  ),
  private = list(
    .tmle_task = NULL,
    .likelihood = NULL,
    .cv_fold = NULL,
    .V = NULL,
    .blip_type = NULL,
    .blip_library = NULL,
    .DR_full = NULL,
    .cf_tasks = NULL,
    .maximize = NULL
  )
)
