#' Learns the Optimal Rule given a tmle_task and likelihood, using Q learning.
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
    initialize = function(tmle_task, likelihood, maximize = TRUE) {
      private$.tmle_task <- tmle_task
      private$.likelihood <- likelihood$initial_likelihood
      private$.maximize <- maximize
    },

    fit_blip = function() {
      tmle_task <- self$tmle_task

      # todo: function
      A_vals <- tmle_task$npsem$A$variable_type$levels
      A_vals <- factor(A_vals, A_vals)

      # Generate counterfactual tasks for each value of A:
      cf_tasks <- lapply(A_vals, function(A_val) {
        # if(is.character(A_val)){
        #  A_val<-as.numeric(A_val)
        # A_val<-as.factor(A_val)
        # }
        A_val <- as.numeric(A_val)
        newdata <- data.table(A = A_val)
        cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(), new_data = newdata)
        return(cf_task)
      })

      private$.cf_tasks <- cf_tasks
    },

    rule = function(tmle_task, fold_number = "full") {
      # Get Q(a,W) for each level of A, all folds
      blip_fin <- sapply(private$.cf_tasks, private$.likelihood$get_likelihood, "Y", fold_number)

      if (private$.maximize) {
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
    }
  ),
  private = list(
    .tmle_task = NULL,
    .likelihood = NULL,
    .cf_tasks = NULL,
    .maximize = NULL
  )
)
