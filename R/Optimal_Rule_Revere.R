#' Learns the Optimal Rule given a tmle_task and likelihood
#' 
#' Uses new revere framework
#'
#' @importFrom R6 R6Class
#' @importFrom data.table data.table
#'
#' @export
#
Optimal_Rule_Revere <- R6Class(
  classname = "Optimal_Rule_Revere",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  lock_objects = FALSE,
  public = list(
    initialize = function(tmle_task, likelihood, fold_number = "split-specific",
                              V = NULL, blip_type = "blip2", blip_library, maximize = TRUE) {
      private$.tmle_task <- tmle_task
      private$.likelihood <- likelihood
      private$.fold_number <- fold_number
      private$.blip_type <- blip_type
      private$.blip_library <- blip_library
      private$.maximize <- maximize
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
    
    blip_revere_function = function(tmle_task, fold_number){
      likelihood <- self$likelihood
      
      A_vals <- tmle_task$npsem$A$variable_type$levels
      
      # Generate counterfactual tasks for each value of A:
      cf_tasks <- lapply(A_vals, function(A_val) {
        newdata <- data.table(A = A_val)
        cf_task <- tmle_task$generate_counterfactual_task(UUIDgenerate(), new_data = newdata)
        return(cf_task)
      })
      
      # DR A-IPW mapping of blip
      A <- tmle_task$get_tmle_node("A")
      Y <- tmle_task$get_tmle_node("Y")
      A_vals <- tmle_task$npsem$A$variable_type$levels
      A_ind <- self$factor_to_indicators(A,A_vals)
      Y_mat <- replicate(length(A_vals), Y)
      
      Q_vals <- sapply(cf_tasks, likelihood$get_likelihood, "Y", fold_number)
      g_vals <- sapply(cf_tasks, likelihood$get_likelihood, "A", fold_number)
      DR <- (A_ind / g_vals) * (Y_mat - Q_vals) + Q_vals
      
      # todo: port other blip code
      # todo: support multivariate outcome
      blip <- DR[,2] - DR[,1]
      V <- tmle_task$data[,self$V,with=FALSE]
      data <- data.table(V,blip=blip)
      revere_task <- make_sl3_Task(data, outcome="blip",covariates=self$V, folds=tmle_task$folds)
      
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
      
      # TODO: swap arg order in sl3
      blip_revere_task <- sl3:::sl3_revere_Task$new(self$blip_revere_function, tmle_task)
      blip_fit <- self$blip_library$train(blip_revere_task)
      private$.blip_fit <- blip_fit
    },

    rule = function(tmle_task, fold_number="full") {
      
      # TODO: when applying the rule, we actually only need the covariates
      blip_task <- self$blip_revere_function(tmle_task, fold_number)
      blip_preds <- self$blip_fit$predict_fold(blip_task, fold_number)
      
      rule_preds <- NULL
      if (!private$.maximize) {
        blip_preds <- blip_preds * -1
      }
      
      # TODO: reimplement multinomial
      if (TRUE) {
        # should be if blip_preds is 1-dimensional
        rule_preds <- as.numeric(blip_preds > 0)
      
      } else {
        if (blip_type == "blip1") {
          rule_preds <- max.col(blip_preds) + 1
        } else {
          rule_preds <- max.col(blip_preds)
        }
      }
    
      
      return(rule_preds)
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
      return(private$.blip_library)
    }
  ),
  private = list(
    .tmle_task = NULL,
    .likelihood = NULL,
    .fold_number = NULL,
    .V = NULL,
    .blip_type = NULL,
    .blip_fit = NULL,
    .blip_library = NULL,
    .maximize = NULL
  )
)
