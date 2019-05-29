#' Learns the Optimal Rule given a tmle_task and likelihood, using the Revere framework.
#' Complements 'tmle3_Spec_mopttx_blip_revere'.
#'
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
    initialize = function(tmle_task, likelihood, fold_number = "split-specific", V = NULL,
                              blip_type = "blip2", learners, maximize = TRUE, realistic = FALSE) {
      private$.tmle_task <- tmle_task
      private$.likelihood <- likelihood
      private$.fold_number <- fold_number
      private$.blip_type <- blip_type
      private$.learners <- learners
      private$.maximize <- maximize
      private$.realistic <- realistic

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
        blip_preds <- cbind(0,blip_preds)
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
    .realistic = NULL
  )
)
