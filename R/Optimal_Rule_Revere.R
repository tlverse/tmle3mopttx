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
    initialize = function(tmle_task, tmle_spec, likelihood, V = NULL,
                          blip_type = "blip2", learners, maximize = TRUE, realistic = FALSE,
                          shift_grid = seq(-1, 1, by = 0.5)) {
      private$.tmle_task <- tmle_task
      private$.tmle_spec <- tmle_spec
      private$.likelihood <- likelihood
      private$.blip_type <- blip_type
      private$.learners <- learners
      private$.maximize <- maximize
      private$.realistic <- realistic
      private$.shift_grid <- shift_grid

      if (missing(V)) {
        V <- tmle_task$npsem$W$variables
      }

      private$.V <- V
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

      #If there are missing values in Y, 
      #Blip outcomes will have missing values as well
      if (blip_type == "blip1") {
        blip <- DR[, 2] - DR[, 1]
      } else if (blip_type == "blip2") {
        blip <- DR - rowMeans(DR)
      } else if (blip_type == "blip3") {
        blip <- DR - (rowMeans(DR) * g_vals)
      }

      # TO DO: Nicer solutions. Do it one by one, for now
      # If there are missing Ys, there will be missing blips- for now drop these rows.
      #(otherwise we train on imputed values... )
      if (is.null(V)) {
        data <- data.table(V = blip, blip = blip)
        outcomes <- grep("blip", names(data), value = TRUE)
        V <- grep("V", names(data), value = TRUE)
        
        #Drop censored values:
        if(!is.null(tmle_task$npsem$Y$censoring_node)){
          delta<-tmle_task$npsem$Y$censoring_node$name
          observed <- tmle_task$get_tmle_node(delta)  
          
          data <- data[observed,]
          folds <- sl3::subset_folds(tmle_task$folds,which(observed))
          
          revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = V, 
                                       folds = folds)
        }else{
          revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = V, 
                                       folds = tmle_task$folds)
        }
      } else {
        V <- tmle_task$data[, self$V, with = FALSE]
        data <- data.table(V, blip = blip)
        outcomes <- grep("blip", names(data), value = TRUE)
        
        #Drop censored values:
        if(!is.null(tmle_task$npsem$Y$censoring_node)){
          delta<-tmle_task$npsem$Y$censoring_node$name
          observed <- tmle_task$get_tmle_node(delta)  
 
          data <- data[observed,]
          folds <- sl3::subset_folds(tmle_task$folds,which(observed))
          revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = self$V, 
                                       folds = folds)
        }else{
          revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = self$V, 
                                       folds = tmle_task$folds)
        }
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
      tmle_spec <- self$tmle_spec
      likelihood <- self$likelihood
      V <- self$V
      
      type<-tmle_spec$options$type
      maximize<-tmle_spec$options$maximize
      complex<-tmle_spec$options$complex
      realistic<-tmle_spec$options$realistic
      learner_list<-tmle_spec$options$learners

      #Edit the tmle3 task so it avoids missing values:
      if(!is.null(tmle_task$npsem$Y$censoring_node)){
        delta<-tmle_task$npsem$Y$censoring_node$name
        
        #Subset data and nodes:
        observed <- tmle_task$get_tmle_node(delta)  
        data <- tmle_task$get_data()
        data <- data[observed]
        data <- data[,(ncol(data)) := NULL]
        folds <- sl3::subset_folds(tmle_task$folds,which(observed))
        
        #Create node list:
        W<-c(tmle_task$.__enclos_env__$private$.npsem$W$variables)
        A<-tmle_task$.__enclos_env__$private$.npsem$A$variables
        Y<-tmle_task$.__enclos_env__$private$.npsem$Y$variables
        
        node_list<-list(W = W, A = A, Y = Y)
        
        tmle_spec_new <- tmle3_mopttx_blip_revere(
          V = V, type = type,
          learners = learner_list, maximize = maximize,
          complex = complex, realistic = realistic
        )

        tmle_task_noC <-tmle_spec_new$make_tmle_task(data, node_list = node_list, folds)
      }else{
        tmle_task_noC <- tmle_task
      }
      
      blip_revere_task <- sl3:::sl3_revere_Task$new(self$blip_revere_function, tmle_task_noC)
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
    tmle_spec = function(){
      return(private$.tmle_spec)
    },
    likelihood = function() {
      return(private$.likelihood)
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
    .tmle_spec = NULL,
    .likelihood = NULL,
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
