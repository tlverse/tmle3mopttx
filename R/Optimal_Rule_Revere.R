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
#'
#' @section Parameters:
#'   - \code{tmle_task}: Task object of \code{\link[keras]{tmle3}} specifying the data and
#'   node structure.
#'   - \code{tmle_spec}: Spec object of \code{\link[keras]{tmle3}}. Allows for different
#'   Specs to use the current class for learning the Optimal Rule.
#'   - \code{likelihood}: Likelihood object of \code{\link[keras]{tmle3}}, corresponding
#'   to the current estimate of the required parts of the likelihood necessary for the target
#'   parameter.
#'   - \code{V}: User-specified list of covariates used to define the rule.
#'   - \code{blip_type}: Blip type, corresponding to different ways of defining the
#'   reference category in learning the blip; mostly applies to categorical treatment.
#'   Available categories include "blip1" (reference level of treatment), "blip2"
#'   (average level of treatment) and "blip3" (weighted average level of treatment).
#'   - \code{learners}: List of user-defined learners for relevant parts of the
#'   likelihood.
#'   - \code{maximize}: Should the average outcome be maximized of minimized? Default is
#'   maximize=TRUE.
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
    initialize = function(tmle_task, tmle_spec, likelihood, V = NULL,
                          blip_type = "blip2", learners, maximize = TRUE,
                          shift_grid = seq(-1, 1, by = 0.5)) {
      private$.tmle_task <- tmle_task
      private$.tmle_spec <- tmle_spec
      private$.likelihood <- likelihood
      private$.blip_type <- blip_type
      private$.learners <- learners
      private$.maximize <- maximize
      private$.realistic <- tmle_spec$options$realistic
      private$.resource <- tmle_spec$options$resource
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

      # If there are missing values in Y,
      # Blip outcomes will have missing values as well
      if (blip_type == "blip1") {
        blip <- DR[, 2] - DR[, 1]
      } else if (blip_type == "blip2") {
        blip <- DR - rowMeans(DR)
      } else if (blip_type == "blip3") {
        blip <- DR - (rowMeans(DR) * g_vals)
      }

      # TO DO: Nicer solutions. Do it one by one, for now
      # If there are missing Ys, there will be missing blips- for now drop these rows.
      # (otherwise we train on imputed values... )
      if (is.null(V)) {
        data <- data.table(V = blip, blip = blip)
        outcomes <- grep("blip", names(data), value = TRUE)
        V <- grep("V", names(data), value = TRUE)

        revere_task <- make_sl3_Task(data,
          outcome = outcomes, covariates = V,
          folds = tmle_task$folds
        )

        # Drop censored values:
        # if(!is.null(tmle_task$npsem$Y$censoring_node)){
        #  delta<-tmle_task$npsem$Y$censoring_node$name
        #  observed <- tmle_task$get_tmle_node(delta)
        #
        #  data <- data[observed,]
        #  folds <- sl3::subset_folds(tmle_task$folds,which(observed))
        #
        #  revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = V,
        #                               folds = folds)
        # }else{
        #  revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = V,
        #                               folds = tmle_task$folds)
        # }
      } else {
        V <- tmle_task$data[, self$V, with = FALSE]
        data <- data.table(V, blip = blip)
        outcomes <- grep("blip", names(data), value = TRUE)

        revere_task <- make_sl3_Task(data,
          outcome = outcomes, covariates = self$V,
          folds = tmle_task$folds
        )

        # Drop censored values:
        # if(!is.null(tmle_task$npsem$Y$censoring_node)){
        #  delta<-tmle_task$npsem$Y$censoring_node$name
        #  observed <- tmle_task$get_tmle_node(delta)
        #
        #  data <- data[observed,]
        #  folds <- sl3::subset_folds(tmle_task$folds,which(observed))
        #  revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = self$V,
        #                               folds = folds)
        # }else{
        #  revere_task <- make_sl3_Task(data, outcome = outcomes, covariates = self$V,
        #                               folds = tmle_task$folds)
        # }
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

      type <- tmle_spec$options$type
      maximize <- tmle_spec$options$maximize
      complex <- tmle_spec$options$complex
      realistic <- tmle_spec$options$realistic
      resource <- tmle_spec$options$resource
      learner_list <- tmle_spec$options$learners
      
      # Edit the tmle3 task so it avoids missing values:
      if (!is.null(tmle_task$npsem$Y$censoring_node)) {
        delta <- tmle_task$npsem$Y$censoring_node$name

        # Subset data and nodes:
        observed <- tmle_task$get_tmle_node(delta)
        data <- tmle_task$get_data()
        data <- data[observed]
        data <- data[, (ncol(data)) := NULL]
        folds <- sl3::subset_folds(tmle_task$folds, which(observed))

        # Create node list:
        W <- c(tmle_task$.__enclos_env__$private$.npsem$W$variables)
        A <- tmle_task$.__enclos_env__$private$.npsem$A$variables
        Y <- tmle_task$.__enclos_env__$private$.npsem$Y$variables

        node_list <- list(W = W, A = A, Y = Y)

        tmle_spec_new <- tmle3_mopttx_blip_revere(
          V = V, type = type,
          learners = learner_list, maximize = maximize,
          complex = complex, realistic = realistic, resource = resource
        )

        tmle_task_noC <- tmle_spec_new$make_tmle_task(data, node_list = node_list, folds)
      } else {
        tmle_task_noC <- tmle_task
      }

      blip_revere_task <- sl3:::sl3_revere_Task$new(self$blip_revere_function, tmle_task_noC)
      blip_fit <- self$blip_library$train(blip_revere_task)
      private$.blip_fit <- blip_fit
    },

    rule = function(tmle_task, fold_number = "full") {
      realistic <- private$.realistic
      resource <- private$.resource
      likelihood <- self$likelihood

      # TODO: when applying the rule, we actually only need the covariates
      ### NOTE:
      # If there is missing outcome, this will return rules for ALL values.
      # This is ok- we don't have missing Ws or As, just Ys (hence, we can get a predicted value).
      # This outputs a warning, but that's ok.
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
      
      #Allow resource constrain only on binary treatment for now
      if(length(A_vals) == 2){
        #TO DO: Note that this doesn't really allow us to rank blip < 0
        max_preds <-apply(blip_preds, 1, max) 
        rank_df <- data.table("id" = c(1:length(max_preds)), 
                              "blip_preds" = max_preds)
        rank_df <- rank_df[order(rank_df[,2],decreasing=TRUE),]
        self$.rank <- rank_df
        
        #Total to get treatment:
        A1 <- sum(rank_df$blip_preds>0)
        A1_constrain <- floor(A1 * resource)
        
        get_A_id <- rank_df[1:A1_constrain, "id"]
        get_A_id <- get_A_id$id
        
        rank_df <- rank_df[order(rank_df[,1],decreasing=FALSE),]
        
        rule_preds_resource <- rule_preds
        rule_preds_resource[!(rank_df$id %in% get_A_id)] <- min(A_vals[rule_preds])
      }else{
        rule_preds_resource <- rule_preds
      }
      
    
      return(rule_preds_resource)
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
    tmle_spec = function() {
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
    },
    return_rank = function() {
      return(private$.rank)
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
    .resource = NULL,
    .shift_grid = NULL,
    .opt_delta = NULL,
    .opt_A = NULL,
    .Q_vals = NULL,
    .rank = NULL
  )
)
