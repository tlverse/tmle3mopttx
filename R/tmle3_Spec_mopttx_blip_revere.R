#' TMLE for the Mean Under the Optimal Individualized Rule
#'
#' The functions contained in the class define a TMLE for the Mean Under
#' the Optimal Individualized Rule with Categorical Treatment, learned and estimated
#' under Revere CV-TMLE. For learning the Optimal Rule, see 'Optimal_Rule_Revere' class.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom tmle3 tmle3_Spec
#'
#' @export
#'
#' @keywords data
#'
#' @return A \code{tmle3} object inheriting from \code{\link[tmle3]{tmle3_Spec}} with
#' methods for obtaining the TMLE for the Mean Under the Optimal Individualized Rule.
#' For a full list of the available  functionality, see the complete documentation of
#' \code{\link[tmle3]{tmle3_Spec}}.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link[tmle3]{tmle3_Spec}}.
#'
#' @section Parameters:
#'   - \code{V}: User-specified list of covariates used to define the rule.
#'   - \code{type}: Blip type, corresponding to different ways of defining the
#'   reference category in learning the blip; mostly applies to categorical treatment.
#'   Available categories include "blip1" (reference level of treatment), "blip2"
#'   (average level of treatment) and "blip3" (weighted average level of treatment).
#'   - \code{learners}: List of user-defined learners for relevant parts of the
#'   likelihood.
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
#'   - \code{interpret}: If \code{TRUE}, returns a HAL fit of the blip, explaining the rule.
#'   - \code{reference}: reference category for blip1. Default is the smallest numerical category or factor.
#'   
#' @examples
#' \dontrun{
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
#'}
tmle3_Spec_mopttx_blip_revere <- R6Class(
  classname = "tmle3_Spec_mopttx_blip_revere",
  portable = TRUE,
  class = TRUE,
  lock_objects = FALSE,
  inherit = tmle3::tmle3_Spec,
  public = list(
    initialize = function(V = NULL, type, learners, maximize = TRUE, complex = TRUE,
                          realistic = FALSE, resource = 1, interpret = FALSE, 
                          likelihood_override=NULL, reference=NULL, ...) {
      options <- list(
        V = V, type = type, learners = learners, maximize = maximize, complex = complex,
        realistic = realistic, resource = resource, interpret=interpret, 
        likelihood_override=likelihood_override, reference=reference, ...
      )
      do.call(super$initialize, options)
    },
    
    vals_from_factor = function(x) {
      sort(unique(x))
    },
    
    #Edited to support known likelihoods
    make_initial_likelihood = function(tmle_task, learner_list = NULL) {
      # produce trained likelihood when likelihood_def provided
      
      if (!is.null(self$options$likelihood_override)) {
        likelihood <- self$options$likelihood_override$train(tmle_task)
      } else {
        likelihood <- point_tx_likelihood(tmle_task, learner_list)
      }
      
      return(likelihood)
    },
    
    ### Function for predicting the rule for a new dataset
    ### (blip function learned previously with an old dataset)
    
    # The user specifies a new tmle_task, that should be same as the
    # original (except now with new data).
    # tmle_task MUST have the same column names as the original dataset!
    predict_rule = function(tmle_task_new) {
      
      # Grab the same configuration as the original problem:
      realistic <- self$options$realistic
      complex <- self$options$complex
      learner_list <- self$options$learners
      
      # Calculate g for the new dataset:
      likelihood <- self$make_initial_likelihood(tmle_task_new, learner_list)
      
      # Grab the opt object:
      opt <- private$.opt
      
      # Save the rule for each individual:
      # TO DO: potentially an issue with realistic rule
      # (I think it fetches the old likelihood)
      rule_preds <- opt$rule(tmle_task_new, "full")
      
      # Define new updater and targeted likelihood
      updater_new <- self$make_updater()
      targeted_likelihood_new <- self$make_targeted_likelihood(likelihood, updater_new)
      
      # TO DO: Develop with for complex rule
      lf_rule <- define_lf(LF_rule, "A", rule_fun = opt$rule)
      intervens <- Param_TSM$new(targeted_likelihood_new, lf_rule)
      updater_new$tmle_params <- intervens
      
      fit <- fit_tmle3(
        tmle_task_new, targeted_likelihood_new, intervens,
        updater_new
      )
      
      return(list(rule = rule_preds, tmle_fit = fit))
    },
    
    make_rules = function(V) {
      
      # TO DO: Add a variable importance here;
      # right now it naively respects the ordering
      
      # TO DO: Alternativly re-code this
      V_sub <- list()
      V_sub <- c(list(V))
      
      for (i in 2:length(V)) {
        V_sub <- c(V_sub, list(V[-1]))
        V <- V[-1]
      }
      
      return(V_sub)
    },
    
    make_est_fin = function(fit, max, p.value = 0.35) {
      
      # Goal: pick the simplest rule, that is significant
      summary_all <- fit$summary
      
      # Separate static rules:
      lev <- length(fit$tmle_task$npsem$A$variable_type$levels)
      summary_static <- summary_all[((nrow(summary_all) - lev + 1):nrow(summary_all)), ]
      
      if (max) {
        summary_static <- summary_static[order(summary_static$tmle_est, decreasing = TRUE), ]
      } else {
        summary_static <- summary_static[order(summary_static$tmle_est, decreasing = FALSE), ]
      }
      
      summary <- summary_all[(1:(nrow(summary_all) - lev)), ]
      summary <- rbind.data.frame(summary, summary_static)
      
      psi <- summary$tmle_est
      se_psi <- summary$se
      n <- length(fit$estimates[[1]]$IC)
      
      for (i in 1:length(psi)) {
        if (i + 1 <= length(psi)) {
          # Welch's t-test
          t <- (psi[i] - psi[i + 1]) / (sqrt(se_psi[i]^2 + se_psi[i + 1]^2))
          p <- pt(-abs(t), df = n - 1)
          
          if (p <= p.value) {
            # res <- summary[i, ]
            res <- i
            break
          } else if ((i + 1) == length(psi)) {
            # all estimates are non-significantly different.
            # names <- summary$param
            # stp <- data.frame(data.frame(do.call("rbind", strsplit(as.character(names), "=", fixed = TRUE)))[, 2])
            # stp <- data.frame(do.call("rbind", strsplit(as.character(stp[, 1]), "}", fixed = TRUE)))[, 1]
            # ind <- min(which(!is.na(suppressWarnings(as.numeric(levels(stp)))[stp]) == TRUE))
            # res <- match(summary[ind, ]$param, fit$summary$param)
            
            # Return the better static rule:
            # res <- summary_static[1, ]
            res <- length(psi) - lev + 1
          }
        }
      }
      return(res)
    },
    
    set_opt = function(opt) {
      private$.opt <- opt
    },
    
    set_rule = function(rule) {
      private$.rule <- rule
    },
    
    ### Simulation specific: Returns data-adaptive truth.
    # Takes as input: data set with large n, node list, and true Q function.
    data_adapt_psi = function(data_tda, node_list, Qbar0) {
      opt <- self$return_rule()
      tda_task <- self$make_tmle_task(data = data_tda, node_list = node_list)
      tda_tx <- opt$rule(tda_task, "full")
      
      tda_W <- tda_task$get_tmle_node("W")
      Edn <- mean(Qbar0(tda_tx, as.matrix(tda_W)))
      
      return(Edn = Edn)
    },
    
    get_blip_fit = function(){
      blip = private$.opt
      blip_fit <- blip$blip_fit
      return(blip_fit)
    },
    
    get_blip_pred = function(tmle_task, fold_number = "full") {
      blip = private$.opt
      blip_task <- blip$blip_revere_function(tmle_task, fold_number=fold_number)
      blip_preds <- blip$blip_fit$predict_fold(blip_task, fold_number)
      return(blip_preds)
    },
    
    make_params = function(tmle_task, likelihood) {
      
      #Grab all parameters:
      V <- private$.options$V
      complex <- private$.options$complex
      max <- private$.options$maximize
      realistic <- private$.options$realistic
      resource <- private$.options$resource
      interpret <- private$.options$interpret
      likelihood_override <- private$.options$likelihood_override
      
      # If complex=TRUE, it will return JUST the learned E[Yd]
      if (complex) {
        # Learn the rule
        opt_rule <- Optimal_Rule_Revere$new(tmle_task,
                                            tmle_spec = self, likelihood$initial_likelihood,
                                            V =  V, options = private$.options
        )
        
        opt_rule$fit_blip()
        self$set_opt(opt_rule)
        
        # Save interpretable rule, if fit
        private$.blip_fit_interpret <- opt_rule$blip_fit_interpret
        
        # Save the rule for each individual:
        self$set_rule(opt_rule$rule(tmle_task, "validation"))
        
        # Define a dynamic Likelihood factor:
        lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
        
        if (is.null(V)) {
          intervens <- Param_TSM$new(likelihood, lf_rule)
        } else {
          #intervens <- Param_TSM_name$new(likelihood, v = V, lf_rule)
          intervens <- Param_TSM$new(likelihood, lf_rule)
        }
      } else if (!complex) {
        # TO DO: Order covarates in order of importance
        # Right now naively respects the order
        
        if (realistic) {
          stop("At the moment less complex rules can only be estimated for true (possibly not
               realistic) interventions. Check back as the package matures!")
        } else {
          if (length(V) < 2) {
            stop("This is a simple rule, should be run with complex=TRUE.")
          } else {
            upd <- self$make_updater()
            targ_likelihood <- Targeted_Likelihood$new(likelihood$initial_likelihood, upd)
            
            V_sub <- self$make_rules(V)
            
            tsm_rule <- lapply(V_sub, function(v) {
              opt_rule <- Optimal_Rule_Revere$new(tmle_task,
                                                  tmle_spec = self,
                                                  likelihood$initial_likelihood,
                                                  V = v,
                                                  options = private$.options
              )
              
              opt_rule$fit_blip()
              self$set_opt(opt_rule)
              
              # Save the rule for each individual:
              self$set_rule(opt_rule$rule(tmle_task, "validation"))
              
              # Define a dynamic Likelihood factor:
              lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
              Param_TSM_name$new(targ_likelihood, v = v, lf_rule)
              #Param_TSM$new(targ_likelihood, lf_rule)
            })
            
            # Define a static intervention for each level of A:
            A_vals <- tmle_task$npsem$A$variable_type$levels
            
            interventions <- lapply(A_vals, function(A_val) {
              intervention <- define_lf(LF_static, "A", value = A_val)
              tsm <- define_param(Param_TSM_name, targ_likelihood, v = A_val, intervention)
              #tsm <- define_param(Param_TSM, targ_likelihood, intervention)
            })
            
            intervens <- c(tsm_rule, interventions)
            upd$tmle_params <- intervens
            
            fit <- fit_tmle3(tmle_task, targ_likelihood, intervens, upd)
            ind <- self$make_est_fin(fit, max = max)
            best_interven <- intervens[[ind]]
            
            lev <- tmle_task$npsem$A$variable_type$levels
            V_sub_all <- c(V_sub, lev)
            V_sub_all[[self$make_est_fin(fit, max = max)]]
            
            intervens <- define_param(Param_TSM_name, likelihood,
                                      intervention_list = best_interven$intervention_list,
                                      v = V_sub_all[[ind]])
            #intervens <- define_param(Param_TSM, likelihood,
            #                          intervention_list = best_interven$intervention_list)
          }
        }
      }
      return(intervens)
    }
  ),
  active = list(
    return_opt = function() {
      return(private$.opt)
    },
    return_rule = function() {
      return(private$.rule)
    },
    blip_fit_interpret = function() {
      return(summary(private$.blip_fit_interpret)$table)
    }
  ),
  private = list(
    .opt = list(),
    .rule = NULL,
    .blip_fit_interpret = NULL
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
#'  corresponds to chosing a reference category, and defining the blip for all other categories relative to the
#'  specified reference. Note that in the case of binary treatment, "blip1" is just the usual blip.
#'  "Blip2$ corresponds to defining the blip relative to the average of all categories. Finally,
#'  "Blip3" corresponds to defining the blip relative to the weighted average of all categories.
#' @param learners Library for Y (outcome), A (treatment), and B (blip) estimation.
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#' @param complex If \code{TRUE}, learn the rule using the specified covariates \code{V}. If
#'  \code{FALSE}, check if a less complex rule is better.
#' @param realistic If \code{TRUE}, it will return a rule what is possible due to practical positivity constraints.
#' @param resource Indicates the percent of initially estimated individuals who should be given treatment that
#'  get treatment, based on their blip estimate. If resource = 1 all estimated individuals to benefit from
#'  treatment get treatment, if resource = 0 none get treatment.
#' @param interpret If \code{TRUE}, returns a HAL fit of the blip, explaining the rule.  
#' @param likelihood_override if estimates of the likelihood are known, override learners.
#' @param reference reference category for blip1. Default is the smallest numerical category or factor.
#'
#' @export
tmle3_mopttx_blip_revere <- function(V = NULL, type = "blip1", learners, maximize = TRUE,
                                     complex = TRUE, realistic = FALSE, resource = 1, 
                                     interpret = FALSE, likelihood_override=NULL, reference=NULL) {
  tmle3_Spec_mopttx_blip_revere$new(
    V = V, type = type, learners = learners,
    maximize = maximize, complex = complex, realistic = realistic, 
    resource = resource, interpret = interpret, reference=reference,
    likelihood_override=likelihood_override
  )
}