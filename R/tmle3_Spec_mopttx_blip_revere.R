#' Defines a TMLE for the Mean Under the Optimal Individualized Rule with
#' Categorical Treatment under Revere CV-TMLE.
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_mopttx_blip_revere <- R6Class(
  classname = "tmle3_Spec_mopttx_blip_revere",
  portable = TRUE,
  class = TRUE,
  lock_objects = FALSE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(V = NULL, type, learners, maximize = TRUE, complex = TRUE, realistic = FALSE, ...) {
      options <- list(
        V = V, type = type, learners = learners, maximize = maximize, complex = complex,
        realistic = realistic, ...
      )
      do.call(super$initialize, options)
    },

    vals_from_factor = function(x) {
      sort(unique(x))
    },

    # make_updater = function() {
    #   updater <- tmle3_cv_Update$new()
    # },

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

    make_params = function(tmle_task, likelihood) {
      V <- private$.options$V
      complex <- private$.options$complex
      max <- private$.options$maximize
      realistic <- private$.options$realistic

      # If complex=TRUE, it will return JUST the learned E[Yd]
      if (complex) {
        # Learn the rule
        opt_rule <- Optimal_Rule_Revere$new(tmle_task, likelihood$initial_likelihood, "split-specific",
          V = V, blip_type = private$.options$type,
          learners = private$.options$learners,
          maximize = private$.options$maximize,
          realistic = realistic
        )

        opt_rule$fit_blip()
        self$set_opt(opt_rule)

        # Save the rule for each individual:
        self$set_rule(opt_rule$rule(tmle_task, "validation"))

        # Define a dynamic Likelihood factor:
        lf_rule <- define_lf(LF_rule, "A", rule_fun = function(task) {
          opt_rule$rule(task, "validation")
        })
        intervens <- Param_TSM$new(likelihood, lf_rule)
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
              opt_rule <- Optimal_Rule_Revere$new(tmle_task, likelihood$initial_likelihood, "split-specific",
                V = v, blip_type = private$.options$type,
                learners = private$.options$learners,
                maximize = private$.options$maximize,
                realistic = realistic
              )

              opt_rule$fit_blip()
              self$set_opt(opt_rule)

              # Save the rule for each individual:
              self$set_rule(opt_rule$rule(tmle_task, "validation"))

              # Define a dynamic Likelihood factor:
              lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
              Param_TSM2$new(targ_likelihood, v = v, lf_rule)
            })

            # Define a static intervention for each level of A:
            A_vals <- tmle_task$npsem$A$variable_type$levels

            interventions <- lapply(A_vals, function(A_val) {
              intervention <- define_lf(LF_static, "A", value = A_val)
              tsm <- define_param(Param_TSM, targ_likelihood, intervention)
            })

            intervens <- c(tsm_rule, interventions)
            upd$tmle_params <- intervens

            fit <- fit_tmle3(tmle_task, targ_likelihood, intervens, upd)
            ind <- self$make_est_fin(fit, max = max)
            best_interven <- intervens[[ind]]

            lev <- tmle_task$npsem$A$variable_type$levels
            V_sub_all <- c(V_sub, lev)
            V_sub_all[[self$make_est_fin(fit, max = max)]]

            intervens <- define_param(Param_TSM2, likelihood,
              intervention_list = best_interven$intervention_list,
              v = V_sub_all[[ind]]
            )
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
    }
  ),
  private = list(
    .opt = list(),
    .rule = NULL
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
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#' @param complex If \code{TRUE}, learn the rule using the specified covariates \code{V}. If
#' \code{FALSE}, check if a less complex rule is better.
#' @param realistic If \code{TRUE}, it will return a rule what is possible due to practical positivity constraints.
#'
#' @export
#'

tmle3_mopttx_blip_revere <- function(V = NULL, type = "blip1", learners, maximize = TRUE,
                                     complex = TRUE, realistic = FALSE) {
  tmle3_Spec_mopttx_blip_revere$new(
    V = V, type = type, learners = learners,
    maximize = maximize, complex = complex, realistic = realistic
  )
}
