#' Defines a TMLE for the Mean Under the Optimal Individualized Rule with Categorical Treatment
#'
#' @importFrom R6 R6Class
#'
#' @export
#
tmle3_Spec_mopttx_blip <- R6Class(
  classname = "tmle3_Spec_mopttx_blip",
  portable = TRUE,
  class = TRUE,
  inherit = tmle3_Spec,
  public = list(
    initialize = function(V, type, b_learner, maximize = TRUE, complex = TRUE, ...) {
      options <- list(V = V, type = type, b_learner = b_learner, maximize = maximize, complex = complex)
      do.call(super$initialize, options)
    },

    vals_from_factor = function(x) {
      sort(unique(x))
    },

    make_updater = function() {
      updater <- tmle3_cv_Update$new()
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

    make_est_fin = function(fit, p.value = 0.05) {

      # Order estimates:
      summary <- fit$summary
      summary <- summary[order(summary$tmle_est, decreasing = TRUE), ]

      psi <- summary$tmle_est
      se_psi <- summary$se
      n <- length(fit$estimates[[1]]$IC)

      for (i in 1:length(psi)) {
        if (i + 1 <= length(psi)) {
          # Welch's t-test
          t <- (psi[i] - psi[i + 1]) / (sqrt(se_psi[i]^2 + se_psi[i + 1]^2))
          p <- pt(-abs(t), df = n - 1)

          if (p <= p.value) {
            # res<-summary[i,]
            res <- i
            break
          } else if ((i + 1) == length(psi)) {
            # Return the larger static rule:
            # all estimates are non-significantly different.
            names <- summary$param
            stp <- data.frame(data.frame(do.call("rbind", strsplit(as.character(names), "=", fixed = TRUE)))[, 2])
            stp <- data.frame(do.call("rbind", strsplit(as.character(stp[, 1]), "}", fixed = TRUE)))[, 1]

            ind <- min(which(!is.na(suppressWarnings(as.numeric(levels(stp)))[stp]) == TRUE))

            res <- match(summary[ind, ]$param, fit$summary$param)

            # res<-summary[ind,]
          }
        }
      }
      return(res)
    },

    make_params = function(tmle_task, likelihood) {
      V <- private$.options$V
      complex <- private$.options$complex

      # If complex=TRUE, it will return JUST the learned E[Yd]
      if (complex) {
        # Learn the rule
        opt_rule <- Optimal_Rule$new(tmle_task, likelihood, "split-specific",
          V = V, blip_type = private$.options$type,
          blip_library = private$.options$b_learner, maximize = private$.options$maximize
        )

        opt_rule$fit_blip()

        # Define a dynamic Likelihood factor:
        lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
        intervens <- Param_TSM$new(likelihood, lf_rule)
      } else if (!complex) {
        # TO DO: Order covarates in order of importance
        # Right now naively respects the order

        if (length(V) < 2) {
          stop("This is a simple rule, should be run with complex=TRUE.")
        } else {
          V_sub <- self$make_rules(V)

          tsm_rule <- lapply(V_sub, function(v) {
            opt_rule <- Optimal_Rule$new(tmle_task, likelihood, "split-specific",
              V = v, blip_type = private$.options$type,
              blip_library = private$.options$b_learner,
              maximize = private$.options$maximize
            )
            opt_rule$fit_blip()

            # Define a dynamic Likelihood factor:
            lf_rule <- define_lf(LF_rule, "A", rule_fun = opt_rule$rule)
            Param_TSM$new(likelihood, lf_rule)
          })
        }

        # Define a static intervention for each level of A:
        A_vals <- tmle_task$npsem$A$variable_type$levels

        interventions <- lapply(A_vals, function(A_val) {
          intervention <- define_lf(LF_static, "A", value = A_val)
          tsm <- define_param(Param_TSM, likelihood, intervention)
        })

        intervens <- c(tsm_rule, interventions)

        # TO DO: There has to be a better way of doing this
        fit <- fit_tmle3(tmle_task, likelihood, intervens, self$make_updater())
        intervens <- intervens[[self$make_est_fin(fit)]]
      }

      return(intervens)
    }
  ),
  active = list(),
  private = list()
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
#' @param b_learner Library for blip estimation.
#' @param maximize Specify whether we want to maximize or minimize the mean of the final outcome.
#' @param complex If \code{TRUE}, learn the rule using the specified covariates \code{V}. If
#' \code{FALSE}, check if a less complex rule is better.
#'
#' @export
#'

tmle3_mopttx_blip <- function(V, type = "blip1", b_learner, maximize = TRUE, complex = TRUE) {
  tmle3_Spec_mopttx_blip$new(V = V, type = type, b_learner = b_learner, maximize = maximize, complex = complex)
}
