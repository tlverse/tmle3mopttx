#' Treatment Specific Mean with names specifying the covariates the rule depends on.
#'
#' Treatment Specific Mean with names specifying the covariates the rule depends on.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom methods is
#' @importFrom tmle3 Param_base
#'
#' @export
#'
#' @keywords data
#'
#' @return TSP with name of the intervention specified.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link[tmle3]{Param_base}}.
Param_TSM_name <- R6Class(
  classname = "Param_TSM_name",
  portable = TRUE,
  class = TRUE,
  lock_objects = FALSE,
  inherit = tmle3::Param_base,
  public = list(
    initialize = function(observed_likelihood, intervention_list, v=NULL, ..., outcome_node = "Y") {
      super$initialize(observed_likelihood, ..., outcome_node = outcome_node)
      private$.v <- v

      if(!is.null(observed_likelihood$censoring_nodes[[outcome_node]])){
        # add delta_Y=0 to intervention list
        outcome_censoring_node <- observed_likelihood$censoring_nodes[[outcome_node]]
        censoring_intervention <- define_lf(LF_static, outcome_censoring_node, value = 1)
        intervention_list <- c(intervention_list, censoring_intervention)  
      }

      private$.cf_likelihood <- make_CF_Likelihood(observed_likelihood, intervention_list)
    },
    clever_covariates = function(tmle_task = NULL, fold_number = "full") {
      if (is.null(tmle_task)) {
        tmle_task <- self$observed_likelihood$training_task
      }
      intervention_nodes <- names(self$intervention_list)
      pA <- self$observed_likelihood$get_likelihoods(tmle_task, intervention_nodes, fold_number)
      cf_pA <- self$cf_likelihood$get_likelihoods(tmle_task, intervention_nodes, fold_number)

      HA <- cf_pA / pA

      # collapse across multiple intervention nodes
      if (!is.null(ncol(HA)) && ncol(HA) > 1) {
        HA <- apply(HA, 1, prod)
      }

      HA <- bound(HA, c(-40,40))

      return(list(Y = unlist(HA, use.names = FALSE)))
    },
    estimates = function(tmle_task = NULL, fold_number = "full") {
      if (is.null(tmle_task)) {
        tmle_task <- self$observed_likelihood$training_task
      }

      # todo: extend for stochastic
      cf_task <- self$cf_likelihood$enumerate_cf_tasks(tmle_task)[[1]]

      # cf_task <- self$cf_likelihood$cf_tasks[[1]]

      Y <- tmle_task$get_tmle_node(self$outcome_node, impute_censoring=TRUE)

      # clever_covariates happen here (for this param) only, but this is repeated computation
      HA <- self$clever_covariates(tmle_task, fold_number)[[self$outcome_node]]

      # clever_covariates happen here (for all fit params), and this is repeated computation
      EYA <- unlist(self$observed_likelihood$get_likelihood(tmle_task, self$outcome_node, fold_number), use.names = FALSE)

      # clever_covariates happen here (for all fit params), and this is repeated computation
      EY1 <- unlist(self$cf_likelihood$get_likelihood(cf_task, self$outcome_node, fold_number), use.names = FALSE)

      # todo: separate out psi
      # todo: make this a function of f(W)
      psi <- mean(EY1)
      IC <- HA * (Y - EYA) + EY1 - psi
      result <- list(psi = psi, IC = IC)
      return(result)
    }
  ),
  active = list(
    name = function() {
      param_form <- sprintf(
        "E[%s_{%s}]", self$outcome_node,
        paste0("d(V=", paste(private$.v, collapse = ","), ")")
      )
      return(param_form)
    },
    cf_likelihood = function() {
      return(private$.cf_likelihood)
    },
    intervention_list = function() {
      return(self$cf_likelihood$intervention_list)
    },
    update_nodes = function() {
      return(self$outcome_node)
    }
  ),
  private = list(
    .type = "TSM",
    .cf_likelihood = NULL,
    .supports_outcome_censoring = TRUE
  )
)
