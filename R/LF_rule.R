#' Dynamic Likelihood Factor
#'
#' Dynamic Likelihood Factor built on top of \code{\link[tmle3]{LF_base}}.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom uuid UUIDgenerate
#' @importFrom methods is
#' @importFrom tmle3 LF_base
#'
#' @export
#'
#' @keywords data
#'
#' @return \code{\link[tmle3]{LF_base}} object.
#'
#' @format An \code{\link[R6]{R6Class}} object inheriting from
#'  \code{\link{LF_base}}.
LF_rule <- R6Class(
  classname = "LF_rule", portable = TRUE, class = TRUE,
  inherit = tmle3::LF_base, public = list(
    initialize = function(name,
                          type = "density", rule_fun, ...) {
      super$initialize(name, ..., type = type)
      private$.rule_fun <- rule_fun
    },

    get_mean = function(tmle_task, fold_number) {
      return(self$rule_fun(tmle_task, fold_number))
    },

    get_density = function(tmle_task, fold_number) {
      observed <- tmle_task$get_tmle_node(self$name)
      likelihood <- as.numeric(self$rule_fun(tmle_task,
                                             fold_number) == observed)

      return(likelihood)
    },

    cf_values = function(tmle_task) {
      # todo: think carefully about this for data adaptive parameters
      cf_values <- self$rule_fun(tmle_task, "validation")
      return(cf_values)
    }
  ),

  active = list(rule_fun = function() {
    return(private$.rule_fun)
  }),

  private = list(.name = NULL, .rule_fun = NULL, .is_degenerate = TRUE)
)
