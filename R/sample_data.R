#' Mock data set with Binary Treatment
#'
#' A dataset with a simple data structure O = (A, Y, W), where exposure (A) and outcome (Y) are
#' binary. This is a simple dataset designed specifically to illustrate the TMLE estimation procedure.
#'
#' @format A \code{data.frame} with 5 columns.
#' \describe{
#'   \item{Y}{A binary variable representing an outcome of interest.}
#'   \item{A}{A binary variable representing an intervention of interest.}
#'   \item{W1}{A continuous variable representing a covariate of interest.}
#'   \item{W2}{A continuous variable representing a covariate of interest.}
#'   \item{W3}{A continuous variable representing a covariate of interest.}
#' }

"data_bin"

#' Mock data set with Categorical Treatment
#'
#' A dataset with a simple data structure O = (A, Y, W), where outcome (Y) is
#' binary and treatment (A) is categorical.
#' This is a simple dataset designed specifically to illustrate the TMLE estimation procedure.
#'
#' @format A \code{data.frame} with 6 columns.
#' \describe{
#'   \item{Y}{A binary variable representing an outcome of interest.}
#'   \item{A}{A binary variable representing an intervention of interest.}
#'   \item{W1}{A continuous variable representing a covariate of interest.}
#'   \item{W2}{A continuous variable representing a covariate of interest.}
#'   \item{W3}{A continuous variable representing a covariate of interest.}
#'   \item{W4}{A continuous variable representing a covariate of interest.}
#' }

"data_cat"