% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Optimal_Rule_Q_learning.R
\docType{data}
\name{Optimal_Rule_Q_learning}
\alias{Optimal_Rule_Q_learning}
\title{Learns the Optimal Rule given a tmle_task and likelihood, using Q learning.}
\format{An object of class \code{R6ClassGenerator} of length 24.}
\usage{
Optimal_Rule_Q_learning
}
\arguments{
\item{tmle_task}{\code{tmle3} object, makes the tmle task.}

\item{likelihood}{\code{Targeted_Likelihood} object, contains estimates of all relevant parts of the likelihood.}

\item{maximize}{Specify whether we want to maximize or minimize the mean of the final outcome.}
}
\description{
Learns the Optimal Rule given a tmle_task and likelihood, using Q learning.
}
\keyword{datasets}