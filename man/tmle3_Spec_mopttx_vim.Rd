% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmle3_Spec_mopttx_vim.R
\docType{class}
\name{tmle3_Spec_mopttx_vim}
\alias{tmle3_Spec_mopttx_vim}
\title{Variable Importance with the Mean Under the Optimal Individualized Rule}
\format{
An \code{\link[R6]{R6Class}} object inheriting from
 \code{\link[tmle3]{tmle3_Spec}}.
}
\value{
A tmle3 object inheriting from \code{\link[tmle3]{tmle3_Spec}} with
methods for obtaining the Variable Importance metric for the TMLE of the
Mean Under the Optimal Individualized Rule. For a full list of the available
functionality, see the complete documentation of \code{\link[tmle3]{tmle3_Spec}}.
}
\description{
The functions contained in the class define a Variable Importance
metric for the TMLE of the Mean Under the Optimal Individualized Rule with
Categorical Treatment, learned and estimated under Revere CV-TMLE. For learning
the Optimal Rule, see 'Optimal_Rule_Revere' class.
}
\section{Parameters}{

  - \code{V}: User-specified list of covariates used to define the rule.
  - \code{type}: Blip type, corresponding to different ways of defining the
  reference category in learning the blip; mostly applies to categorical treatment.
  Available categories include "blip1" (reference level of treatment), "blip2"
  (average level of treatment) and "blip3" (weighted average level of treatment).
  - \code{method}: Either "SL" (for the TMLE estimate) or "Q" (for Q-learning).
  - \code{learners}: List of user-defined learners for relevant parts of the
  likelihood.
  - \code{contrast}: Defined either a "linear" or "multiplicative" contrast for the delta method.
  - \code{maximize}: Should the average outcome be maximized of minimized? Default is
  maximize=TRUE.
  - \code{complex}: If \code{TRUE}, the returned mean under the Optimal Rule is based on the
  full set of covariates provided by the user (parameter "V"). If \code{FALSE}, simpler rules
  (including the static rules), are evaluated as well; the returned mean under the Optimal
  Rule is then a potentially more parsimonious rule, if the mean performance is similar.
  - \code{realistic}: If \code{TRUE}, the optimal rule returned takes into account the
  probability of treatment given covariates.
  - \code{resource}: Indicates the percent of initially estimated individuals who should be given 
  treatment that get treatment, based on their blip estimate. If resource = 1 all estimated 
  individuals to benefit from treatment get treatment, if resource = 0 none get treatment.
}

\examples{
\dontrun{
library(sl3)
library(tmle3)
library(data.table)

data("data_cat_vim")
data <- data_cat_vim
data$A <- as.integer(data$A)

lrn1 <- Lrnr_mean$new()
lrn2 <- Lrnr_glm_fast$new()
lrn3 <- Lrnr_glmnet$new()

Q_learner <- Lrnr_sl$new(learners = list(lrn1, lrn2, lrn3),
metalearner = Lrnr_nnls$new()
)

mn_metalearner <- make_learner(Lrnr_solnp,
loss_function = loss_loglik_multinomial,
learner_function = metalearner_linear_multinomial
)
g_learner <- make_learner(Lrnr_sl, list(lrn1, lrn3), 
mn_metalearner)

b_learner <- create_mv_learners(learners = list(lrn1, lrn2))

learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)

node_list <- list(W = c("W2", "W3", "W4"), 
A = c("A", "W1"), Y = "Y")

tmle_spec <- tmle3_mopttx_vim(
V = "W3", learners = learner_list, type = "blip2",
contrast = "multiplicative", maximize = FALSE,
method = "SL", complex = TRUE, realistic = FALSE
)
}
}
\keyword{data}
\section{Super classes}{
\code{\link[tmle3:tmle3_Spec]{tmle3::tmle3_Spec}} -> \code{\link[tmle3mopttx:tmle3_Spec_mopttx_blip_revere]{tmle3mopttx::tmle3_Spec_mopttx_blip_revere}} -> \code{tmle3_Spec_mopttx_vim}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-tmle3_Spec_mopttx_vim-new}{\code{tmle3_Spec_mopttx_vim$new()}}
\item \href{#method-tmle3_Spec_mopttx_vim-vals_from_factor}{\code{tmle3_Spec_mopttx_vim$vals_from_factor()}}
\item \href{#method-tmle3_Spec_mopttx_vim-make_tmle_task}{\code{tmle3_Spec_mopttx_vim$make_tmle_task()}}
\item \href{#method-tmle3_Spec_mopttx_vim-set_opt}{\code{tmle3_Spec_mopttx_vim$set_opt()}}
\item \href{#method-tmle3_Spec_mopttx_vim-make_params}{\code{tmle3_Spec_mopttx_vim$make_params()}}
\item \href{#method-tmle3_Spec_mopttx_vim-clone}{\code{tmle3_Spec_mopttx_vim$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="tmle3" data-topic="tmle3_Spec" data-id="make_targeted_likelihood"><a href='../../tmle3/html/tmle3_Spec.html#method-tmle3_Spec-make_targeted_likelihood'><code>tmle3::tmle3_Spec$make_targeted_likelihood()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3" data-topic="tmle3_Spec" data-id="make_updater"><a href='../../tmle3/html/tmle3_Spec.html#method-tmle3_Spec-make_updater'><code>tmle3::tmle3_Spec$make_updater()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="data_adapt_psi"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-data_adapt_psi'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$data_adapt_psi()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="get_blip_fit"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-get_blip_fit'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$get_blip_fit()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="get_blip_pred"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-get_blip_pred'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$get_blip_pred()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="make_est_fin"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-make_est_fin'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$make_est_fin()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="make_initial_likelihood"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-make_initial_likelihood'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$make_initial_likelihood()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="make_rules"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-make_rules'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$make_rules()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="predict_rule"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-predict_rule'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$predict_rule()</code></a></li>
<li><span class="pkg-link" data-pkg="tmle3mopttx" data-topic="tmle3_Spec_mopttx_blip_revere" data-id="set_rule"><a href='../../tmle3mopttx/html/tmle3_Spec_mopttx_blip_revere.html#method-tmle3_Spec_mopttx_blip_revere-set_rule'><code>tmle3mopttx::tmle3_Spec_mopttx_blip_revere$set_rule()</code></a></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tmle3_Spec_mopttx_vim-new"></a>}}
\if{latex}{\out{\hypertarget{method-tmle3_Spec_mopttx_vim-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tmle3_Spec_mopttx_vim$new(
  V = NULL,
  type = "blip2",
  method = "SL",
  learners = NULL,
  contrast = "linear",
  maximize = TRUE,
  complex = TRUE,
  realistic = FALSE,
  resource = 1,
  reference = NULL,
  ...
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tmle3_Spec_mopttx_vim-vals_from_factor"></a>}}
\if{latex}{\out{\hypertarget{method-tmle3_Spec_mopttx_vim-vals_from_factor}{}}}
\subsection{Method \code{vals_from_factor()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tmle3_Spec_mopttx_vim$vals_from_factor(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tmle3_Spec_mopttx_vim-make_tmle_task"></a>}}
\if{latex}{\out{\hypertarget{method-tmle3_Spec_mopttx_vim-make_tmle_task}{}}}
\subsection{Method \code{make_tmle_task()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tmle3_Spec_mopttx_vim$make_tmle_task(data, node_list, ...)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tmle3_Spec_mopttx_vim-set_opt"></a>}}
\if{latex}{\out{\hypertarget{method-tmle3_Spec_mopttx_vim-set_opt}{}}}
\subsection{Method \code{set_opt()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tmle3_Spec_mopttx_vim$set_opt(opt)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tmle3_Spec_mopttx_vim-make_params"></a>}}
\if{latex}{\out{\hypertarget{method-tmle3_Spec_mopttx_vim-make_params}{}}}
\subsection{Method \code{make_params()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tmle3_Spec_mopttx_vim$make_params(tmle_task, likelihood)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tmle3_Spec_mopttx_vim-clone"></a>}}
\if{latex}{\out{\hypertarget{method-tmle3_Spec_mopttx_vim-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tmle3_Spec_mopttx_vim$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
