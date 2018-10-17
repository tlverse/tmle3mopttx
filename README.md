
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`tmle3mopttx`
===============

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

> Targeted Learning and Variable Importance with Optimal Individualized Categorical Treatment

**Authors:** [Ivana Malenica](https://github.com/podTockom), [Jeremy R. Coyle](https://github.com/jeremyrcoyle) and [Mark J. van der Laan](https://vanderlaan-lab.org/)

Description
-----------

`tmle3mopttx` is an adapter/extension R package in the `tlverse` ecosystem, that supports estimation of the optimal individualized treatment and the mean outcome under the estimated rule. In particular, it relies on the modern implementation of the Super Learner algorithm, [sl3](https://github.com/tlverse/sl3) and [tmle3](https://github.com/tlverse/tmle3) for the targeting step (Mark J. van der Laan, Polley, and Hubbard 2007, Mark J van der Laan and Rose (2011)).

In order to avoid nesting cross-validation, it relies on split-specific estimates of 𝔼(*Y*|*A*, *W*) and *P*(*A*|*W*) in order to estimate the rule, as described by Coyle et al (Coyle 2017). In addition, the targeted maximum likelihood estimates of the mean performance under the estimated rule are obtained using CV-TMLE (Zheng and van der Laan 2010). The implementation supports categorical treatments, providing three different versions for the rule estimation. The goal of this work is to build upon the tlverse framework and the estimation methodology implemented for a single mean counterfactual outcome in order to introduce an end-to-end methodology for variable importance analyses.

------------------------------------------------------------------------

Installation
------------

You can install the most recent *stable release* from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) with:

``` r
devtools::install_github("tlverse/tmle3mopttx")
```

------------------------------------------------------------------------

Issues
------

If you encounter any bugs or have any specific feature requests, please [file an issue](https://github.com/tlverse/tmle3mopttx/issues).

------------------------------------------------------------------------

References
----------

Coyle, Jeremy R. 2017. “Computational Considerations for Targeted Learning.” PhD thesis, U.C. Berkeley.

van der Laan, Mark J, and Sherri Rose. 2011. *Targeted Learning: Causal Inference for Observational and Experimental Data*. Springer Science & Business Media.

van der Laan, Mark J., Eric C. Polley, and Alan E. Hubbard. 2007. “Super Learner.” *Statistical Applications in Genetics and Molecular Biology* 6 (1).

Zheng, W., and M. J van der Laan. 2010. “Asymptotic Theory for Cross-validated Targeted Maximum Likelihood Estimation.” *U.C. Berkeley Division of Biostatistics Working Paper Series.*
