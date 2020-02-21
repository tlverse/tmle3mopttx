
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`tmle3mopttx`
===============

[![Travis-CI Build Status](https://travis-ci.org/tlverse/tmle3mopttx.svg?branch=master)](https://travis-ci.org/tlverse/tmle3mopttx) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/tlverse/tmle3mopttx?branch=master&svg=true)](https://ci.appveyor.com/project/tlverse/tmle3mopttx) [![Coverage Status](https://codecov.io/gh/tlverse/tmle3mopttx/branch/master/graph/badge.svg)](https://codecov.io/gh/tlverse/tmle3mopttx) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

> Targeted Learning and Variable Importance with Optimal Individualized Categorical Treatment

**Authors:** [Ivana Malenica](https://github.com/podTockom), [Jeremy R. Coyle](https://github.com/jeremyrcoyle) and [Mark J. van der Laan](https://vanderlaan-lab.org/)

Description
-----------

Suppose one wishes to maximize (or minimize) the population mean of an outcome where for each individual one has access to measured baseline covariates. We consider estimation of the mean outcome under the optimal rule, where the candidate rules are restricted to depend only on user-supplied subset of the baseline covariates. The estimation problem is addressed in a statistical model for the data distribution that is nonparametric.

The `tmle3mopttx` is an adapter/extension R package in the `tlverse` ecosystem, that estimates the mean outcome under the following regimes:

1.  **Optimal Individualized Treatment for categorical treatment,**
2.  **Optimal Individualized Treatment based on possibly sub-optimal rules (including static rules),**
3.  **Optimal Individualized Treatment based on realistic rules.**

The `tmle3mopttx` also provides **variable importance** analysis in terms of optimizing the population mean of an outcome under rules 1-3.

In addition, `tmle3mopttx` supports estimating the mean outcome under regimes 1-3 under:

1.  **Missing outcome.**

In order to avoid nested cross-validation, `tmle3mopttx` relies on split-specific estimates of 𝔼<sub>0</sub>(*Y*|*A*, *W*) and *P*<sub>0</sub>(*A*|*W*) in order to estimate the rule. The targeted maximum likelihood estimates of the mean performance under the estimated rule are obtained using CV-TMLE.

For additional background on Targeted Learning and previous work on optimal individualized treatment regimes, please consider consulting van der Laan, Polley, and Hubbard (2007), Zheng and van der Laan (2010), van der Laan and Rose (2011), van der Laan and Luedtke (2015), Luedtke and van der Laan (2016), Coyle (2017) and van der Laan and Rose (2018).

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

License
-------

The contents of this repository are distributed under the GPL-3 license. See file `LICENSE` for details.

------------------------------------------------------------------------

References
----------

Coyle, J R. 2017. “Computational Considerations for Targeted Learning.” PhD thesis, U.C. Berkeley.

Luedtke, A., and M. J van der Laan. 2016. “Super-Learning of an Optimal Dynamic Treatment Rule.” *International Journal of Biostatistics* 12 (1): 305–32.

van der Laan, M. J, and A. Luedtke. 2015. “Targeted Learning of the Mean Outcome Under an Optimal Dynamic Treatment Rule.” *Journal of Causal Inference* 3 (1): 61–95.

van der Laan, M. J, and S. Rose. 2011. *Targeted Learning: Causal Inference for Observational and Experimental Data*. Springer Science & Business Media.

———. 2018. *Targeted Learning in Data Science: Causal Inference for Complex Longitudinal Studies*. Springer Science & Business Media.

van der Laan, M. J, E C. Polley, and A E. Hubbard. 2007. “Super Learner.” *Statistical Applications in Genetics and Molecular Biology* 6 (1).

Zheng, W., and M. J van der Laan. 2010. “Asymptotic Theory for Cross-validated Targeted Maximum Likelihood Estimation.” *U.C. Berkeley Division of Biostatistics Working Paper Series.*
