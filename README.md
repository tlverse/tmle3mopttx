
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`tmle3mopttx`
===============

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

> Targeted Learning and Variable Importance with Optimal Individualized Categorical Treatment

**Authors:** [Ivana Malenica](https://github.com/podTockom), [Jeremy R. Coyle](https://github.com/jeremyrcoyle) and [Mark J. van der Laan](https://vanderlaan-lab.org/)

Description
-----------

`tmle3mopttx` estimates the optimal individualized treatment rule for the categorical treatment using the modern implementation of the Super Learner algorithm ([sl3](https://github.com/tlverse/sl3)) and [tmle3](https://github.com/tlverse/tmle3) (van der Laan, Polley, and Hubbard 2007). In order to avoid nesting cross-validation, it relies on split-specific estimates of *Q* (𝔼(*Y*|*A*, *W*)) and *g* ((*P*(*A*|*W*))) in order to estimate the rule as described by Coyle et al (Coyle 2017). In addition, it provides the Targeted Maximum Likelihood estimates of the mean performance using CV-TMLE under the estimated rules. This is an adapter package for use with the `tmle3` framework and the tlverse software ecosystem for Targeted Learning.

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

van der Laan, Mark J., Eric C. Polley, and Alan E. Hubbard. 2007. “Super Learner.” *Statistical Applications in Genetics and Molecular Biology* 6 (1).
