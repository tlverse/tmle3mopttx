
<!-- README.md is generated from README.Rmd. Please edit that file -->
R/`tmle3mopttx`
===============

\[![Travis-CI Build Status]() \[![AppVeyor Build Status]() \[![Coverage Status]() \[\[CRAN\]) \[![CRAN downloads]() [![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

Description
-----------

`tmle3mopttx` estimates the optimal individualized treatment rule for the categorical treatment using the modern implementation of the Super Learner algorithm ([sl3](https://github.com/tlverse/sl3)) and [tmle3](https://github.com/tlverse/tmle3) \[@vdl2007super\]. In order to avoid nesting cross-validation, it relies on split-specific estimates of *Q* (𝔼(*Y*|*A*, *W*)) and *g* ((*P*(*A*|*W*))) in order to estimate the rule as described by Coyle et al \[@jeremythesis\]. In addition, it provides the Targeted Maximum Likelihood estimates of the mean performance using CV-TMLE under the estimated rules. This is an adapter package for use with the `tmle3` framework and the tlverse software ecosystem for Targeted Learning.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Installation
------------

You can install the most recent *stable release* from GitHub via [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) with:

``` r
devtools::install_github("tlverse/tmle3mopttx")
```

------------------------------------------------------------------------

Example
-------

------------------------------------------------------------------------

License
-------

The contents of this repository are distributed under the GPL-3 license. See file `LICENSE` for details.
