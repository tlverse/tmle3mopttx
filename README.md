
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`tmle3mopttx`

[![R-CMD-check](https://github.com/tlverse/tmle3mopttx/workflows/R-CMD-check/badge.svg)](https://github.com/tlverse/tmle3mopttx/actions)
[![Coverage
Status](https://codecov.io/gh/tlverse/tmle3mopttx/branch/master/graph/badge.svg)](https://codecov.io/gh/tlverse/tmle3mopttx)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

> Targeted Learning and Variable Importance with Optimal Individualized
> Categorical Treatment

**Authors:** [Ivana Malenica](https://github.com/podTockom), [Jeremy R.
Coyle](https://github.com/jeremyrcoyle) and [Mark J. van der
Laan](https://vanderlaan-lab.org/)

## Description

Suppose one wishes to maximize (or minimize) the population mean of an
outcome where for each individual one has access to measured baseline
covariates. We consider estimation of the mean outcome under the optimal
rule, where the candidate rules are restricted to depend only on
user-supplied subset of the baseline covariates (or no covariates
specified). The estimation problem is addressed in a statistical model
for the data distribution that is nonparametric.

The `tmle3mopttx` is an adapter/extension R package in the `tlverse`
ecosystem, that estimates the mean outcome under the following regimes:

1)  **Optimal Individualized Treatment for categorical treatment,**
2)  **Optimal Individualized Treatment based on possibly sub-optimal
    rules (including static rules),**
3)  **Optimal Individualized Treatment based on realistic rules.**
4)  **Optimal Individualized Treatment with resource constraints.**

The `tmle3mopttx` also provides **variable importance** analysis in
terms of optimizing the population mean of an outcome for settings
(1)-(3). It also can return an **interpretable rule** based on a Highly
Adaptive Lasso fit.

In addition, `tmle3mopttx` supports estimating the mean outcome under
regimes under the following missigness process:

1.  **Missing outcome.**

In order to avoid nested cross-validation, `tmle3mopttx` relies on
split-specific estimates of the conditional expectation of the outcome
(![\\mathbb{E}\_0(Y\|A,W)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbb%7BE%7D_0%28Y%7CA%2CW%29 "\mathbb{E}_0(Y|A,W)"))
and conditional probability of treatment
(![P_0(A\|W)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;P_0%28A%7CW%29 "P_0(A|W)"))
in order to estimate the rule. The targeted maximum likelihood estimates
of the mean performance under the estimated rule are obtained using
CV-TMLE.

The description of the method implemented, with simulations and data
examples using `tmle3mopttx` can be found in Malenica (2021). For
additional background on Targeted Learning and previous work on optimal
individualized treatment regimes, please consider consulting van der
Laan, Polley, and Hubbard (2007), Zheng and van der Laan (2010), van der
Laan and Rose (2011), van der Laan and Luedtke (2015), Luedtke and van
der Laan (2016), Coyle (2017) and van der Laan and Rose (2018).

------------------------------------------------------------------------

## Installation

You can install the most recent *stable release* from GitHub via
[`devtools`](https://www.rstudio.com/products/rpackages/devtools/) with:

``` r
devtools::install_github("tlverse/tmle3mopttx")
```

------------------------------------------------------------------------

## Citation

After using the tstmle3 R package, please cite the following:

``` r
@software{malenica2022tmle3mopttx,
      author = {Malenica, Ivana and Coyle, Jeremy and {van der Laan}, Mark J},
      title = {{tmle3mopttx}: Targeted Learning and Variable Importance with Optimal Individualized Categorical Treatment},
      year  = {2022},
      doi = {},
      url = {https://github.com/tlverse/tmle3mopttx},
      note = {R package version 1.0.0}
    }
```

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/tlverse/tmle3mopttx/issues).

------------------------------------------------------------------------

## License

The contents of this repository are distributed under the GPL-3 license.
See file `LICENSE` for details.

------------------------------------------------------------------------

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-jeremythesis" class="csl-entry">

Coyle, J R. 2017. “Computational Considerations for Targeted Learning.”
PhD thesis, U.C. Berkeley.

</div>

<div id="ref-luedtke2016super" class="csl-entry">

Luedtke, A., and M. J van der Laan. 2016. “Super-Learning of an Optimal
Dynamic Treatment Rule.” *International Journal of Biostatistics* 12
(1): 305–32.

</div>

<div id="ref-malenica2021opttx" class="csl-entry">

Malenica, Ivana. 2021. “Optimal Individualized Treatment Regimes.” In
*Targeted Learning in r: Causal Data Science with the Tlverse Software
Ecosystem*.

</div>

<div id="ref-vanderLaanLuedtke15" class="csl-entry">

van der Laan, M. J, and A. Luedtke. 2015. “Targeted Learning of the Mean
Outcome Under an Optimal Dynamic Treatment Rule.” *Journal of Causal
Inference* 3 (1): 61–95.

</div>

<div id="ref-vdl2007super" class="csl-entry">

van der Laan, M. J, E C. Polley, and A E. Hubbard. 2007. “Super
Learner.” *Statistical Applications in Genetics and Molecular Biology* 6
(1).

</div>

<div id="ref-vdl2011targeted" class="csl-entry">

van der Laan, M. J, and S. Rose. 2011. *Targeted Learning: Causal
Inference for Observational and Experimental Data*. Springer Science &
Business Media.

</div>

<div id="ref-vdl2018targeted" class="csl-entry">

———. 2018. *Targeted Learning in Data Science: Causal Inference for
Complex Longitudinal Studies*. Springer Science & Business Media.

</div>

<div id="ref-cvtmle2010" class="csl-entry">

Zheng, W., and M. J van der Laan. 2010. “<span class="nocase">Asymptotic
Theory for Cross-validated Targeted Maximum Likelihood
Estimation</span>.” *U.C. Berkeley Division of Biostatistics Working
Paper Series.*

</div>

</div>
