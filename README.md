<!-- badges: start -->
[![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/out2text.svg)](https://github.com/sfcheung/out2text)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/out2text.svg)](https://github.com/sfcheung/out2text/commits/main)
[![R-CMD-check](https://github.com/sfcheung/out2text/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/out2text/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Work-in-progress. Not ready for use)

(Version 0.0.9000, updated on 2023-02-01, [release history](https://sfcheung.github.io/out2text/news/index.html))

# out2text

This package contain functions to extract values such as
regression coefficients, *p*-values, confidence intervals,
from common procedures such as regression conducted by `lm()`
and format them as strings so that they can be embedded in
RMarkdown or Quarto documents.

For more information on this package, please visit its GitHub page:

https://sfcheung.github.io/out2text/

# Installation

Stable release versions of this package can be downloaded below:

https://github.com/sfcheung/out2text/releases

The latest developmental version of this package can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/out2text")
```

# Issues

If you have any suggestions and found any bugs, please feel
feel to open a GitHub issue. Thanks.