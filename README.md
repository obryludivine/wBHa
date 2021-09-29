
# wBHa

<!-- badges: start -->
<!-- badges: end -->
## Version 

???

## Description

**wBHa** is an R package that implements a covariate multiple testing procedure specifically adapted in a Genome-Wide Association Studies context. wBHa allows better detection of rare variants (which where difficult to detect since the current existing procedures are not powerful enough) by better integrating external information while allowing the optimization of the overall power.

## Installation

You can install the current released version of wBHa from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("wBHa")
```

You can also install the current development version from github with:
``` r
library(devtools)
install_github("obryludivine/wBHa")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
set.seed(123)

# Load package
library(wBHa)

pvalues <- c(runif(100,0,0.1), runif(100,0,1))
covariates <- runif(200,0.05,0.5)
wBHa_object <- wBHa(pvalues, covariates, alpha=0.05, K=60)

data("GSE90102_01")
pvalues <- GSE90102_01$rawp
covariates <- GSE90102_01$MAF
wBHa_object <- wBHa(pvalues, covariates)
```

## Contacts

