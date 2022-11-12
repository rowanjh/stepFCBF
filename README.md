
# stepFCBF package

## Introduction

Provides the step_fcbf function, which allows fast correlation based
filter (FCBF) feature selection to be added as a recipe step in
‘tidymodels’. FCBF is a feature selection method that aims to retain a
set of features with high correlation to the outcome, and low
correlation to other retained features. It uses the metric ‘Symmetrical
Uncertainty’ to determine the magnitude of correlation.

The underlying calculations are conducted by the Bioconductor package
FCBF, and the algorithm is described in Yu, L. and Liu, H.; Feature
Selection for High-Dimensional Data: A Fast Correlation Based Filter
Solution,Proc. 20th Intl. Conf. Mach. Learn. (ICML-2003), Washington DC,
2003.

##### Discretization

step_fcbf can handle both nominal and numeric features. However, the
underlying FCBF algorithm can only handle nominal features, so numeric
features first need to be discretized. step_fcbf function internally
converts numeric features to binary nominal features, using a median
split by default (other quantiles between 0-1 can be provided with the
‘cutpoint =’ argument). Discretization is only used within the feature
selection algorithm, once features have been selected the numeric
version of the feature is retained for further processing and modeling.

##### Symmetric Uncertainty Threshold

FCBF requires a cut point to be provided for symmetrical uncertainty
(between 0-1). Smaller thresholds of SU will result in more features
being retained. Appropriate thresholds are data-dependent, so it is
recommended that different values of SU be explored using a subset of
the training set. e.g. see ‘FCBF::su_plot()’

## Installation

You can install the development version of stepFCBF from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rowanjh/stepFCBF")
```

stepFCBF depends on the bioconductor package “FCBF”. Run the below code
or see the [FCBF package
website](https://www.bioconductor.org/packages/release/bioc/html/FCBF.html)
for more detailed installation instructions:

``` r
# install.packages("BiocManager")
BiocManager::install("FCBF")
```

## Examples

Basic example for including step_fcbf into a recipe

``` r
library(recipes)
library(stepFCBF)
data("iris")
```

``` r
my_recipe <- recipe(Species ~ ., data = iris) %>%
    step_fcbf(all_predictors(), min_su = 0.001)

prepped_recipe <- my_recipe %>% prep(iris)
#> [1] "Number of features features =  4"
#> [1] "Number of prospective features =  4"
#> [1] "Number of final features =  2"

# Original features
prepped_recipe$var_info
#> # A tibble: 5 x 4
#>   variable     type    role      source  
#>   <chr>        <chr>   <chr>     <chr>   
#> 1 Sepal.Length numeric predictor original
#> 2 Sepal.Width  numeric predictor original
#> 3 Petal.Length numeric predictor original
#> 4 Petal.Width  numeric predictor original
#> 5 Species      nominal outcome   original

# Selected features
prepped_recipe$term_info
#> # A tibble: 3 x 4
#>   variable    type    role      source  
#>   <chr>       <chr>   <chr>     <chr>   
#> 1 Sepal.Width numeric predictor original
#> 2 Petal.Width numeric predictor original
#> 3 Species     nominal outcome   original
```
