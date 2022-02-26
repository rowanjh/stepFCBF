
# stepFCBF

Provides the step_FCBF function, which allows fast correlation based
filter (FCBF) feature selection to be added as a recipe step in
‘tidymodels’. step_FCBF uses the bioconductor ‘FCBF’ package as the
engine.

## Installation

You can install the development version of stepFCBF from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rowanjh/stepFCBF")
```

stepFCBF depends on the bioconductor package “FCBF”. See the [package
website](https://www.bioconductor.org/packages/release/bioc/html/FCBF.html)
for installation instructions or run the below code:

``` r
BiocManager::install("FCBF", version="devel")
```

## Example

Basic syntax to incorporate step_FCBF into a model

``` r
## library(tidymodels)
## library(stepFCBF)
## 
## # Basic example code
## my_recipe <- recipe(Species ~ ., data = iris) %>%
##     step_FCBF(all_predictors(), min_su = 0.001)
## 
## prepped_recipe <- my_recipe %>% prep(iris)
## 
```
