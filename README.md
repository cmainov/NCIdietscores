# NCIdietscores

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/cmainov/NCIdietscores/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmainov/NCIdietscores/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->
  
### Description
___

Compute diet quality scores from National Cancer Institute (NCI)-developed dietary assessment screeners. Specifically, this package facilitates the computation of dietary variables from two screeners:


* *Multifactor Screener in Observing Protein and Energy Nutrition (OPEN) Study Dietary Intake Screener* (use function: `mfs_scores`)
* *Percentage Energy from Fat Screener* (i.e., *The Quick Food Scan*) (use function: `qfs_scores`)


### Install
___

Install the development version hosted on this repository (requires the `devtools` package) by specifying the following syntax:

```
# install.packages( "devtools" )
devtools::install_github( "cmainov/NCIdietscores" )
```

### Notes
___

* `qfs_scores` and `mfs_scores` require specifically-formatted datasets as inputs. Data examples can be accessed using:

```
library( NCIdietscores )

data( diet.data ) # toy data example for `mfs_scores`
data( test.data ) # toy data examplefor `qfs_scores`
```

* Alternatively, users may also specify their own column names though these functions already assume a default set of column names.


