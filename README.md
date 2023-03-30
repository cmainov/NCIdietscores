# NCIdietscores

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/cmainov/NCIdietscores/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmainov/NCIdietscores/actions/workflows/R-CMD-check.yaml)
  
  [![Build Status](https://app.travis-ci.com/cmainov/NCIdietscores.svg?branch=master)](https://app.travis-ci.com/cmainov/NCIdietscores)
  
  [![Coverage Badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/cmainov/561effea43e5e22aa06db790e18db4fd/raw/NCIdietscores__heads_master.json)]

  <!-- badges: end -->
  
  
### Description
___

The functions in this package can be used to compute diet quality scores from National Cancer Institute (NCI)-developed dietary assessment screeners. Specifically, this package facilitates the computation of dietary variables from two screeners:


* *Multifactor Screener in Observing Protein and Energy Nutrition (OPEN) Study* (use function: `mfs_scores`)
* *Percentage Energy from Fat Screener* (i.e., *The Quick Food Scan*) (use function: `qfs_scores`)

Each function takes a `data.frame` as an input and returns the same `data.frame` with the diet scores appended and some summary statistics for the new variables. See the function documentations (`?qfs_scores` or `?mfs_scores`) for the names and descriptions of the variables each function outputs.


#### Links to the Screeners and Documentation Files
[Multifactor Screener](https://epi.grants.cancer.gov/diet/shortreg/instruments/multifactor-screener-in-open-self-report-version.pdf),
[Documentation](https://epi.grants.cancer.gov/diet/screeners/OPEN.pdf),
[Codebook/Data Dictionary](https://epi.grants.cancer.gov/past-initiatives/open/multifactor/open_multifactor_datadic.pdf)

[Quick Food Scan](https://epi.grants.cancer.gov/diet/shortreg/instruments/percent-energy-from-fat-screener.pdf),
[Documentation](https://epi.grants.cancer.gov/diet/screeners/fat/scoring.html)

___

### Install
___

Install the development version hosted on this repository (requires the `devtools` package) by specifying the following syntax:

```
# install.packages( "devtools" )
devtools::install_github( "cmainov/NCIdietscores" )
```

___

### Notes
___

* `qfs_scores` and `mfs_scores` require specifically-formatted datasets as inputs. Data examples can be accessed using:

```
library( NCIdietscores )

NCIdietscores::diet.data # toy data example for `mfs_scores`
NCIdietscores::test.data # toy data example for `qfs_scores`
```

* Alternatively, users may also specify their own column names though these functions already assume a default set of column names. See function documentation (`?qfs_scores` or `?mfs_scores`) for a guide on specifying column names and the function calls.

___

### Example Run
___

Using the toy package data, we can run `mfs_scores` on `diet.data`

```{r}
mfs_scores( NCIdietscores::diet.data )

#    pred.fiber       pred.pcf      pred.fv7.ce    
#  Min.   :2.471   Min.   :15.99   Min.   : 1.792  
#  1st Qu.:3.245   1st Qu.:45.62   1st Qu.: 4.478  
#  Median :4.132   Median :52.53   Median : 5.365  
#  Mean   :4.110   Mean   :54.23   Mean   : 5.992  
#  3rd Qu.:4.872   3rd Qu.:61.83   3rd Qu.: 7.294  
#  Max.   :6.863   Max.   :93.23   Max.   :11.602  
#   pred.fv6.ce     raw.pred.fv7.ce   raw.pred.fv6.ce  
#  Min.   : 1.245   Min.   : 0.9808   Min.   : 0.4712  
#  1st Qu.: 4.280   1st Qu.: 4.0387   1st Qu.: 3.5043  
#  Median : 4.611   Median : 4.9938   Median : 4.4268  
#  Mean   : 5.386   Mean   : 5.8911   Mean   : 5.2252  
#  3rd Qu.: 6.806   3rd Qu.: 7.7228   3rd Qu.: 7.3139  
#  Max.   :10.329   Max.   :12.6409   Max.   :11.4094  
#   pred.fv7.ps     pred.fv6.ps   
#  Min.   :1.948   Min.   :1.511  
#  1st Qu.:2.806   1st Qu.:2.684  
#  Median :3.165   Median :2.864  
#  Mean   :3.210   Mean   :2.987  
#  3rd Qu.:3.607   3rd Qu.:3.520  
#  Max.   :4.532   Max.   :3.980  
#   HQ1 HQ2 HQ3 HQ4 HQ5 HQ6 HQ7 HQ8 HQ9 HQ10 HQ11 HQ12 HQ13
# 1   5   4   5   6   8   4   9   2   6    7    7    5    8
# 2   6   9   1   7   1   7   8   1   1    5    9    9    1
# 3   3   6   3   3   3   6   3   5   9    8    1    5    5
# 4   9   3   6   7   5   1   9   6   6    2    8    8    6
# 5   3   9   6   6   5   2   2   4   9    3    8    1    3
# 6   6   4   5   4   6   7   9   3   5    9    6    3    9
#   HQ14 HQ15 HQ16 HQ2A SEX AGE pred.fiber pred.pcf
# 1    7    2    4    4   1  27   5.163022 35.76562
# 2    5    7    9    1   2  23   6.097605 66.98295
# 3    3    5    4    3   2  90   3.102291 45.61516
# 4    4    7    6    2   2  60   5.054856 49.02046
# 5    6    2    9    1   2  37   3.025184 70.54729
# 6    2    9    8    5   1  98   5.163261 63.78510
#   pred.fv7.ce pred.fv6.ce raw.pred.fv7.ce raw.pred.fv6.ce
# 1   10.150420    8.785047       10.690459        9.248209
# 2    9.028103    8.665692       10.630752       10.230210
# 3    4.816921    3.771122        4.652084        3.319304
# 4    7.029482    6.943260        7.722786        7.696331
# 5    4.497077    4.388527        4.226628        4.129235
# 6    9.467351    7.530622        9.785022        7.535112
#   pred.fv7.ps pred.fv6.ps
# 1    4.189652    3.777207
# 2    4.001536    3.896283
# 3    3.018089    2.612201
# 4    3.532296    3.520022
# 5    2.865299    2.821729
# 6    4.169954    3.593200
```
___

### Bug-reporting and Contributions
___

**NCIdietscores** is licensed under the [GNU General Public License Version 3](https://www.gnu.org/licenses/gpl-3.0.txt). You can report any bugs, ask questions, or suggest new features in the [issue queue](https://github.com/cmainov/NCIdietscores/issues). Pull requests and contributions will be reviewed and incorporated into the package at the discretion of the package maintainer.

___

### References
___
* The Multifactor screener in the Observing Protein & Energy Nutrition (OPEN) Study. Epidemiology and Genomics Research Program. National Cancer Institute. https://epi.grants.cancer.gov/diet/screeners/files.Updated November 20, 2019.

* Thompson FE, Midthune D, Subar AF, Kipnis V, Kahle LL, Schatzkin A. Development and evaluation of a short instrument to estimate usual dietary intake of percentage energy from fat. J Am Diet Assoc 2007 May;107(5):760-7.

* Thompson FE, Midthune D, Williams GC, Yaroch AL, Hurley TG, Resnicow K, Hebert JR, Toobert DJ, Greene GW, Peterson K, Nebeling L. Evaluation of a short dietary assessment instrument for percentage energy from fat in an intervention study. J Nutr 2008 Jan;138(1):193S-199S.


