# NCIdietscores

 <!-- badges: start -->
  [![R-CMD-check](https://github.com/cmainov/NCIdietscores/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmainov/NCIdietscores/actions/workflows/R-CMD-check.yaml)
  
  
  <!-- badges: end -->
  
  
### Description
___

The functions in this package can be used to compute diet quality scores from National Cancer Institute (NCI)-developed dietary assessment screeners. Specifically, it facilitates the computation of dietary variables from two screeners:


* *Multifactor Screener in Observing Protein and Energy Nutrition (OPEN) Study* (use function: `mfs_scores`)
* *Percentage Energy from Fat Screener* (i.e., *The Quick Food Scan*) (use function: `qfs_scores`)

See the function documentations (`?qfs_scores` or `?mfs_scores`) for the names and descriptions of the variables each function outputs.


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

### Bug-reporting and Contributions
___

**NCIdietscores** is licensed under the [GNU General Public License Version 3](https://www.gnu.org/licenses/gpl-3.0.txt). You can report any bugs, ask questions, or suggest new features in the [issue queue](https://github.com/cmainov/NCIdietscores/issues). Pull requests and contributions will be reviewed and incorporated into the package at the discretion of the package maintainer.

___

### References
___
* The Multifactor screener in the Observing Protein & Energy Nutrition (OPEN) Study. Epidemiology and Genomics Research Program. National Cancer Institute. https://epi.grants.cancer.gov/diet/screeners/files.Updated November 20, 2019.

* Thompson FE, Midthune D, Subar AF, Kipnis V, Kahle LL, Schatzkin A. Development and evaluation of a short instrument to estimate usual dietary intake of percentage energy from fat. J Am Diet Assoc 2007 May;107(5):760-7.

* Thompson FE, Midthune D, Williams GC, Yaroch AL, Hurley TG, Resnicow K, Hebert JR, Toobert DJ, Greene GW, Peterson K, Nebeling L. Evaluation of a short dietary assessment instrument for percentage energy from fat in an intervention study. J Nutr 2008 Jan;138(1):193S-199S.


