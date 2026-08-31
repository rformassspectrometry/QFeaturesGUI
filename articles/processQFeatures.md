# processQFeatures App

``` r

library(QFeaturesGUI)
```

This app can be used once the data from
[`importQFeatures()`](https://rformassspectrometry.github.io/QFeaturesGUI/reference/importQFeatures.md)
have been downloaded. In order to do that, unzip the folder downloaded,
and load the RDS that contain the QFeatures object in your R environment
or pass directly the path as an argument when using
[`processQFeatures()`](https://rformassspectrometry.github.io/QFeaturesGUI/reference/processQFeatures.md).
You can also directly start the application
[`processQFeatures()`](https://rformassspectrometry.github.io/QFeaturesGUI/reference/processQFeatures.md)
and load the RDS into the application.

## Start the app

Parameters of the Shiny application:

- `qfeatures` (optional): a QFeatures object. This can be either a path
  to an RDS file or a QFeatures object already loaded in your R session.

- `prefilledSteps` (optional): defines the different steps of the
  workflow used to analyse the QFeatures object. Accepted values are
  `sampleFiltering`, `featureFiltering`, `normalisation`,
  `missingValuesFeatures`, `missingValuesSamples`, `zeroToNA`,
  `logTransform`, `imputation`, `aggregation`, and `join`. The suggested
  workflow set as the default value is `sampleFiltering`,
  `featureFiltering`, `missingValuesFeatures`, `missingValuesSamples`,
  `normalisation`, `aggregation`, `join`, and `aggregation`.

- `initialSets` (optional): sets to use for the analysis of the
  QFeatures object. The default is `seq_along(qfeatures)`.

The app can be started without any parameter, in that case the app will
start by asking you to load an RDS containing a QFeatures object. Once
loaded you can adjust the sets needed for the analysis of the object.
Note that you can also use the “Use demo QFeatures” button to use an
example QFeatures.

## Workflow configuration

A predefined workflow is defined by default with the argument
`prefilledSteps`. If `prefilledSteps` was modified when the application
was launched, this change will be taken into account.

The page contains a brief explanation of each step. To modify the
workflow, drag and drop the different steps to be executed on the
QFeatures object. Once the workflow is configured, click on
`Confirm Current Workflow`.

If the default workflow is the one you want to use, or if you have
already specified the desired workflow in the `prefilledSteps`
parameter, simply click on `Step 1`.

## Sample/Feature Filtering

### Pre-Filtering Metrics section

![pre-Filtering metrics section](screenshots/preFilteringMetrics.jpg)

pre-Filtering metrics section

The `Pre-Filtering Metrics` section (Figure 1) is composed of two
different parts:

- a dimension reduction graph (`B`) on the right and the settings (`A`)
  to customize this graph on the left. For example, you can choose the
  dimensionality reduction method or type.

- a single feature visualization (`C`) where you can display a boxplot
  of a selected feature split by sample annotation.

### Filtering section

![filtering section](screenshots/filtering.jpg)

filtering section

- The second section is `Filtering` (Figure 2). To create a condition,
  click `Add Filtering Condition` (`A`). Clicking this button will open
  the filtering boxes (`B`). In this box, you can add filtering
  conditions. Once you have chosen the annotation and the value to
  filter, the number of cells that will be filtered is updated
  dynamically. You will also find a summary of the selected conditions.
  Once the filters are chosen, click `Apply Filters` (`C`). If a
  condition is unnecessary, you can also click on
  `Remove Last Condition` (`A`).

### Post-filtering Metrics section

![post-Filtering metrics section](screenshots/postFilteringMetrics.jpg)

post-Filtering metrics section

- The third section, entitled `Post-Filtering Metrics` (Figure 3),
  repeats the elements of the `Pre-Filtering Metrics` section, but the
  graph presents the data after filtering. It also contains statistics
  on the number and percentage of samples/features removed.

Once the filtering has been done, save the object by clicking the button
`Save the processed sets`.

## Filtering missing values by samples/features

![Filtering missing values by samples/features
page](screenshots/missing_values_filtering.jpg)

Filtering missing values by samples/features page

Missing values can be very numerous in certain proteomics experiments
and need to be dealt with carefully.

In this step, you will need to define a threshold for the percentage of
missing values (NA) beyond which a feature/sample should be removed
(`A`). By changing this value, the statistics relating to the number and
percentage of features/samples removed (`B`) and the associated graph
(`C`) will be automatically updated. Once the threshold is defined,
click on `Save the processed sets` (`D`).

## Normalisation

![Normalization page](screenshots/normalisation.jpg)

Normalization page

QFeatures objects can be normalised in this step. Several normalisation
methods are available (`A`).

- `sum` and `max`, where each feature’s intensity is divided by the
  maximum or the sum of the feature, respectively. These two methods are
  applied along the features (rows).

- `center.mean` and `center.median` centre the respective sample
  (column) intensities by subtracting the respective column means or
  medians. `div.mean` and `div.median` divide by the column means or
  medians.

- `diff.median` centres all samples (columns) so that they all match the
  grand median by subtracting the respective column median differences
  from the grand median.

- Using `quantiles` or `quantiles.robust` applies (robust) quantile
  normalisation, as implemented in
  `preprocessCore::normalize.quantiles()` and
  `preprocessCore::normalize.quantiles.robust()`. `vsn` uses the
  `vsn::vsn2()` function. Note that the latter also glog-transforms the
  intensities. See the respective manuals for more details and function
  arguments.

Once the normalisation method is selected, click on
`Apply normalisation` (`B`); the density graph after normalisation will
be displayed. Click on `Save the processed sets` (`C`).

## Zero to NA

![Zero to NA page](screenshots/zero_to_na.jpg)

Zero to NA page

This step converts all zero intensities into missing values (NA) in the
selected sets.

## Log Transformation

![Log transformation page](screenshots/logTransform.jpg)

Log transformation page

When analysing continuous data using parametric methods (such as t-test
or linear models), it is often necessary to log-transform the data.

The log base is set to `log2` by default, but can also be set to `log10`
or `ln` (`A`). A pseudocount value can be added (`B`) to handle zero
values in the data before applying the logarithm. Then click on
`Apply log transform` (`C`); this will display a density plot after log
transformation. Once this is done, do not forget to click on
`Save the processed sets` (`D`).

## Imputation

![Imputation page](screenshots/imputation.jpg)

Imputation page

There are two types of mechanisms resulting in missing values in LC/MSMS
experiments.

- Missing values resulting from the absence of detection of a feature,
  despite ions being present at detectable concentrations. For example,
  this can happen in the case of ion suppression or as a result of the
  stochastic, data-dependent nature of the MS acquisition method. These
  missing values are expected to be randomly distributed in the data and
  are defined as *missing at random* (MAR) or *missing completely at
  random* (MCAR).

- Biologically relevant missing values, resulting from the absence or
  low abundance of ions (below the limit of detection of the
  instrument). These missing values are not expected to be randomly
  distributed in the data and are defined as *missing not at random*
  (MNAR).

See [Imputing quantitative proteomics
data](https://rformassspectrometry.github.io/QFeatures/articles/Imputation.html)
for more information.

First choose an imputation method (`A`):

- `knn`: nearest neighbour averaging, as implemented in the
  [`impute::impute.knn`](https://www.bioconductor.org/packages/release/bioc/html/impute.html)
  function. Note that this function is used with default parameters.

- `MinDet`: performs the imputation of left-censored missing data using
  a deterministic minimal value approach. Considering expression data
  with `n` samples and `p` features, for each sample, the missing
  entries are replaced with a minimal value observed in that sample. The
  minimal value observed is estimated as the q-th quantile (default q =
  0.01) of the observed values in that sample. Implemented in
  [`imputeLCMD::impute.MinDet`](https://rdrr.io/cran/imputeLCMD/man/impute.MinDet.html).
  Note that this function is used with default parameters.

- `zero`: replaces the missing values with 0.

Then click on `Apply imputation` (`B`). Once the imputation has run, the
page will display a post-imputation density plot that you can colour by
colData (`C`). Once the imputation step is finished, do not forget to
`Save the processed sets` (`D`).

## Aggregation

![Aggregation page](screenshots/aggregate.jpg)

Aggregation page

At this stage, it is possible to use the peptide-level intensities or
aggregate the peptide-level data into protein intensities.

To do this, a quantitative feature aggregation function is required.
This function, called `Function to aggregate` (`A`), takes a matrix as
input and returns a vector of length equal to `ncol(x)`. The function
can be one of the following types:

- [`MsCoreUtils::medianPolish()`](https://rdrr.io/pkg/MsCoreUtils/man/medianPolish.html)
  fits an additive model (two-way decomposition) using Tukey’s median
  polish procedure with
  [`stats::medpolish()`](https://rdrr.io/r/stats/medpolish.html).

- [`MsCoreUtils::robustSummary`](https://rdrr.io/pkg/MsCoreUtils/man/robustSummary.html)
  calculates a robust aggregation using
  [`MASS::rlm()`](https://rdrr.io/pkg/MASS/man/rlm.html) (default).

- [`base::colMeans()`](https://rdrr.io/r/base/colSums.html) to use the
  mean of each column.

- [`matrixStats::colMedians()`](https://rdrr.io/pkg/matrixStats/man/rowMedians.html)
  to use the median of each column.

- [`base::colSums()`](https://rdrr.io/r/base/colSums.html) to use the
  sum of each column.

See
[`MsCoreUtils::aggregate_by_vector()`](https://rdrr.io/pkg/MsCoreUtils/man/aggregate.html)
for more aggregation functions.

A rowData variable (`B`) is also needed to define how to aggregate the
features of the assays.

Once these two variables have been set, click on `Aggregate` (`C`).

An aggregation boxplot will be displayed. This boxplot shows a feature
coloured by a colData value. Once the aggregation is done, click on
`Save the processed sets`.

## Join

![Join page](screenshots/join.jpg)

Join page

The Join tab will combine the results from the previous step into a
single dataset for further analysis or export. Simply give it a new
assay name (`A`). Then click `Join and save the processed sets` (`B`).

## Summary

![Summary page](screenshots/summary.jpg)

Summary page

The summary page displays the various assays present in the QFeatures
object and indicates their interrelationships. Finally, a download
button provides a zip file containing the final QFeatures object, the
script used to generate it, and the R session with the package version.

## Session Information

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] QFeaturesGUI_0.99.1 BiocStyle_2.40.0   
#> 
#> loaded via a namespace (and not attached):
#>  [1] tidyselect_1.2.1            viridisLite_0.4.3          
#>  [3] dplyr_1.2.1                 farver_2.1.2               
#>  [5] S7_0.2.2                    fastmap_1.2.0              
#>  [7] SingleCellExperiment_1.34.0 lazyeval_0.2.3             
#>  [9] shinyjs_2.1.1               promises_1.5.0             
#> [11] nipals_1.0                  digest_0.6.39              
#> [13] mime_0.13                   lifecycle_1.0.5            
#> [15] cluster_2.1.8.2             ProtGenerics_1.44.0        
#> [17] magrittr_2.0.5              compiler_4.6.1             
#> [19] rlang_1.3.0                 sass_0.4.10                
#> [21] tools_4.6.1                 igraph_2.3.3               
#> [23] yaml_2.3.12                 data.table_1.18.6.1        
#> [25] knitr_1.51                  S4Arrays_1.12.0            
#> [27] htmlwidgets_1.6.4           DelayedArray_0.38.2        
#> [29] plyr_1.8.9                  RColorBrewer_1.1-3         
#> [31] abind_1.4-8                 purrr_1.2.2                
#> [33] BiocGenerics_0.58.1         desc_1.4.3                 
#> [35] grid_4.6.1                  stats4_4.6.1               
#> [37] xtable_1.8-8                waiter_0.2.5.1             
#> [39] ggplot2_4.0.3               scales_1.4.0               
#> [41] MASS_7.3-65                 MultiAssayExperiment_1.38.0
#> [43] SummarizedExperiment_1.42.0 cli_3.6.6                  
#> [45] rmarkdown_2.31              ragg_1.5.2                 
#> [47] generics_0.1.4              otel_0.2.0                 
#> [49] httr_1.4.8                  reshape2_1.4.5             
#> [51] shinydashboardPlus_2.0.6    cachem_1.1.0               
#> [53] stringr_1.6.0               AnnotationFilter_1.36.0    
#> [55] BiocManager_1.30.27         XVector_0.52.0             
#> [57] matrixStats_1.5.0           vctrs_0.7.3                
#> [59] Matrix_1.7-5                jsonlite_2.0.0             
#> [61] bookdown_0.48               IRanges_2.46.0             
#> [63] S4Vectors_0.50.2            clue_0.3-68                
#> [65] systemfonts_1.3.2           fontawesome_0.5.3          
#> [67] plotly_4.12.1               tidyr_1.3.2                
#> [69] jquerylib_0.1.4             glue_1.8.1                 
#> [71] pkgdown_2.2.1               QFeatures_1.22.0           
#> [73] shinyalert_3.1.0            DT_0.34.0                  
#> [75] stringi_1.8.9               gtable_0.3.6               
#> [77] later_1.4.8                 GenomicRanges_1.64.0       
#> [79] shinydashboard_0.7.3        tibble_3.3.1               
#> [81] pillar_1.11.1               htmltools_0.5.9            
#> [83] Seqinfo_1.2.0               R6_2.6.1                   
#> [85] textshaping_1.0.5           evaluate_1.0.5             
#> [87] shiny_1.14.0                lattice_0.22-9             
#> [89] Biobase_2.72.0              shinyFeedback_0.4.0        
#> [91] httpuv_1.6.17               bslib_0.12.0               
#> [93] Rcpp_1.1.2                  SparseArray_1.12.2         
#> [95] xfun_0.60                   MsCoreUtils_1.24.0         
#> [97] fs_2.1.0                    MatrixGenerics_1.24.0      
#> [99] pkgconfig_2.0.3
```
