# Launch a Shiny application to process QFeatures objects

`processQFeatures()` launches an interactive Shiny application that
allows users to visually configure and apply pre-processing workflows to
a QFeatures object.

The input `qfeatures` can be provided either as an in-memory QFeatures
object or as a path to an `.rds` file containing one.

## Usage

``` r
processQFeatures(
  qfeatures,
  initialSets = seq_along(qfeatures),
  prefilledSteps = c("sampleFiltering", "featureFiltering", "missingValuesFeatures",
    "missingValuesSamples", "normalisation", "aggregation", "join", "aggregation")
)
```

## Arguments

- qfeatures:

  A
  [QFeatures](https://rformassspectrometry.github.io/QFeatures/reference/QFeatures-class.html)
  object to be processed, or a character string specifying the path to a
  `.rds` file containing a
  [QFeatures](https://rformassspectrometry.github.io/QFeatures/reference/QFeatures-class.html)
  object.

- initialSets:

  An integer, logical, or character vector specifying which assays
  (feature sets) should be used as the starting point for processing.
  Defaults to all assays in `qfeatures`.

- prefilledSteps:

  A character vector specifying the initial workflow steps to display
  when the application launches. Steps must be provided using their
  internal identifiers (e.g. `"sampleFiltering"`, `"featureFiltering"`,
  `"normalisation"`).

## Value

The processQFeatures Shiny application.

## Details

The application provides a drag-and-drop workflow builder that allows
users to select, order, and configure processing steps such as
filtering, normalization, and transformation. The configured workflow
can then be applied to the selected assays.

## Examples

``` r

# the example should be launched with `ask = FALSE`

library(QFeatures)
#> Loading required package: MultiAssayExperiment
#> Loading required package: SummarizedExperiment
#> Loading required package: MatrixGenerics
#> Loading required package: matrixStats
#> 
#> Attaching package: ‘MatrixGenerics’
#> The following objects are masked from ‘package:matrixStats’:
#> 
#>     colAlls, colAnyNAs, colAnys, colAvgsPerRowSet, colCollapse,
#>     colCounts, colCummaxs, colCummins, colCumprods, colCumsums,
#>     colDiffs, colIQRDiffs, colIQRs, colLogSumExps, colMadDiffs,
#>     colMads, colMaxs, colMeans2, colMedians, colMins, colOrderStats,
#>     colProds, colQuantiles, colRanges, colRanks, colSdDiffs, colSds,
#>     colSums2, colTabulates, colVarDiffs, colVars, colWeightedMads,
#>     colWeightedMeans, colWeightedMedians, colWeightedSds,
#>     colWeightedVars, rowAlls, rowAnyNAs, rowAnys, rowAvgsPerColSet,
#>     rowCollapse, rowCounts, rowCummaxs, rowCummins, rowCumprods,
#>     rowCumsums, rowDiffs, rowIQRDiffs, rowIQRs, rowLogSumExps,
#>     rowMadDiffs, rowMads, rowMaxs, rowMeans2, rowMedians, rowMins,
#>     rowOrderStats, rowProds, rowQuantiles, rowRanges, rowRanks,
#>     rowSdDiffs, rowSds, rowSums2, rowTabulates, rowVarDiffs, rowVars,
#>     rowWeightedMads, rowWeightedMeans, rowWeightedMedians,
#>     rowWeightedSds, rowWeightedVars
#> Loading required package: GenomicRanges
#> Loading required package: stats4
#> Loading required package: BiocGenerics
#> Loading required package: generics
#> 
#> Attaching package: ‘generics’
#> The following objects are masked from ‘package:base’:
#> 
#>     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
#>     setequal, union
#> 
#> Attaching package: ‘BiocGenerics’
#> The following objects are masked from ‘package:stats’:
#> 
#>     IQR, mad, sd, var, xtabs
#> The following objects are masked from ‘package:base’:
#> 
#>     Filter, Find, Map, Position, Reduce, anyDuplicated, aperm, append,
#>     as.data.frame, basename, cbind, colnames, dirname, do.call,
#>     duplicated, eval, evalq, get, grep, grepl, is.unsorted, lapply,
#>     mapply, match, mget, order, paste, pmax, pmax.int, pmin, pmin.int,
#>     rank, rbind, rownames, sapply, saveRDS, table, tapply, unique,
#>     unsplit, which.max, which.min
#> Loading required package: S4Vectors
#> 
#> Attaching package: ‘S4Vectors’
#> The following object is masked from ‘package:utils’:
#> 
#>     findMatches
#> The following objects are masked from ‘package:base’:
#> 
#>     I, expand.grid, unname
#> Loading required package: IRanges
#> Loading required package: Seqinfo
#> Loading required package: Biobase
#> Welcome to Bioconductor
#> 
#>     Vignettes contain introductory material; view with
#>     'browseVignettes()'. To cite Bioconductor, see
#>     'citation("Biobase")', and for packages 'citation("pkgname")'.
#> 
#> Attaching package: ‘Biobase’
#> The following object is masked from ‘package:MatrixGenerics’:
#> 
#>     rowMedians
#> The following objects are masked from ‘package:matrixStats’:
#> 
#>     anyMissing, rowMedians
#> 
#> Attaching package: ‘QFeatures’
#> The following object is masked from ‘package:base’:
#> 
#>     sweep
library(QFeaturesGUI)

data("sampleTable")
data("inputTable")

qfeatures <- readQFeatures(
    inputTable,
    colData = sampleTable,
    runCol = "Raw.file"
)
#> Checking arguments.
#> Loading data as a 'SummarizedExperiment' object.
#> Splitting data in runs.
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
#> Formatting sample annotations (colData).
#> Formatting data as a 'QFeatures' object.

app <- processQFeatures(
    qfeatures,
    initialSets = seq_along(qfeatures)
)

if (interactive()) {
    shiny::runApp(app)
}
```
