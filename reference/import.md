# A shiny app to import QFeatures objects.

import is a simple graphical interface to import bulk and single-cell
proteomics data. The app uses the
[`readQFeatures`](https://rformassspectrometry.github.io/QFeatures/reference/readQFeatures.html)
function from the QFeatures package to convert simple tables (single or
multiple, CSV or TSV) to a QFeatures object. The app allows users to
convert tables to a QFeatures object.

## Usage

``` r
import(colData = NULL, assayData = NULL, maxSize = 1000)
```

## Arguments

- colData:

  A data frame that contains the sample table.

- assayData:

  A data frame that contains the input table.

- maxSize:

  An integer that changes the shiny.maxRequestSize value, in MB.

## Value

The "import" Shiny app object.

## Examples

``` r
library(QFeaturesGUI)

data("sampleTable")
data("inputTable")
app <- import(colData = sampleTable, assayData = inputTable, maxSize = 100)

if (interactive()) {
    shiny::runApp(app)
}
```
