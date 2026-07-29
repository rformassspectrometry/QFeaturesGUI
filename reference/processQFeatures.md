# Launch a Shiny application to process QFeatures objects

`processQFeatures()` launches an interactive Shiny application that
allows users to visually configure and apply pre-processing workflows to
a QFeatures object.

The input `qfeatures` can be provided as an in-memory QFeatures object,
as a path to an `.rds` file containing one, or omitted. If omitted, the
application prompts the user to upload a QFeatures object from an `.rds`
file.

## Usage

``` r
processQFeatures(
  qfeatures = NULL,
  initialSets = NULL,
  prefilledSteps = c("sampleFiltering", "featureFiltering", "missingValuesFeatures",
    "missingValuesSamples", "normalisation", "aggregation", "join", "aggregation"),
  maxSize = 100
)
```

## Arguments

- qfeatures:

  Optional QFeatures object to be processed, or a character string
  specifying the path to a `.rds` file containing a QFeatures object. If
  omitted, the app starts without processing steps and displays a
  startup modal.

- initialSets:

  An integer, logical, or character vector specifying which assays
  (feature sets) should be used as the starting point for processing. If
  `NULL` and `qfeatures` is provided, all assays in `qfeatures` are
  used. If `qfeatures` is omitted, the user chooses the initial sets
  after uploading the `.rds` file.

- prefilledSteps:

  A character vector specifying the initial workflow steps to display
  when the application launches. Steps must be provided using their
  internal identifiers (e.g. `"sampleFiltering"`, `"featureFiltering"`,
  `"normalisation"`).

- maxSize:

  An integer that changes the `shiny.maxRequestSize` value, in MB. This
  controls the maximum upload size for the startup `.rds` file upload
  modal.

## Value

The processQFeatures Shiny application.

## Details

The application provides a drag-and-drop workflow builder that allows
users to select, order, and configure processing steps such as
filtering, normalization, and transformation. The configured workflow
can then be applied to the selected assays.

## Examples

``` r

library(QFeaturesGUI)


app <- processQFeatures()

if (interactive()) {
    shiny::runApp(app)
}
```
