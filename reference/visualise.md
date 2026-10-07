# Launch a Shiny application to visualise QFeatures objects

`visualise()` launches an interactive Shiny application that allows
users to visualise a QFeatures object.

The input `qfeatures` can be provided as an in-memory QFeatures object,
as a path to an `.rds` file containing one, or omitted. If omitted, the
application prompts the user to upload a QFeatures object from an `.rds`
file or use the bundled demo dataset.

## Usage

``` r
visualise(qfeatures = NULL, maxSize = 100)
```

## Arguments

- qfeatures:

  Optional QFeatures object to visualise, or a character string
  specifying the path to an `.rds` file containing one. If omitted or
  `NULL`, the app displays a startup modal for uploading a file or
  loading the bundled demo.

- maxSize:

  An integer that changes the `shiny.maxRequestSize` value, in MB. This
  controls the maximum upload size for the startup `.rds` file upload
  modal.

## Value

The visualise Shiny application.

## Examples

``` r

library(QFeaturesGUI)


app <- visualise()

if (interactive()) {
    shiny::runApp(app)
}
```
