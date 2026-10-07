# Deprecated functions in QFeaturesGUI

These functions are retained for compatibility with older versions of
QFeaturesGUI. They issue a deprecation warning and forward all arguments
to the replacement function.

## Usage

``` r
importQFeatures(...)

processQFeatures(...)

visualizeQFeatures(...)
```

## Arguments

- ...:

  Arguments passed to the corresponding replacement function.

## Value

A Shiny application object returned by the replacement function.

## Details

The following functions are deprecated:

- `importQFeatures()`: use
  [`import()`](https://rformassspectrometry.github.io/QFeaturesGUI/reference/import.md).

- `processQFeatures()`: use
  [`process()`](https://rformassspectrometry.github.io/QFeaturesGUI/reference/process.md).

- `visualizeQFeatures()`: use
  [`visualise()`](https://rformassspectrometry.github.io/QFeaturesGUI/reference/visualise.md).

They are at the deprecated stage of the Bioconductor deprecation cycle
and may be made defunct in a future release cycle.

## Examples

``` r
# Use import(), process(), and visualise() in new code.
import_app <- suppressWarnings(importQFeatures())
process_app <- suppressWarnings(processQFeatures())
visualise_app <- suppressWarnings(visualizeQFeatures())
```
