# QFeaturesGUI

## QFeaturesGUI

`QFeaturesGUI` is a collection of Shiny applications that provide
graphical user interfaces for the analysis of MS-based proteomics data
using the Bioconductor ecosystem. It builds on the functionality of the
[`QFeatures`](https://rformassspectrometry.github.io/QFeatures/) and
[`scp`
package](https://bioconductor.org/packages/release/bioc/html/scp.html)
(Vanderaa and Gatto (2021)), offering user-friendly tools for both
**bulk** and **single-cell proteomics (SCP)** data analysis.

The suite leverages the power of **Shiny** and **shinydashboardPlus** to
deliver interactive, modular applications. Rather than a single
monolithic app, `QFeaturesGUI` is composed of multiple applications,
each dedicated to a specific aspect of proteomics data analysis. These
applications are documented in separate vignettes, making it easy for
users to navigate and find information relevant to their needs.

Currently available applications include:

- Data import
- Data processing

Additional applications will be added in future releases.

## The `QFeatures` and `scp` Packages

`QFeaturesGUI` serves as a graphical interface to core Bioconductor data
infrastructures for proteomics. To use the GUI applications effectively,
it is recommended to understand how the underlying packages handle
proteomics data. This section provides a brief overview; refer to the
respective package documentation for more detailed information.

### The `QFeatures` Package

The [`QFeatures`](https://rformassspectrometry.github.io/QFeatures/)
package is a data framework specifically designed to manipulate and
process **MS-based quantitative proteomics data**, with a primary focus
on **bulk proteomics** workflows (Gatto (2020)).

It preserves the relationships between different levels of information,
such as peptide to spectrum match (PSM) data, peptide data, and protein
data. Additionally, the `QFeatures` package provides an interface to
various utility functions that streamline common MS data processing
tasks.

For further details on MS data analysis tools, refer to the
[RforMassSpectrometry project](https://www.rformassspectrometry.org/).

### The `scp` Package

The [`scp`
package](https://bioconductor.org/packages/release/bioc/html/scp.html)
extends `QFeatures` to support **single-cell proteomics (SCP)** data
analysis (Vanderaa and Gatto (2021)). It operates on a specialized data
structure that wraps `QFeatures` objects around
[`SingleCellExperiment`](http://bioconductor.org/packages/release/bioc/html/SingleCellExperiment.md)
objects (Amezquita et al. (2020)).

This data structure can be conceptualized as Matryoshka dolls, where
`SingleCellExperiment` objects are the smaller dolls contained within
the larger `QFeatures` object.

The `SingleCellExperiment` class provides a dedicated framework for
single-cell data, acting as an interface to cutting-edge methods for
processing, visualizing, and analyzing single-cell data. By combining
`SingleCellExperiment` and `QFeatures`, the `scp` package enables
principled handling of SCP-specific challenges while maintaining
compatibility with the broader MS-based proteomics ecosystem.

## Before You Start

Before diving into proteomics data analysis using `QFeaturesGUI`, it is
important to note that these applications act as interfaces to the
underlying `QFeatures` and `scp` packages.

When errors occur during an analysis, `QFeaturesGUI` reports the
corresponding error messages in a dropdown menu at the top right of the
application interface. These messages originate from the underlying
package functions. Users are therefore encouraged to consult the
documentation of the associated `QFeatures` or `scp` functions to
identify and resolve issues.

The error messages provide valuable clues, and searching the package
documentation will often help users troubleshoot effectively.

## Installation and Launch

### Installation

``` r

# Check if remotes is installed. Otherwise install it.
if (!require("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
# Install the package
remotes::install_github("rformassspectrometry/QFeaturesGUI")
# Load the package
library(QFeaturesGUI)
```

### Application launch

`QFeaturesGUI` is composed of multiple Shiny applications, each
dedicated to a specific step of the proteomics data analysis workflow.
Applications are launched independently, depending on the task to be
performed.

For example, the application dedicated to importing data into a
`QFeatures` object can be launched as follows:

``` r

app <- importQFeatures()

if (interactive()) {
    shiny::runApp(app)
}
```

Similarly, the application dedicated to data processing can be launched
using:

``` r

app <- processQFeatures(qfeaturesObject)

if (interactive()) {
    shiny::runApp(app)
}
```

## Applications overview

Rather than a single application with multiple sections, QFeaturesGUI
provides a set of dedicated applications. Each application addresses one
step of the proteomics workflow and exposes functionality through a
focused graphical interface.

The use of each application is described in a corresponding vignette:

- **Data import**: `importQFeatures`  
  See the [importQFeatures
  vignette](https://rformassspectrometry.github.io/QFeaturesGUI/articles/importQFeatures.md)

- **Data processing**: `processQFeatures`  
  See the [processQFeatures
  vignette](https://rformassspectrometry.github.io/QFeaturesGUI/articles/processQFeatures.md)

Additional applications will be introduced in future releases and
documented in their own vignettes.

## Citation

## References

Amezquita, Robert A, Aaron T L Lun, Etienne Becht, et al. 2020.
“Orchestrating Single-Cell Analysis with Bioconductor.” *Nat. Methods*
17 (2): 137–45.

Gatto, Laurent. 2020. *QFeatures: Quantitative Features for Mass
Spectrometry Data*.

Vanderaa, Christophe, and Laurent Gatto. 2021. “Replication of
Single-Cell Proteomics Data Reveals Important Computational Challenges.”
*Expert Review of Proteomics*, 1–9.
