# QFeaturesGUI

``` r

library(QFeaturesGUI)
```

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
#> [1] QFeaturesGUI_0.99.3 BiocStyle_2.40.0   
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

Amezquita, Robert A, Aaron T L Lun, Etienne Becht, et al. 2020.
“Orchestrating Single-Cell Analysis with Bioconductor.” *Nat. Methods*
17 (2): 137–45.

Gatto, Laurent. 2020. *QFeatures: Quantitative Features for Mass
Spectrometry Data*.

Vanderaa, Christophe, and Laurent Gatto. 2021. “Replication of
Single-Cell Proteomics Data Reveals Important Computational Challenges.”
*Expert Review of Proteomics*, 1–9.
