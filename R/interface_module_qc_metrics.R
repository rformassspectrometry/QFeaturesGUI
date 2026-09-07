#' @title Interface for the qc metrics module
#'
#' @param id module id
#'
#' @return a tagList object that contains the UI for the qc metrics module
#' @rdname INTERNAL_interface_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny fluidRow tagList selectInput NS column
#' @importFrom shinydashboardPlus box boxSidebar
#' @importFrom plotly plotlyOutput
#'
interface_module_qc_metrics <- function(id, type) {
    tagList(
        fluidRow(
            box(
                title = "Settings",
                status = "primary",
                width = 4,
                solidHeader = FALSE,
                collapsible = FALSE,
                selectInput(
                    inputId = NS(id, "assay_type"),
                    choices = c("samples", "features"),
                    label = "Select dimension reduction type",
                    selected = "samples"
                ),
                selectInput(
                    inputId = NS(id, "selected_assay"),
                    choices = NULL,
                    label = "Select the set for dimension reduction"
                ),
                column(
                    width = 6,
                    selectInput(
                        inputId = NS(id, "x_axis"),
                        label = "Component on X axis",
                        choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
                        selected = "PC1"
                    ),
                ),
                column(
                    width = 6,
                    selectInput(
                        inputId = NS(id, "y_axis"),
                        label = "Component on Y axis",
                        choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
                        selected = "PC2"
                    ),
                ),
                selectInput(
                    inputId = NS(id, "pca_color"),
                    label = "Color by",
                    choices = NULL
                ),
                checkboxInput(
                    inputId = NS(id, "scale"),
                    label = "Scale data",
                    value = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "center"),
                    label = "Center data",
                    value = TRUE
                ),
                checkboxInput(
                    inputId = NS(id, "show_legend"),
                    label = "Show Legend",
                    value = FALSE
                ),
                numericInput(
                    inputId = NS(id, "color_width"),
                    label = "Color value max length (chr)",
                    value = 10,
                    min = 5,
                    max = 30
                )
            ),
            box(
                title = "Dimension Reduction (Nipals)",
                status = "primary",
                width = 8,
                solidHeader = FALSE,
                collapsible = FALSE,
                interface_module_pca(
                    NS(id, "features")
                )
            )
        ),
        fluidRow(
            box(
                title = "Single Feature Visualisation",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = FALSE,
                interface_module_viz_box(NS(id, "viz_box"))
            )
        )
    )
}

#' PCA plot interface module
#'
#' @param id module id
#' @return a plotly for PCA
#' @rdname INTERNAL_interface_module_pca_box
#' @keywords internal
#'
#' @importFrom shiny selectInput checkboxInput numericInput NS
#' @importFrom plotly plotlyOutput
#'
interface_module_pca <- function(id) {
    with_output_waiter(plotlyOutput(outputId = NS(id, "pca")),
        html = waiter::spin_6(),
        color = "transparent"
    )
}
