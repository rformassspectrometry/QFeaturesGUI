#' Summary tab for the interface module
#'
#' @param id the id of the module
#' @return a box with the summary tab
#'
#' @rdname INTERNAL_interface_module_summary_tab
#' @keywords internal
#' @importFrom shinydashboardPlus box
#' @importFrom DT dataTableOutput
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
interface_module_summary_tab <- function(id) {
    box(
        title = "QFeatures Summary",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        with_output_waiter(DT::dataTableOutput(NS(id, "qfeatures_dt")),
            html = waiter::spin_6(),
            color = "transparent"
        ),
        with_output_waiter(DT::dataTableOutput(NS(id, "assay_table")),
            html = waiter::spin_6(),
            color = "transparent"
        ),
        box(
            title = "Visual Summary",
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            width = 12,
            with_output_waiter(plotlyOutput(NS(id, "qfeatures_plot")),
                html = waiter::spin_6(),
                color = "transparent"
            )
        ),
        box(
            title = "Dimension reduction",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            box(
                title = "Settings",
                status = "primary",
                width = 4,
                solidHeader = FALSE,
                collapsible = FALSE,
                selectInput(
                    inputId = NS(id, "pca_type"),
                    choices = c("samples", "features"),
                    label = "Select dimension reduction type",
                    selected = "samples"
                ),
                selectInput(
                    inputId = NS(id, "selected_set"),
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
                    NS(id, "summary_pca")
                )
            )
        ),
        downloadButton(
            outputId = NS(id, "download_qfeatures"),
            "Download QFeatures",
            class = "load-button",
            style = "width: 100%;"
        )
    )
}
