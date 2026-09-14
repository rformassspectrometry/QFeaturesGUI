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
            interface_module_pca_box(NS(id, "summary_pca"))
        ),
        downloadButton(
            outputId = NS(id, "download_qfeatures"),
            "Download QFeatures",
            class = "load-button",
            style = "width: 100%;"
        )
    )
}
