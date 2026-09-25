#' @title Interface for the qc metrics module
#'
#' @param id module id
#'
#' @return a tagList object that contains the UI for the qc metrics module
#' @rdname INTERNAL_interface_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny fluidRow tagList NS
#' @importFrom shinydashboardPlus box
#'
interface_module_qc_metrics <- function(id, type) {
    tagList(
        interface_module_pca_box(NS(id, "features")),
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
