#' @title Filtering tab (section) ui builder
#'
#' @param id The module id
#' @param type A character string specifying the type of filtering (`"samples"` or `"features"`)
#'
#' @return A shiny fluidRow object that contains the filtering tab UI components
#' @rdname INTERNAL_interface_module_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboard infoBoxOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList
#'
interface_module_filtering_tab <- function(id, type = c("samples", "features")) {
    type <- match.arg(type)

    number_removed_id <- paste0("number_", type, "_removed")
    percent_removed_id <- paste0("percent_", type, "_removed")

    tagList(
        fluidRow(
            box(
                title = "Pre-Filtering Metrics",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                interface_module_qc_metrics(NS(id, "psm_pre"), type)
            )
        ),
        fluidRow(
            box(
                title = "Filtering",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                actionButton(NS(id, "add_box"), "Add Filtering Condition",
                    width = "100%",
                    class = "load-button no-bottom-margin"
                ),
                actionButton(NS(id, "remove_box"), "Remove Last Condition",
                    width = "100%",
                    class = "suppress-button"
                ),
                box(
                    title = "Filtering Boxes",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    uiOutput(NS(id, "filtering_boxes"))
                ),
                box(
                    title = "Filtering Summary",
                    status = "primary",
                    width = 12,
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    style = "min-height: 180px",
                    uiOutput(NS(id, "filtering_summary")),
                    actionButton(NS(id, "apply_filters"),
                        "Apply Filters",
                        width = "100%",
                        class = "load-button"
                    )
                )
            )
        ),
        fluidRow(
            box(
                title = "Post-Filtering Metrics",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                fluidRow(
                    infoBoxOutput(NS(id, number_removed_id), width = 6),
                    infoBoxOutput(NS(id, percent_removed_id), width = 6)
                ),
                interface_module_qc_metrics(NS(id, "psm_filtered"), type)
            )
        ),
        bs3Tooltip(
            trigger = shiny::actionButton(
                inputId = NS(id, "export"),
                label = "Save the processed sets",
                icon = icon("hand-pointer", class = "fa-solid"),
                width = "100%",
                class = "load-button"
            ),
            tooltipText = paste(
                "Write the processed sets to the QFeatures object.",
                "This is needed to proceed to the next steps."
            ),
            placement = "top"
        )
    )
}
