#' Zero to NA tab (section) ui builder
#'
#' @param id The module id
#' @return A shiny tagList object that contains the Zero to NA tab UI components
#' @rdname INTERNAL_interface_module_zero_to_na_tab
#' @keywords internal
#'
#' @importFrom shiny NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools p tagList
#'
interface_module_zero_to_na_tab <- function(id) {
    tagList(
        box(
            title = "Zero to NA",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = TRUE,
            p(
                "Convert all zero intensities into missing values (NA) in the selected sets."
            ),
            uiOutput(NS(id, "zero_to_na_summary"))
        ),
        bs3Tooltip(
            trigger = shiny::actionButton(
                inputId = NS(id, "export"),
                "Save the processed sets",
                icon("hand-pointer", class = "fa-solid"),
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
