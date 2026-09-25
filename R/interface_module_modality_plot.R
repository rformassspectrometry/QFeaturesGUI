#' Modalities plot interface module
#'
#' @param id module id
#' @return A fluid row containing settings and the plot
#' @rdname INTERNAL_interface_module_modality_plot
#' @keywords internal
interface_module_modality_plot <- function(id) {
    fluidRow(
        box(
            title = "Settings",
            status = "primary",
            width = 4,
            solidHeader = FALSE,
            collapsible = FALSE,
            selectInput(NS(id, "selected_assay"),
                label = "Select sets",
                choices = NULL,
                multiple = TRUE
            ),
            selectInput(NS(id, "reference_modality"),
                label = "Select reference modality",
                choices = NULL,
                multiple = FALSE
            ),
            selectizeInput(NS(id, "featnames"),
                "Select feature to inspect",
                choices = NULL)
        ),
        box(
            title = "Intensity across modality",
            status = "primary",
            width = 8,
            solidHeader = FALSE,
            collapsible = FALSE,
            with_output_waiter(plotlyOutput(outputId = NS(id, "modality_plot")),
                html = waiter::spin_6(),
                color = "transparent"
            )
        )
    )
}

