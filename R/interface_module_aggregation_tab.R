#' aggregation tab (section) ui builder
#'
#' @return A shiny tagList object that contains the aggregation tab UI components
#' @rdname INTERNAL_interface_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput textOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyjs disabled
#'
interface_module_aggregation_tab <- function(id) {
    tagList(
        fluidRow(
            box(
                title = "Settings",
                status = "primary",
                width = 3,
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput(
                    inputId = NS(id, "method"),
                    label = bs3Tooltip(
                        trigger = "Function to aggregate",
                        tooltipText = "See online documentation for more information about methods."
                    ),
                    choices = c(
                        "robustSummary",
                        "medianPolish",
                        "colMeans",
                        "colMedians",
                        "colSums"
                    ),
                    selected = "medianPolish"
                ),
                br(),
                selectInput(
                    inputId = NS(id, "fcol"),
                    bs3Tooltip(
                        trigger = "rowData variable defining the assay features to aggregate",
                        tooltipText = "Metadata of the assay to aggregate."
                    ),
                    choices = NULL
                ),
                br(),
                actionButton(
                    inputId = NS(id, "aggregate"),
                    label = "Aggregate",
                    width = "100%",
                    class = "load-button"
                ),
                br(),
                tags$h4("Plot options"),
                selectizeInput(
                    inputId = NS(id, "features"),
                    "Feature to plot",
                    choices = NULL
                ),
                selectInput(
                    inputId = NS(id, "color"),
                    label = "Color by",
                    choices = NULL
                ),
                checkboxInput(
                    inputId = NS(id, "addPoints"),
                    label = "Show points",
                    value = TRUE
                )
            ),
            box(
                title = "Aggregation boxplot",
                status = "primary",
                width = 9,
                solidHeader = TRUE,
                collapsible = FALSE,
                div(
                    style = "text-align: center; font-size: 16px; color: #777;",
                    textOutput(NS(id, "pre_boxplot"))
                ),
                uiOutput(NS(id, "aggregation_boxplot_ui"))
            )
        ),
        disabled(
            div(
                id = NS(id, "export_aggregation"),
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
        )
    )
}

#' boxplot box (section) ui builder
#'
#' @return A shiny tagList object that contains the boxplot box UI components
#' @rdname INTERNAL_interface_module_feature_levels_boxplot
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#'

interface_module_feature_levels_boxplot <- function(id) {
    with_output_waiter(
        plotlyOutput(outputId = NS(id, "boxplot")),
        html = waiter::spin_6(),
        color = "transparent"
    )
}
