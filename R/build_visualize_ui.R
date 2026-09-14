#' UI builder for visualizeQFeatures
#'
#' @return A shiny dashboard UI for the visualizeQFeatures app
#' @rdname INTERNAL_build_visualize_ui
#' @keywords internal
#'
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboardPlus dashboardSidebar
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter useWaiter
build_visualize_ui <- function() {
    ui <- dashboardPage(
        skin = "blue",
        header = header("visualizeQFeatures"),
        sidebar = dashboardSidebar(disable = TRUE, minified = FALSE, width = 0),
        body = dashboardBody(
            useShinyjs(),
            waiter::useWaiter(),
            interface_module_summary(id = "visualize")
        ),
        scrollToTop = TRUE
    )
    ui
}
