#' UI builder for the processQFeatures app
#'
#' @param initial_steps prefilled workflow steps
#' @return A shiny dashboard UI
#' @rdname INTERNAL_build_process_ui
#' @keywords internal
#'
#' @importFrom shinydashboard dashboardBody tabItem tabItems
#' @importFrom shinydashboard  sidebarMenu menuItem menuItemOutput
#' @importFrom shinydashboardPlus dashboardPage dashboardSidebar
#' @importFrom htmltools includeCSS
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinyalert useShinyalert
#' @importFrom shiny icon downloadButton
#' @importFrom waiter useWaiter
#' @importFrom shinyjs useShinyjs
build_process_ui <- function(initial_steps) {
    ui <- dashboardPage(
        skin = "blue",
        header = header("processQFeatures"),
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem("Workflow Config",
                    icon = icon("screwdriver-wrench"),
                    tabName = "workflow_config_tab"
                ),
                menuItemOutput("dynamic_sidebar"),
                menuItem("Summary",
                    icon = icon("readme"), # table
                    tabName = "summary_tab"
                )
            ),
            width = 272,
            minified = FALSE
        ),
        body = dashboardBody(
            useShinyjs(),
            useShinyFeedback(),
            useShinyalert(force = TRUE),
            waiter::useWaiter(),
            includeCSS(system.file(package = "QFeaturesGUI", "www", "style.css")),
            tabItems(
                tabItem(
                    tabName = "workflow_config_tab",
                    interface_module_workflow_config_tab(
                        "workflow_config",
                        initial_steps
                    )
                ),
                tabItem(
                    tabName = "summary_tab",
                    interface_module_summary(
                        "summary_tab",
                        downloadButton(
                            outputId = "summary_tab-download_qfeatures",
                            "Download QFeatures",
                            class = "load-button",
                            style = "width: 100%;"
                        )
                    )
                ),
                # Preconstructed workflow step tabs
                tabItem(tabName = "step_1", uiOutput("dynamic_step_ui_1")),
                tabItem(tabName = "step_2", uiOutput("dynamic_step_ui_2")),
                tabItem(tabName = "step_3", uiOutput("dynamic_step_ui_3")),
                tabItem(tabName = "step_4", uiOutput("dynamic_step_ui_4")),
                tabItem(tabName = "step_5", uiOutput("dynamic_step_ui_5")),
                tabItem(tabName = "step_6", uiOutput("dynamic_step_ui_6")),
                tabItem(tabName = "step_7", uiOutput("dynamic_step_ui_7")),
                tabItem(tabName = "step_8", uiOutput("dynamic_step_ui_8")),
                tabItem(tabName = "step_9", uiOutput("dynamic_step_ui_9")),
                tabItem(tabName = "step_10", uiOutput("dynamic_step_ui_10")),
                tabItem(tabName = "step_11", uiOutput("dynamic_step_ui_11")),
                tabItem(tabName = "step_12", uiOutput("dynamic_step_ui_12")),
                tabItem(tabName = "step_13", uiOutput("dynamic_step_ui_13")),
                tabItem(tabName = "step_14", uiOutput("dynamic_step_ui_14")),
                tabItem(tabName = "step_15", uiOutput("dynamic_step_ui_15")),
                tabItem(tabName = "step_16", uiOutput("dynamic_step_ui_16")),
                tabItem(tabName = "step_17", uiOutput("dynamic_step_ui_17")),
                tabItem(tabName = "step_18", uiOutput("dynamic_step_ui_18")),
                tabItem(tabName = "step_19", uiOutput("dynamic_step_ui_19")),
                tabItem(tabName = "step_20", uiOutput("dynamic_step_ui_20"))
            )
        ),
        title = "processQFeatures",
        scrollToTop = TRUE
    )

    return(ui)
}
