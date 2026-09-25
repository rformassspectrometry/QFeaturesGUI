build_visualize_server <- function(qfeatures, has_qfeatures) {
    server <- function(input, output, session) {
        global_rv$exception_data <- data.frame(
            id = character(),
            title = character(),
            type = character(),
            func_call = character(),
            message = character(),
            full_message = character(),
            time = as.POSIXct(character()),
            stringsAsFactors = FALSE
        )
        server_exception_menu(input, output, session)
        current_qfeatures <- shiny::reactiveVal(
            if (has_qfeatures) qfeatures else NULL
        )
        server_module_summary(
            id = "visualize",
            qfeatures = current_qfeatures
        )

        output$startup_upload_ui <- shiny::renderUI({
            if (is.null(current_qfeatures())) {
                shiny::actionButton(
                    "startup_show_upload",
                    "Load QFeatures",
                    class = "btn-primary"
                )
            }
        })

        server_qfeatures_startup(
            input, output, session,
            app_name = "visualizeQFeatures",
            has_qfeatures = has_qfeatures,
            on_load = function(uploaded, initial_idx) {
                current_qfeatures(uploaded)
            }
        )
    }

    server
}
