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
        server_module_summary(
            id = "visualize",
            qfeatures = reactive(qfeatures)
        )
    }

    server
}
