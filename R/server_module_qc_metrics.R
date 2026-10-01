#' Server module for qc metrics box
#'
#' @param id module id
#' @param assays_to_process a reactiveVal that contains the different assays that will be used in the module
#' @return The different assays that will be processed in the page
#' @rdname INTERNAL_server_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny moduleServer is.reactive
#'
server_module_qc_metrics <- function(id, assays_to_process) {
    stopifnot(is.reactive(assays_to_process))
    moduleServer(id, function(input, output, session) {
        server_module_pca_box(
            id = "features",
            assays_to_process = assays_to_process,
            assay_labels = remove_QFeaturesGUI
        )
        server_module_viz_box("viz_box", assays_to_process)
    })
}
