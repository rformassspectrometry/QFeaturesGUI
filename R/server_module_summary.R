#' QFeatures summary module server
#'
#' @param id the id of the module
#' @param qfeatures A reactive containing the QFeatures object to summarize,
#'   or NULL while no object is available.
#' @param assay_labels A function taking assay names and returning display
#'   labels of the same length. Defaults to [identity()].
#' @return a server module for the QFeatures summary
#'
#' @rdname INTERNAL_server_module_summary
#' @keywords internal
#'
#' @importFrom DT renderDataTable datatable
#' @importFrom SummarizedExperiment assay
#' @importFrom shiny moduleServer reactive is.reactive
#' @importFrom plotly renderPlotly

server_module_summary <- function(id, qfeatures, assay_labels = identity) {
    stopifnot(is.reactive(qfeatures), is.function(assay_labels))
    moduleServer(id, function(input, output, session) {
        qfeatures_df <- reactive({
            qfeatures_to_df(qfeatures(), assay_labels = assay_labels)
        })

        output$qfeatures_dt <- DT::renderDataTable({
            DT::datatable(qfeatures_df(),
                extensions = "FixedColumns",
                selection = "single",
                options = list(
                    searching = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15)
                )
            )
        })

        output$assay_table <- DT::renderDataTable({
            current_qfeatures <- qfeatures()
            row <- input$qfeatures_dt_rows_selected
            if (length(row) == 1L && row %in% seq_along(current_qfeatures)) {
                DT::datatable(
                    data.frame(assay(current_qfeatures[[row]])),
                    extensions = "FixedColumns",
                    options = list(
                        searching = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15, 20)
                    )
                )
            }
        })

        output$qfeatures_plot <- renderPlotly({
            current_qfeatures <- qfeatures()
            if (length(current_qfeatures) > 0L) {
                empty_qfeatures <- current_qfeatures[1, ]
                names(empty_qfeatures) <- assay_labels(names(empty_qfeatures))
                plot(empty_qfeatures,
                    interactive = TRUE
                )
            }
        })
        server_module_pca_box(
            id = "summary_pca",
            assays_to_process = qfeatures,
            assay_labels = assay_labels
        )
        server_module_modality_plot(
            id = "modality_plot",
            assays_to_process = qfeatures,
            assay_labels = assay_labels
        )
    })
}
