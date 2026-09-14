#' Summary tab module server
#'
#' @param id the id of the module
#' @return a server module for the summary tab
#'
#' @rdname INTERNAL_server_module_summary_tab
#' @keywords internal
#'
#' @importFrom DT renderDataTable datatable
#' @importFrom SummarizedExperiment assay
#' @importFrom shiny moduleServer observe reactive
#' @importFrom plotly renderPlotly plot_ly

server_module_summary_tab <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Reactive that subscribes to every step's reactiveVal.
        # Re-fires whenever any step is saved, invalidating all outputs below.
        any_step_saved <- reactive({
            lapply(global_rv$step_rvs, function(rv) rv())
        })

        pca_assays <- reactive({
            any_step_saved()
            .qf$qfeatures
        })
        qfeatures_df <- reactive({
            any_step_saved()
            error_handler(
                qfeatures_to_df,
                component_name = "qfeatures_to_df",
                .qf$qfeatures
            )
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
            any_step_saved()
            if (!is.null(input$qfeatures_dt_rows_selected)) {
                row <- input$qfeatures_dt_rows_selected
                DT::datatable(
                    data.frame(assay(.qf$qfeatures[[row]])),
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
            any_step_saved()
            if (length(.qf$qfeatures) > 0) {
                empty_qfeatures <- .qf$qfeatures[1, ]
                names(empty_qfeatures) <- remove_QFeaturesGUI(names(empty_qfeatures))
                plot(empty_qfeatures,
                    interactive = TRUE
                )
            }
        })
        server_module_pca_box(
            id = "summary_pca",
            assays_to_process = pca_assays
        )
        output$download_qfeatures <- downloadHandler(
            filename = function() {
                "processQFeatures_files.zip"
            },
            content = function(file) {
                with_task_loader(
                    caption = "Preparing download, can be quite time consuming",
                    expr = {
                        tmpdir <- tempdir()
                        final_qfeatures <- .qf$qfeatures
                        names(final_qfeatures) <- remove_QFeaturesGUI(names(final_qfeatures))
                        rds_file <- file.path(tmpdir, "processQFeatures_QFeatures_object.rds")
                        saveRDS(final_qfeatures, rds_file)
                        rmd_file <- file.path(tmpdir, "sessionInfo.Rmd")
                        SI_file <- file.path(tmpdir, "processQFeatures_sessionInfo.html")
                        r_file <- file.path(tmpdir, "processQFeatures_script.R")
                        writeLines(
                            c(
                                "---",
                                "title : \"SessionInfo\"",
                                "output: html_document",
                                "---",
                                "",
                                "```{r}",
                                "sessionInfo()",
                                "```"
                            ),
                            rmd_file
                        )
                        rmarkdown::render(
                            rmd_file,
                            output_file = SI_file,
                            quiet = TRUE
                        )
                        writeLines(
                            c(
                                "# Reproducible R script",
                                paste0("# Generated on: ", Sys.time()),
                                "",
                                "####################################\n######### Package loading ##########\n####################################\nlibrary(QFeatures)\nlibrary(MsCoreUtils)\n",
                                "####################################\n########## Load dataset ############\n####################################\n## Replace 'myDataset' with the path towards your initial Qfeatures .rds file.\n## Or directly assign your initial QFeatures object to qf.\nqf <- readRDS('myDataset') \n",
                                unlist(global_rv$code_lines)
                            ),
                            r_file
                        )
                        utils::zip(
                            zipfile = file,
                            files = c(rds_file, SI_file, r_file),
                            flags = "-j"
                        )
                    }
                )
            }
        )
    })
}
