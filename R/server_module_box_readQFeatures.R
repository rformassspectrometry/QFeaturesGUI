#' A server module that contains the server logic to the readQFeatures box module
#'
#' @param id module id
#' @param input_table a reactiveVal containing the input table
#' @param sample_table a reactiveVal containing the sample table
#'
#' @return A QFeatures object
#' @rdname INTERNAL_box_readqfeatures_server
#' @keywords internal
#'
#' @importFrom shiny is.reactive reactive moduleServer observe eventReactive updateSelectInput removeModal downloadHandler
#' @importFrom DT renderDataTable datatable
#' @importFrom QFeatures readQFeatures setQFeaturesType getQFeaturesType
#' @importFrom QFeatures zeroIsNA
#' @importFrom methods as
#' @importFrom utils zip
#' @importFrom shinyjs enable show
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom SummarizedExperiment SummarizedExperiment
#' @importFrom MultiAssayExperiment MultiAssayExperiment ExperimentList
#' @importFrom shinydashboard infoBox renderInfoBox
#' @importFrom rmarkdown render
#'
box_readqfeatures_server <- function(id, input_table, sample_table) {
    stopifnot(is.reactive(input_table))
    stopifnot(is.reactive(sample_table))
    moduleServer(id, function(input, output, session) {
        observe({
            if (!is.null(input_table())) {
                shinyjs::enable("convert")
            }
        })
        observeEvent(input$convert, {
            shinyjs::show("qfeatures_preview_box")
            shinyjs::show("download_qfeatures_object")
        })
        qfeatures <- eventReactive(input$convert, {
            with_task_loader(
                caption = "Be aware that the conversion into QFeatures object can be quite time consuming for large datasets",
                expr = {
                    if (is.data.frame(sample_table())) {
                        colData <- sample_table()
                        quantCols <- NULL
                    } else {
                        colData <- NULL
                        quantCols <- input$quant_cols
                    }

                    if (input$run_col != "NULL") {
                        runCol <- input$run_col
                    } else {
                        runCol <- NULL
                    }
                    qfeatures <- error_handler(
                        QFeatures::readQFeatures,
                        component_name = "QFeatures converting (readQFeatures)",
                        assayData = input_table(),
                        colData = colData,
                        runCol = runCol,
                        quantCols = quantCols,
                        removeEmptyCols = input$removeEmptyCols,
                        verbose = FALSE
                    )
                    if (input$zero_as_NA && length(qfeatures) > 0) {
                        qfeatures <- error_handler(
                            QFeatures::zeroIsNA,
                            component_name = "QFeatures converting (zero_as_NA)",
                            object = qfeatures,
                            i = seq_along(qfeatures)
                        )
                    }
                    if (input$logTransform) {
                        qfeatures <- error_handler(
                            log_transform_existing_sets,
                            component_name = "Log transforming QFeatures",
                            object = qfeatures,
                            i = seq_along(qfeatures),
                            base = 2
                        )
                    }
                    if (input$singlecell) {
                        qfeatures <- setQFeaturesType(qfeatures, type = "scp")
                    }
                    names(qfeatures) <- paste0(
                        names(qfeatures),
                        "_(QFeaturesGUI#0)_initial_import"
                    )
                    shinyjs::show("selected_assay_preview_box")
                    qfeatures
                }
            )
        })

        observe({
            updateSelectInput(session,
                "run_col",
                choices = c("NULL", colnames(input_table())),
                selected = "NULL"
            )
            updateSelectInput(session,
                "quant_cols",
                choices = colnames(input_table())
            )
        })

        qfeatures_df <- reactive({
            req(qfeatures())
            error_handler(
                qfeatures_to_df,
                component_name = "qfeatures_to_df",
                page_assays_subset(qfeatures(), "_(QFeaturesGUI#0)"),
                assay_labels = remove_QFeaturesGUI
            )
        })

        output$type_of_qfeatures <- renderInfoBox({
            req(qfeatures())
            infoBox("Type of QFeatures :", getQFeaturesType(qfeatures()), fill = TRUE, color = "light-blue")
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
            if (!is.null(input$qfeatures_dt_rows_selected)) {
                row <- input$qfeatures_dt_rows_selected
                DT::datatable(
                    data.frame(assay(qfeatures()[[row]])),
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
        observeEvent(input$convert, {
            global_rv$code_lines$create_qfeatures <- code_generator_read_qfeatures(
                input_table,
                sample_table,
                qfeatures(),
                input$run_col,
                input$removeEmptyCols,
                input$quant_cols,
                input$logTransform,
                input$zero_as_NA,
                input$singlecell
            )
        })
        output$downloadQFeatures <- downloadHandler(
            filename = function() {
                "importQFeatures_files.zip"
            },
            content = function(file) {
                with_task_loader(
                    caption = "Preparing download, can be quite time consuming",
                    expr = {
                        tmpdir <- tempdir()
                        final_qfeatures <- qfeatures()
                        names(final_qfeatures) <- remove_QFeaturesGUI(names(final_qfeatures))
                        rds_file <- file.path(tmpdir, "importQFeatures_QFeatures_object.rds")
                        saveRDS(final_qfeatures, rds_file)
                        rmd_file <- file.path(tmpdir, "importQFeatures_sessionInfo.Rmd")
                        SI_file <- file.path(tmpdir, "importQFeatures_sessionInfo.html")
                        r_file <- file.path(tmpdir, "importQFeatures_script.R")
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
                                "library(QFeatures)\n"
                            ),
                            r_file
                        )
                        write(
                            c(
                                global_rv$code_lines$read_input_data
                            ),
                            file = r_file,
                            append = TRUE
                        )
                        if (!is.null(sample_table())) {
                            write(
                                c(
                                    global_rv$code_lines$read_sample_data
                                ),
                                file = r_file,
                                append = TRUE
                            )
                        }
                        write(
                            c(
                                "# Create QFeatures object",
                                global_rv$code_lines$create_qfeatures
                            ),
                            file = r_file,
                            append = TRUE
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
