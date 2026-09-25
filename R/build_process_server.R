#' Server Builder for the processQFeatures app
#'
#' @param qfeatures a `QFeatures` object given by the user
#' @param initial_sets index of the base sets of the QFeatures
#' @param initial_steps prefilled workflow steps
#' @param has_qfeatures `logical(1)` indicating whether the app was launched
#'   with an initial QFeatures object
#'
#' @return return the server function for the processQFeatures app.
#' @rdname INTERNAL_build_process_server
#' @keywords internal
#'
#' @importFrom QFeatures QFeatures
#' @importFrom shiny observeEvent observe reactiveVal downloadHandler
#' @importFrom shinydashboard updateTabItems
#' @importFrom shinyalert shinyalert
#'
build_process_server <- function(qfeatures, initial_sets, initial_steps, has_qfeatures = TRUE) {
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
        .qf$qfeatures <- if (has_qfeatures) {
            format_qfeatures(qfeatures, initial_sets)
        } else {
            NULL
        }
        global_rv$workflow_config <- if (has_qfeatures) initial_steps else character(0)
        global_rv$code_lines <- list()
        global_rv$step_rvs <- list()
        server_exception_menu(input, output, session)
        server_sidebar(input, output, session)
        server_module_workflow_config("workflow_config")
        server_dynamic_workflow(input, output, session)
        # Adapt the process app's non-reactive store and workflow notifications
        # to the reactive QFeatures input expected by the summary module.
        summary_qfeatures <- reactiveVal(.qf$qfeatures)
        observe({
            lapply(global_rv$step_rvs, function(rv) rv())
            summary_qfeatures(.qf$qfeatures)
        })
        server_module_summary(
            "summary_tab",
            qfeatures = summary_qfeatures,
            assay_labels = remove_QFeaturesGUI
        )

        output[["summary_tab-download_qfeatures"]] <- downloadHandler(
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

        server_qfeatures_startup(
            input, output, session,
            app_name = "processQFeatures",
            has_qfeatures = has_qfeatures,
            select_initial_sets = TRUE,
            on_load = function(uploaded, initial_idx) {
                workflow_steps <- input[["workflow_config-workflow_list"]]
                if (is.null(workflow_steps)) {
                    workflow_steps <- initial_steps
                }

                .qf$qfeatures <- format_qfeatures(uploaded, initial_idx)
                summary_qfeatures(.qf$qfeatures)
                global_rv$workflow_config <- workflow_steps
                global_rv$code_lines <- list()

                n_sets <- length(initial_idx)
                n_steps <- length(workflow_steps)
                shinyalert(
                    title = "QFeatures loaded",
                    text = paste0(
                        "Loaded QFeatures with ", n_sets,
                        " initial set", if (n_sets != 1) "s" else "", ".",
                        if (n_steps > 0) {
                            paste0(
                                "\nWorkflow pre-configured with ", n_steps,
                                " step", if (n_steps != 1) "s" else "", "."
                            )
                        } else {
                            ""
                        }
                    ),
                    closeOnClickOutside = TRUE,
                    type = "success",
                    confirmButtonCol = "#3c8dbc"
                )

                invisible(NULL)
            }
        )

        if (!has_qfeatures) {
            return(invisible(NULL))
        }

        n_sets <- length(initial_sets)
        n_steps <- length(initial_steps)
        shinyalert(
            title = "App ready",
            text = paste0(
                "Loaded QFeatures with ", n_sets,
                " initial set", if (n_sets != 1) "s" else "", ".",
                if (n_steps > 0) {
                    paste0(
                        "\nWorkflow pre-configured with ", n_steps,
                        " step", if (n_steps != 1) "s" else "", "."
                    )
                } else {
                    ""
                }
            ),
            closeOnClickOutside = TRUE,
            type = "success",
            confirmButtonCol = "#3c8dbc"
        )
    }

    server
}
