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

        uploaded_qfeatures <- shiny::reactiveVal(NULL)
        upload_message <- shiny::reactiveVal(NULL)
        startup_reading <- shiny::reactiveVal(FALSE)

        output$startup_initial_sets_label <- shiny::renderText({
            uploaded <- uploaded_qfeatures()
            if (is.null(uploaded)) {
                return("Initial sets")
            }

            selected_sets <- input$startup_initial_sets
            if (is.null(selected_sets)) {
                selected_sets <- names(uploaded)
            }

            paste0("Initial sets (", length(selected_sets), " selected)")
        })

        output$startup_initial_sets_ui <- shiny::renderUI({
            uploaded <- uploaded_qfeatures()
            if (startup_reading()) {
                return(NULL)
            }
            if (is.null(uploaded)) {
                return(shiny::p(
                    "Upload an .rds file containing a QFeatures object to",
                    "choose the initial sets."
                ))
            }

            shiny::tagList(
                shiny::tags$label(
                    `for` = "startup_initial_sets",
                    class = "control-label",
                    shiny::textOutput("startup_initial_sets_label", inline = TRUE)
                ),
                shiny::selectizeInput(
                    "startup_initial_sets",
                    NULL,
                    choices = names(uploaded),
                    selected = names(uploaded),
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                        plugins = list("remove_button"),
                        placeholder = "Choose one or more initial sets"
                    )
                )
            )
        })

        output$startup_upload_message <- shiny::renderUI({
            msg <- upload_message()
            if (is.null(msg)) {
                return(NULL)
            }
            shiny::tags$div(class = "text-danger", msg)
        })

        output$startup_read_status <- shiny::renderUI({
            if (!startup_reading()) {
                return(NULL)
            }

            shiny::tags$div(
                class = "qfeatures-startup-read-status",
                shiny::tags$div(
                    class = "progress",
                    shiny::tags$div(
                        class = "progress-bar progress-bar-striped active",
                        role = "progressbar",
                        style = "width: 100%;"
                    )
                ),
                shiny::tags$p(
                    shiny::tags$em(
                        "Reading QFeatures object. This can take some time for large files."
                    )
                )
            )
        })

        show_startup_upload_modal <- function() {
            shiny::showModal(shiny::modalDialog(
                title = "Load a QFeatures object",
                shiny::p(
                    "processQFeatures was started without a QFeatures object.",
                    "Upload an .rds file and choose initial sets, or start",
                    "with the bundled demo."
                ),
                shiny::fileInput(
                    "startup_qfeatures_rds",
                    "QFeatures RDS file",
                    accept = c(".rds", ".Rds", ".RDS")
                ),
                shiny::uiOutput("startup_initial_sets_ui"),
                shiny::uiOutput("startup_read_status"),
                shiny::uiOutput("startup_upload_message"),
                easyClose = FALSE,
                size = "l",
                footer = shiny::tagList(
                    shiny::modalButton("Cancel"),
                    shiny::actionButton(
                        "startup_use_demo_qfeatures",
                        "Use demo QFeatures",
                        class = "btn-default"
                    ),
                    shiny::actionButton(
                        "startup_load_qfeatures",
                        "Load QFeatures",
                        class = "btn-primary"
                    )
                )
            ))
        }

        load_startup_qfeatures <- function(uploaded, selected_sets, workflow_steps) {
            initial_idx <- tryCatch(
                normalise_initial_sets(uploaded, selected_sets),
                error = function(e) e
            )
            if (inherits(initial_idx, "error")) {
                upload_message(conditionMessage(initial_idx))
                return(invisible(NULL))
            }

            if (is.null(workflow_steps)) {
                workflow_steps <- initial_steps
            }

            .qf$qfeatures <- format_qfeatures(uploaded, initial_idx)
            summary_qfeatures(.qf$qfeatures)
            global_rv$workflow_config <- workflow_steps
            global_rv$code_lines <- list()
            shiny::removeModal()

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

        shiny::observeEvent(input$startup_qfeatures_rds,
            {
                uploaded_qfeatures(NULL)
                upload_message(NULL)
                startup_reading(TRUE)
                datapath <- input$startup_qfeatures_rds$datapath

                session$onFlushed(function() {
                    uploaded <- tryCatch(
                        check_qfeatures(datapath),
                        error = function(e) e
                    )
                    startup_reading(FALSE)
                    if (inherits(uploaded, "error")) {
                        upload_message(paste(
                            "Could not load QFeatures object:",
                            conditionMessage(uploaded)
                        ))
                        return(invisible(NULL))
                    }

                    uploaded_qfeatures(uploaded)
                }, once = TRUE)
            },
            ignoreInit = TRUE
        )

        shiny::observeEvent(input$startup_load_qfeatures,
            {
                uploaded <- uploaded_qfeatures()
                if (is.null(uploaded)) {
                    upload_message(
                        "Upload a valid .rds file containing a QFeatures object."
                    )
                    return(invisible(NULL))
                }

                workflow_steps <- input[["workflow_config-workflow_list"]]
                load_startup_qfeatures(
                    uploaded,
                    input$startup_initial_sets,
                    workflow_steps
                )
            },
            ignoreInit = TRUE
        )

        shiny::observeEvent(input$startup_use_demo_qfeatures,
            {
                uploaded_qfeatures(NULL)
                upload_message(NULL)
                startup_reading(TRUE)
                workflow_steps <- input[["workflow_config-workflow_list"]]

                session$onFlushed(function() {
                    demo_qfeatures <- tryCatch(
                        demo_process_qfeatures(),
                        error = function(e) e
                    )
                    startup_reading(FALSE)
                    if (inherits(demo_qfeatures, "error")) {
                        upload_message(paste(
                            "Could not create demo QFeatures object:",
                            conditionMessage(demo_qfeatures)
                        ))
                        return(invisible(NULL))
                    }

                    uploaded_qfeatures(demo_qfeatures)
                    load_startup_qfeatures(
                        demo_qfeatures,
                        names(demo_qfeatures),
                        workflow_steps
                    )
                }, once = TRUE)
            },
            ignoreInit = TRUE
        )

        shiny::observeEvent(input$startup_show_upload,
            {
                show_startup_upload_modal()
            },
            ignoreInit = TRUE
        )

        if (!has_qfeatures) {
            session$onFlushed(function() {
                show_startup_upload_modal()
            }, once = TRUE)
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
