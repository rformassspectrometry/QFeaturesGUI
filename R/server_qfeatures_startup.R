#' Shared startup loader for QFeatures applications
#'
#' @param input,output,session The parent Shiny server objects.
#' @param app_name Application name displayed in the startup modal.
#' @param has_qfeatures Whether an initial QFeatures object was supplied.
#' @param on_load Callback receiving the loaded QFeatures object and integer
#'   assay indices. Called in an isolated reactive context.
#' @param select_initial_sets Whether to let users select initial assays.
#'
#' @return No return value; registers startup outputs and observers.
#' @keywords internal
#' @noRd
server_qfeatures_startup <- function(
      input, output, session, app_name, has_qfeatures, on_load,
      select_initial_sets = FALSE
) {
    uploaded_qfeatures <- shiny::reactiveVal(NULL)
    upload_message <- shiny::reactiveVal(NULL)
    startup_reading <- shiny::reactiveVal(FALSE)

    if (select_initial_sets) {
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
                    "for" = "startup_initial_sets",
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
    }

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
            shiny::tags$p(shiny::tags$em(
                "Reading QFeatures object. This can take some time for large files."
            ))
        )
    })

    show_startup_upload_modal <- function() {
        shiny::showModal(shiny::modalDialog(
            title = "Load a QFeatures object",
            shiny::p(
                paste0(app_name, " was started without a QFeatures object."),
                if (select_initial_sets) {
                    "Upload an .rds file and choose initial sets, or start with the bundled demo."
                } else {
                    "Upload an .rds file, or start with the bundled demo."
                }
            ),
            shiny::fileInput(
                "startup_qfeatures_rds",
                "QFeatures RDS file",
                accept = c(".rds", ".Rds", ".RDS")
            ),
            if (select_initial_sets) {
                shiny::uiOutput("startup_initial_sets_ui")
            },
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
        ), session = session)
    }

    load_startup_qfeatures <- function(uploaded, selected_sets = names(uploaded)) {
        result <- tryCatch({
            initial_idx <- if (select_initial_sets) {
                normalise_initial_sets(uploaded, selected_sets)
            } else {
                seq_along(uploaded)
            }
            # Demo loading runs after a flush, outside a reactive context.
            shiny::isolate(on_load(uploaded, initial_idx))
            NULL
        }, error = function(e) e)

        if (inherits(result, "error")) {
            upload_message(conditionMessage(result))
            return(invisible(NULL))
        }
        upload_message(NULL)
        shiny::removeModal(session = session)
        invisible(NULL)
    }

    read_startup_qfeatures <- function(reader, use_demo = FALSE) {
        uploaded_qfeatures(NULL)
        upload_message(NULL)
        startup_reading(TRUE)

        # Flush the progress indicator before reading a potentially large file.
        session$onFlushed(function() {
            uploaded <- tryCatch(reader(), error = function(e) e)
            startup_reading(FALSE)
            if (inherits(uploaded, "error")) {
                upload_message(paste(
                    if (use_demo) {
                        "Could not create demo QFeatures object:"
                    } else {
                        "Could not load QFeatures object:"
                    },
                    conditionMessage(uploaded)
                ))
                return(invisible(NULL))
            }

            uploaded_qfeatures(uploaded)
            if (use_demo) {
                load_startup_qfeatures(uploaded)
            }
        }, once = TRUE)
    }

    shiny::observeEvent(input$startup_qfeatures_rds, {
        datapath <- input$startup_qfeatures_rds$datapath
        read_startup_qfeatures(function() check_qfeatures(datapath))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$startup_load_qfeatures, {
        if (startup_reading()) {
            return(invisible(NULL))
        }
        uploaded <- uploaded_qfeatures()
        if (is.null(uploaded)) {
            upload_message(
                "Upload a valid .rds file containing a QFeatures object."
            )
            return(invisible(NULL))
        }

        selected_sets <- if (select_initial_sets) {
            input$startup_initial_sets
        } else {
            names(uploaded)
        }
        load_startup_qfeatures(uploaded, selected_sets)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$startup_use_demo_qfeatures, {
        read_startup_qfeatures(demo_qfeatures, use_demo = TRUE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$startup_show_upload, {
        show_startup_upload_modal()
    }, ignoreInit = TRUE)

    if (!has_qfeatures) {
        session$onFlushed(function() {
            show_startup_upload_modal()
        }, once = TRUE)
    }

    invisible(NULL)
}
