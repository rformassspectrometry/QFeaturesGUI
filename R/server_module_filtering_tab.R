#' @title Server logic for the filtering tab
#' @param id The module id
#' @param step_number The step number
#' @param step_rv A reactiveVal used to signal when this step is saved
#' @param parent_rv A reactiveVal used to gate this step until the parent step is saved
#' @param type A character string specifying the type of the filtering (`"samples"` or `"features"`)
#'
#' @return The processed assays
#' @rdname INTERNAL_server_module_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactiveValues observe NS reactive req reactiveVal icon
#' @importFrom htmltools tags
#' @importFrom shinydashboard renderInfoBox infoBox
server_module_filtering_tab <- function(
      id,
      step_number,
      step_rv,
      parent_rv,
      type = c("samples", "features")
) {
    type <- match.arg(type)

    moduleServer(id, function(input, output, session) {
        pattern <- paste0("_(QFeaturesGUI#", step_number - 1, ")")

        step_ready <- reactive({
            if (!is.null(parent_rv)) req(parent_rv() > 0L)
            TRUE
        })

        parent_assays <- reactive({
            req(step_ready())
            error_handler(page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = .qf$qfeatures,
                pattern = pattern
            )
        })

        server_module_qc_metrics("psm_pre", parent_assays)

        n_boxes <- reactiveVal(0)

        filtering_condition_labels <- reactiveValues()
        filtering_condition_specs <- reactiveValues()
        boxes_states <- reactiveValues()

        display_or_default <- function(value, default) {
            if (is.null(value) || length(value) == 0 || !nzchar(as.character(value)[1])) {
                return(default)
            }
            as.character(value)[1]
        }

        observeEvent(input$add_box, {
            n_boxes(n_boxes() + 1)
        })

        observeEvent(input$remove_box, {
            if (n_boxes() > 0) {
                removed_index <- n_boxes()
                n_boxes(removed_index - 1L)
                filtering_condition_labels[[paste0("condition_label_", removed_index)]] <- NULL
                filtering_condition_specs[[paste0("condition_spec_", removed_index)]] <- NULL
                boxes_states[[paste0("box_", removed_index)]] <- NULL
            }
        })

        observeEvent(n_boxes(), {
            output$filtering_boxes <- renderUI({
                if (n_boxes() == 0) {
                    return(tags$div(
                        class = "alert alert-warning",
                        style = "text-align: center; max-width: 680px; margin: 10px auto;",
                        "No filtering boxes yet. Add one with the 'Add Filtering Condition' button above."
                    ))
                }
                tags$div(
                    lapply(seq_len(n_boxes()), function(i) {
                        interface_module_filtering_box(
                            NS(id, paste0("filtering_", i)),
                            box_title = paste0("Filtering Box #", i),
                            width = 4
                        )
                    })
                )
            })

            if (n_boxes() > 0) {
                lapply(seq_len(n_boxes()), function(i) {
                    key <- paste0("box_", i)
                    res <- server_module_filtering_box(
                        paste0("filtering_", i),
                        parent_assays,
                        type,
                        boxes_states[[key]]
                    )
                    observe({
                        filtering_condition_labels[[paste0("condition_label_", i)]] <- res$condition_label()
                        filtering_condition_specs[[paste0("condition_spec_", i)]] <- res$condition_spec()
                        boxes_states[[key]] <- list(
                            annotation_selection = res$annotation_selection(),
                            filter_operator = res$filter_operator(),
                            filter_value = res$filter_value()
                        )
                    })
                })
            }

            output$filtering_summary <- renderUI({
                if (n_boxes() == 0) {
                    return(tags$div(
                        class = "alert alert-warning",
                        style = "text-align: center; max-width: 680px; margin: 10px auto;",
                        "No filtering conditions yet. Add at least one filtering box."
                    ))
                }

                tags$div(
                    class = "list-group",
                    lapply(seq_len(n_boxes()), function(i) {
                        condition_label <- display_or_default(
                            filtering_condition_labels[[paste0("condition_label_", i)]],
                            "Condition not configured yet."
                        )
                        tags$div(
                            class = "list-group-item",
                            tags$div(
                                style = "font-weight: 600;",
                                paste0("Condition #", i)
                            ),
                            tags$div(
                                style = "margin-top: 6px;",
                                tags$code(condition_label)
                            ),
                            if (i < n_boxes()) {
                                tags$div(
                                    style = "margin-top: 8px;",
                                    tags$span(class = "label label-default", "AND")
                                )
                            }
                        )
                    })
                )
            })
        })

        condition_specs_list <- reactive({
            specs <- lapply(seq_len(n_boxes()), function(i) {
                filtering_condition_specs[[paste0("condition_spec_", i)]]
            })
            specs <- Filter(function(spec) {
                if (is.list(spec) &&
                    !is.null(spec$annotation) &&
                    !is.null(spec$operator) &&
                    is_missingness_filter_operator(spec$operator)) {
                    return(TRUE)
                }
                is.list(spec) &&
                    !is.null(spec$annotation) &&
                    !is.null(spec$operator) &&
                    !is.null(spec$value) &&
                    length(spec$value) > 0 &&
                    !all(is.na(spec$value))
            }, specs)
            specs
        })

        processed_assays <- eventReactive(
            c(input$apply_filters, step_ready()),
            {
                if (length(condition_specs_list()) > 0) {
                    if (type == "samples") {
                        return(error_handler(sample_filtering,
                            component_name = "Sample filtering",
                            qfeatures = parent_assays(),
                            condition_specs = condition_specs_list()
                        ))
                    }
                    return(error_handler(feature_filtering,
                        component_name = "Feature filtering",
                        qfeatures = parent_assays(),
                        condition_specs = condition_specs_list()
                    ))
                }
                parent_assays()
            }
        )
        server_module_qc_metrics("psm_filtered", processed_assays)
        output[[paste0("number_", type, "_removed")]] <- renderInfoBox({
            nb_removed <- number_removed(parent_assays(), processed_assays(), type = type)
            infoBox(
                paste0("Number of ", type, " removed: "),
                nb_removed,
                fill = TRUE,
                color = "light-blue",
                icon = icon("filter")
            )
        })

        output[[paste0("percent_", type, "_removed")]] <- renderInfoBox({
            pct_removed <- percent_removed(parent_assays(), processed_assays(), type = type)
            infoBox(
                paste0("Percent of ", type, " removed: "),
                paste(pct_removed, "%"),
                fill = TRUE,
                color = "light-blue",
                icon = icon("filter")
            )
        })

        observeEvent(input$export,
            {
                req(processed_assays())
                with_task_loader(
                    caption = paste(
                        "Be aware that this operation",
                        "can be quite time consuming for large data sets"
                    ),
                    expr = {
                        error_handler(
                            add_assays_to_global_rv,
                            component_name = "Add assays to global_rv",
                            processed_qfeatures = processed_assays(),
                            step_number = step_number,
                            type = paste0(type, "_filtering")
                        )
                        step_rv(step_rv() + 1L)
                        global_rv$code_lines[[paste0("Initialization_names_", step_number)]] <- codeGeneratorInitialization(qf = .qf$qfeatures, step_number = step_number)
                        global_rv$code_lines[[paste0("filtering_", type, "_", step_number)]] <- codeGeneratorFiltering(qf = .qf$qfeatures, condition = condition_specs_list(), type = type, step_number = step_number)
                    }
                )
            },
            ignoreInit = TRUE
        )
    })
}


#' @title sample filtering
#'
#' @param qfeatures A qfeatures object
#' @param condition_specs A list of filtering condition specifications
#'
#' @return A qfeatures object with the filtered samples
#' @rdname INTERNAL_sample_filtering
#' @keywords internal
#'
sample_filtering <- function(qfeatures, condition_specs) {
    rowname_selector_key <- ".qfeaturesgui_rowname"
    sample_metadata <- as.data.frame(SummarizedExperiment::colData(qfeatures))
    sample_metadata[[rowname_selector_key]] <- rownames(SummarizedExperiment::colData(qfeatures))
    keep_mask <- rep(TRUE, nrow(sample_metadata))

    for (spec in condition_specs) {
        annotation <- as.character(spec$annotation)
        if (!(annotation %in% colnames(sample_metadata))) {
            stop(paste0("Unknown sample annotation column: ", annotation))
        }
        condition_mask <- apply_filter_operator(
            values = sample_metadata[[annotation]],
            operator = as.character(spec$operator),
            target = spec$value
        )
        condition_mask[is.na(condition_mask)] <- FALSE
        keep_mask <- keep_mask & condition_mask
    }

    qfeatures[, keep_mask, ]
}


#' @title feature filtering
#'
#' @param qfeatures A qfeatures object
#' @param condition_specs A list of filtering condition specifications
#'
#' @return A qfeatures object with the filtered features
#' @rdname INTERNAL_feature_filtering
#' @keywords internal
#'
feature_filtering <- function(qfeatures, condition_specs) {
    rowname_selector_key <- ".qfeaturesgui_rowname"
    filtered_qfeatures <- qfeatures
    for (assay_name in names(qfeatures)) {
        assay_object <- qfeatures[[assay_name]]
        feature_metadata <- as.data.frame(SummarizedExperiment::rowData(assay_object))
        row_ids <- NULL
        if ("rowname" %in% colnames(feature_metadata)) {
            row_ids <- feature_metadata[["rowname"]]
        } else if ("name" %in% colnames(feature_metadata)) {
            row_ids <- feature_metadata[["name"]]
        } else {
            row_ids <- rownames(SummarizedExperiment::rowData(assay_object))
        }
        feature_metadata[[rowname_selector_key]] <- as.character(row_ids)
        keep_mask <- rep(TRUE, nrow(feature_metadata))

        for (spec in condition_specs) {
            annotation <- as.character(spec$annotation)
            if (!(annotation %in% colnames(feature_metadata))) {
                stop(paste0(
                    "Unknown feature annotation column: ",
                    annotation,
                    " in assay ",
                    assay_name
                ))
            }
            condition_mask <- apply_filter_operator(
                values = feature_metadata[[annotation]],
                operator = as.character(spec$operator),
                target = spec$value
            )
            condition_mask[is.na(condition_mask)] <- FALSE
            keep_mask <- keep_mask & condition_mask
        }
        filtered_qfeatures[[assay_name]] <- assay_object[keep_mask, , drop = FALSE]
    }
    filtered_qfeatures
}


#' @title apply filter operator
#'
#' @param values A vector of values to compare
#' @param operator A comparison operator (`"=="`, `"!="`, `"<"`, `"<="`, `">"` or `">="`)
#' @param target A target value (or vector of values for equality operators)
#'
#' @return A logical vector indicating whether each value satisfies the condition
#' @rdname INTERNAL_apply_filter_operator
#' @keywords internal
#'
apply_filter_operator <- function(values, operator, target) {
    if (operator == "is_missing") {
        return(is.na(values))
    }
    if (operator == "is_not_missing") {
        return(!is.na(values))
    }
    if (length(target) == 0) {
        return(rep(FALSE, length(values)))
    }
    if (operator %in% c("==", "!=") && length(target) > 1) {
        target_values <- as.character(target)
        if (operator == "==") {
            return(values %in% target_values)
        }
        return(!is.na(values) & !(values %in% target_values))
    }
    operator_functions <- list(
        "==" = `==`,
        "!=" = `!=`,
        "<" = `<`,
        "<=" = `<=`,
        ">" = `>`,
        ">=" = `>=`
    )
    if (!(operator %in% names(operator_functions))) {
        stop("Unsupported filtering operator: ", operator)
    }
    operator_functions[[operator]](values, target)
}

missingness_filter_operators <- function() {
    c("is_missing", "is_not_missing")
}

is_missingness_filter_operator <- function(operator) {
    operator %in% missingness_filter_operators()
}
