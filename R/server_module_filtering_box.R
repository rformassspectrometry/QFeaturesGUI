#' @title server_module_annotation_plot
#'
#' @param id The module id
#' @param assays_to_process A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @param state A list that contains the state of the module (input states)
#' @return The updated assays
#' @rdname INTERNAL_server_module_filtering_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput reactive observe is.reactive req updateTextInput updateSelectInput selectizeInput numericInput updateSelectizeInput
#' @importFrom SummarizedExperiment colData rowData
#' @importFrom shinyFeedback feedbackDanger
#' @importFrom QFeatures filterFeatures
#'
server_module_filtering_box <- function(id, assays_to_process, type, state) {
    is.reactive(assays_to_process)
    moduleServer(id, function(input, output, session) {
        rowname_selector_key <- ".qfeaturesgui_rowname"

        operator_labels <- c(
            "<" = "is less than",
            "<=" = "is less than or equal to",
            ">" = "is greater than",
            ">=" = "is greater than or equal to",
            "==" = "is equal to",
            "!=" = "is not equal to"
        )
        operator_choices_all <- c(
            "Less than" = "<",
            "Less than or equal to" = "<=",
            "Greater than" = ">",
            "Greater than or equal to" = ">=",
            "Equal to" = "==",
            "Not equal to" = "!="
        )
        operator_choices_categorical <- c(
            "Equal to" = "==",
            "Not equal to" = "!="
        )

        combined_samples_annotations <- reactive({
            req(assays_to_process())
            sample_metadata <- as.data.frame(colData(assays_to_process()))
            sample_metadata[is.na(sample_metadata)] <- "NA"
            sample_metadata[[rowname_selector_key]] <- rownames(colData(assays_to_process()))
            sample_metadata
        })

        combined_features_annotations <- reactive({
            req(assays_to_process())
            feature_metadata <- as.data.frame(rbindRowData(assays_to_process(), seq_along(assays_to_process())))
            feature_metadata[is.na(feature_metadata)] <- "NA"
            row_ids <- NULL
            if ("rowname" %in% colnames(feature_metadata)) {
                row_ids <- feature_metadata[["rowname"]]
            } else if ("name" %in% colnames(feature_metadata)) {
                row_ids <- feature_metadata[["name"]]
            } else {
                row_ids <- rownames(feature_metadata)
            }
            feature_metadata[[rowname_selector_key]] <- as.character(row_ids)
            feature_metadata
        })

        sample_annotation_choices <- reactive({
            req(combined_samples_annotations())
            available_columns <- setdiff(colnames(combined_samples_annotations()), rowname_selector_key)
            c("Rowname" = rowname_selector_key, stats::setNames(available_columns, available_columns))
        })

        feature_annotation_choices <- reactive({
            req(combined_features_annotations())
            available_columns <- setdiff(
                colnames(combined_features_annotations()),
                c("assay", "rowname", "name", rowname_selector_key)
            )
            c("Rowname" = rowname_selector_key, stats::setNames(available_columns, available_columns))
        })

        annotation_choices <- reactive({
            req(assays_to_process())
            if (type == "samples") {
                sample_annotation_choices()
            } else if (type == "features") {
                feature_annotation_choices()
            }
        })
        observe({
            updateSelectInput(
                inputId = "annotation_selection",
                choices = annotation_choices()
            )
        })
        if (!is.null(state)) {
            updateSelectInput(
                inputId = "filter_operator",
                selected = state$filter_operator
            )
            observe({
                updateSelectInput(
                    inputId = "annotation_selection",
                    choices = annotation_choices(),
                    selected = state$annotation_selection
                )
            })
        }


        annotations_type <- reactive({
            req(input$annotation_selection)
            req(assays_to_process())
            if (type == "samples") {
                class(combined_samples_annotations()[[input$annotation_selection]])[[1]]
            } else if (type == "features") {
                class(combined_features_annotations()[[input$annotation_selection]])[[1]]
            }
        })

        operator_choices <- reactive({
            req(annotations_type())
            if (annotations_type() %in% c("character", "factor")) {
                return(operator_choices_categorical)
            }
            operator_choices_all
        })

        observe({
            req(operator_choices())
            selected_operator <- input$filter_operator
            if (!(selected_operator %in% unname(operator_choices()))) {
                selected_operator <- unname(operator_choices())[[1]]
            }
            updateSelectInput(
                session,
                inputId = "filter_operator",
                choices = operator_choices(),
                selected = selected_operator
            )
        })

        is_empty_categorical_multiselect <- reactive({
            req(annotations_type())
            req(input$filter_operator)
            is_categorical <- annotations_type() %in% c("character", "factor")
            is_equality_operator <- input$filter_operator %in% c("==", "!=")
            if (!(is_categorical && is_equality_operator)) {
                return(FALSE)
            }
            filter_value <- input[[paste0("filter_ui_", type)]]
            !is.null(filter_value) && length(filter_value) == 0
        })

        output$filtering_ui <- renderUI({
            state_filter_value <- shiny::isolate(input[[paste0("filter_ui_", type)]])
            if (is.null(state_filter_value) && !is.null(state)) {
                state_filter_value <- state$filter_value
            }
            is_categorical <- annotations_type() %in% c("character", "factor")
            is_equality_operator <- input$filter_operator %in% c("==", "!=")
            if (is_categorical) {
                if (type == "samples") {
                    choices <- unique(combined_samples_annotations()[[input$annotation_selection]])
                } else {
                    choices <- unique(combined_features_annotations()[[input$annotation_selection]])
                }
                selected_value <- NULL
                if (!is.null(state_filter_value)) {
                    if (is_equality_operator) {
                        state_filter_value <- as.character(state_filter_value)
                        selected_value <- intersect(as.character(choices), state_filter_value)
                    } else {
                        state_filter_value <- as.character(state_filter_value)[1]
                        if (state_filter_value %in% as.character(choices)) {
                            selected_value <- state_filter_value
                        }
                    }
                }
                selectizeInput(
                    session$ns(paste0("filter_ui_", type)),
                    label = "Filtering Value",
                    choices = NULL,
                    multiple = is_equality_operator,
                    selected = NULL
                )
            } else {
                numeric_value <- suppressWarnings(as.numeric(state_filter_value)[1])
                if (is.na(numeric_value)) numeric_value <- NA_real_
                if (type == "samples") {
                    numericInput(
                        session$ns(paste0("filter_ui_", type)),
                        label = "Filtering Value",
                        value = numeric_value
                    )
                } else {
                    numericInput(
                        session$ns(paste0("filter_ui_", type)),
                        label = "Filtering Value",
                        value = numeric_value
                    )
                }
            }
        })

        observe({
            req(input$annotation_selection)
            req(input$filter_operator)
            req(annotations_type())
            is_categorical <- annotations_type() %in% c("character", "factor")
            if (!is_categorical) {
                return()
            }
            if (type == "samples") {
                choices <- unique(combined_samples_annotations()[[input$annotation_selection]])
            } else {
                choices <- unique(combined_features_annotations()[[input$annotation_selection]])
            }

            state_filter_value <- shiny::isolate(input[[paste0("filter_ui_", type)]])
            if (is.null(state_filter_value) && !is.null(state)) {
                state_filter_value <- state$filter_value
            }
            is_equality_operator <- input$filter_operator %in% c("==", "!=")
            selected_value <- NULL
            if (!is.null(state_filter_value)) {
                if (is_equality_operator) {
                    state_filter_value <- as.character(state_filter_value)
                    selected_value <- intersect(as.character(choices), state_filter_value)
                } else {
                    state_filter_value <- as.character(state_filter_value)[1]
                    if (state_filter_value %in% as.character(choices)) {
                        selected_value <- state_filter_value
                    }
                }
            }
            updateSelectizeInput(
                session,
                inputId = paste0("filter_ui_", type),
                choices = choices,
                selected = selected_value,
                server = TRUE
            )
        })

        observe({
            feedbackDanger(
                inputId = paste0("filter_ui_", type),
                show = is_empty_categorical_multiselect(),
                text = "Select at least one value for this condition."
            )
        })

        server_module_annotation_plot(
            "annotation_plot",
            assays_to_process,
            type,
            reactive({
                input[[paste0("filter_ui_", type)]]
            }),
            reactive({
                input$annotation_selection
            }),
            reactive({
                input$filter_operator
            })
        )
        condition_label <- reactive({
            req(annotations_type())
            req(input$filter_operator)
            req(input$filter_operator %in% names(operator_labels))
            annotation_label <- if (input$annotation_selection == rowname_selector_key) {
                "Rowname"
            } else {
                input$annotation_selection
            }
            if (annotations_type() %in% c("character", "factor") &&
                input$filter_operator %in% c("==", "!=")) {
                selected_values <- as.character(input[[paste0("filter_ui_", type)]])
                quoted_values <- sprintf("\"%s\"", selected_values)
                if (length(quoted_values) == 0) {
                    filter_value_label <- "\"\""
                } else {
                    filter_value_label <- paste(quoted_values, collapse = " or ")
                }
            } else {
                filter_value <- input[[paste0("filter_ui_", type)]]
                if (annotations_type() %in% c("character", "factor")) {
                    filter_value_label <- paste0("\"", filter_value, "\"")
                } else {
                    filter_value_label <- as.character(filter_value)
                }
            }
            paste(
                annotation_label,
                operator_labels[[input$filter_operator]],
                filter_value_label
            )
        })

        condition_spec <- reactive({
            req(input$annotation_selection)
            req(input$filter_operator)
            req(input$filter_operator %in% names(operator_labels))
            filter_value <- input[[paste0("filter_ui_", type)]]
            if (is.null(filter_value) || is_empty_categorical_multiselect()) {
                return(NULL)
            }
            if (all(is.na(filter_value))) {
                return(NULL)
            }
            list(
                annotation = input$annotation_selection,
                operator = input$filter_operator,
                value = filter_value
            )
        })

        return(list(
            condition_label = condition_label,
            condition_spec = condition_spec,
            annotation_selection = reactive(input$annotation_selection),
            filter_operator = reactive(input$filter_operator),
            filter_value = reactive(input[[paste0("filter_ui_", type)]])
        ))
    })
}


#' @title Annotation Plot server module
#'
#' @param id The module id
#' @param assays_to_process A reactiveVal that contains the different assays that will be used in the module
#' @param type A character string specifying the type of the filtering (sample or feature based)
#' @param filter_value The value that will be used in combinaison with the filter operator
#' @param annotation The annotation that will be used to filter the data
#' @param filter_operator The operator that will be used to filter the data
#'
#' @return A plot of the distribution of the selected annotation in a specific assay
#' @rdname INTERNAL_server_module_annotation_plot
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req eventReactive reactive
#' @importFrom plotly plot_ly renderPlotly
#'
server_module_annotation_plot <- function(id,
    assays_to_process,
    type,
    filter_value,
    selected_annotation,
    filter_operator) {
    moduleServer(id, function(input, output, session) {
        rowname_selector_key <- ".qfeaturesgui_rowname"

        combined_samples_annotations <- reactive({
            req(assays_to_process())
            sample_metadata <- as.data.frame(colData(assays_to_process()))
            sample_metadata[is.na(sample_metadata)] <- "NA"
            sample_metadata[[rowname_selector_key]] <- rownames(colData(assays_to_process()))
            sample_metadata
        })

        combined_features_annotations <- reactive({
            req(assays_to_process())
            feature_metadata <- as.data.frame(rbindRowData(assays_to_process(), seq_along(assays_to_process())))
            feature_metadata[is.na(feature_metadata)] <- "NA"
            row_ids <- NULL
            if ("rowname" %in% colnames(feature_metadata)) {
                row_ids <- feature_metadata[["rowname"]]
            } else if ("name" %in% colnames(feature_metadata)) {
                row_ids <- feature_metadata[["name"]]
            } else {
                row_ids <- rownames(feature_metadata)
            }
            feature_metadata[[rowname_selector_key]] <- as.character(row_ids)
            feature_metadata
        })

        annotation_values <- reactive({
            req(assays_to_process())
            req(selected_annotation())
            if (type == "samples") {
                combined_samples_annotations()[[selected_annotation()]]
            } else if (type == "features") {
                combined_features_annotations()[[selected_annotation()]]
            }
        })
        filtered_annotation <- reactive({
            req(annotation_values())
            selected_operator <- filter_operator()
            req(selected_operator)
            condition_mask <- apply_filter_operator(
                values = annotation_values(),
                operator = selected_operator,
                target = filter_value()
            )
            condition_mask[is.na(condition_mask)] <- FALSE
            annotation_values()[condition_mask]
        })
        output$plot <- renderPlotly({
            annotation <- annotation_values()
            filtered <- filtered_annotation()
            selected <- selected_annotation()
            
            req(annotation)
            req(selected)
            
            plot_title <- if (type == "samples") {
              "All samples across sets"
            } else {
              "All features across sets"
            }
            
            annotation_label <- if (selected == rowname_selector_key) {
              "Rowname"
            } else {
              selected
            }
            
            error_handler(
              annotation_plot_wrapper,
              component_name = "annotation_plot (filtering_box)",
              annotation = annotation,
              filtered_annotation = filtered,
              assay_name = plot_title,
              annotation_name = annotation_label
            )
        })
    })
}

#' @title Annotation plot wrapper
#'
#' @param annotation_df A data.frame that contains the annotation values
#' @param selected_annotation A character string that contains the name of the selected annotation
#' @param filter_value A character string that contains the value of the filter
#' @return A plotly object
#'
#' @rdname INTERNAL_annotation_plot
#' @keywords internal
#'
#' @importFrom plotly plot_ly config %>% add_histogram layout add_annotations
#'
annotation_plot_wrapper <- function(annotation,
    filtered_annotation,
    assay_name,
    annotation_name) {
    if (all(is.na(annotation))) {
        plot <- plot_ly(
            x = numeric(0),
            y = numeric(0),
            type = "scatter",
            mode = "markers"
        ) %>%
            add_annotations(
                text = "All values are NA, cannot create histogram.",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE
            ) %>%
            layout(
                showlegend = FALSE,
                xaxis = list(
                    showticklabels = FALSE,
                    zeroline = FALSE,
                    showgrid = FALSE
                ),
                yaxis = list(
                    showticklabels = FALSE,
                    zeroline = FALSE,
                    showgrid = FALSE
                )
            )
        return(plot)
    }
    plot <- plot_ly() %>%
        add_histogram(x = annotation, name = "Before Filtering") %>%
        layout(
            barmode = "group",
            xaxis = list(title = paste0("Annotation Values (", annotation_name, ")")),
            yaxis = list(title = "Number of appearances"),
            title = assay_name
        ) %>%
        config(displaylogo = FALSE, toImageButtonOptions = list(
            format = "svg",
            filename = "annotation_plot",
            height = 500,
            width = 700,
            scale = 1
        ))
    if (length(filtered_annotation) > 0) {
        plot <- plot %>%
            add_histogram(x = filtered_annotation, name = "After Filtering")
    }
    return(plot)
}
