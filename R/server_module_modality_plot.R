#' modality plot server module
#'
#' @param id module id
#' @param assays_to_process A reactive containing a QFeatures object with the
#'   assays available for this module.
#' @param assay_labels A function taking a character vector of assay names and
#'   returning a character vector of display labels of the same length.
#'   Defaults to [identity()]. Original names are retained as selection values.
#'
#' @return A Shiny module server function managing modality plot settings and
#' rendering
#' @rdname INTERNAL_server_module_modality_plot_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req reactive
#' @importFrom ggplot2 geom_line geom_point geom_boxplot facet_grid
#' @importFrom SummarizedExperiment colData
#' @importFrom MultiAssayExperiment longForm
#' @importFrom plotly plot_ly renderPlotly layout
#'
server_module_modality_plot <- function(id, assays_to_process, assay_labels = identity) {
    stopifnot(is.reactive(assays_to_process), is.function(assay_labels))
    moduleServer(id, function(input, output, session) {
        assay_choices <- reactive({
            assay_names <- names(assays_to_process())
            if (length(assay_names) == 0L) {
                return(character())
            }
            labels <- assay_labels(assay_names)
            stopifnot(is.character(labels), length(labels) == length(assay_names))
            setNames(assay_names, labels)
        })

        observe({
            choices <- assay_choices()
            selected <- intersect(isolate(input$selected_assay), unname(choices))
            if (length(selected) == 0) {
                selected <- if (length(choices) > 0L) unname(choices[1]) else character()
            }
            updateSelectInput(session,
                "selected_assay",
                choices = choices,
                selected = selected
            )
        })

        observe({
            req(assays_to_process())
            choices <- c("Sample names", colnames(colData(assays_to_process())))
            selected <- isolate(input$annotation)
            if (length(selected) != 1L || !(selected %in% choices)) {
                selected <- "Sample names"
            }
            updateSelectInput(session,
                "annotation",
                choices = choices,
                selected = selected
            )
        })

        sub_qfeat <- reactive({
            qfeatures <- assays_to_process()
            req(input$selected_assay, input$selected_assay %in% names(qfeatures))
            selected <- suppressWarnings(suppressMessages(assays_to_process()[, , which(names(qfeatures) %in% input$selected_assay)]))
            stopifnot(is(selected, "QFeatures"))
            selected
        })

        observe({
            req(input$selected_assay)
            updateSelectInput(session,
                "reference_modality",
                choices = input$selected_assay
            )
        })

        observe({
            req(sub_qfeat())
            req(input$reference_modality)
            featNames <- rownames(sub_qfeat())[[input$reference_modality]]
            selectedFeat <- intersect(isolate(input$featnames), featNames)
            if (length(featNames) == 0) {
                selectedFeat <- NULL
            } else if (length(selectedFeat) == 0) {
                selectedFeat <- featNames[[1]]
            }
            updateSelectizeInput(
                session,
                "featnames",
                choices = featNames,
                selected = selectedFeat,
                server = TRUE)
        })
        modality_data <- reactive({
            req(sub_qfeat())
            req(input$featnames, input$annotation)
            feat <- suppressWarnings(suppressMessages(assays_to_process()[input$featnames, , ]))
            feat <- suppressWarnings(suppressMessages(feat[, , names(sub_qfeat())]))
            modality_df <- suppressMessages(data.frame(longForm(feat)))
            if (input$annotation != "Sample names") {
                sample_metadata <- colData(feat)
                req(input$annotation %in% colnames(sample_metadata))
                sample_index <- match(modality_df$primary, rownames(sample_metadata))
                modality_df$sample_group <- factor(
                    sample_metadata[[input$annotation]][sample_index],
                    exclude = NULL
                )
            }
            modality_df$assay <- factor(modality_df$assay,
                levels = names(sub_qfeat()))
            modality_df
        })

        modality_plot <- eventReactive(input$render, {
            plot_data <- modality_data()
            req(nrow(plot_data) > 0L)
            if (input$annotation == "Sample names") {
                plot <- ggplot(plot_data, aes(x = colname, y = value, group = rowname)) +
                    geom_line(aes(color = rowname)) +
                    geom_point(aes(color = rowname)) +
                    facet_grid(~assay)
            } else {
                plot_data$rowname <- factor(plot_data$rowname,
                    levels = unique(plot_data$rowname))
                plot <- ggplot(plot_data, aes(x = rowname, y = value, fill = sample_group)) +
                    geom_boxplot(na.rm = TRUE) +
                    ggplot2::labs(x = "Feature", y = "Intensity", fill = input$annotation) +
                    facet_grid(~assay, scales = "free_x")
            }
            plot
        })

        output$modality_plot <- renderPlotly({
            ggplotly(modality_plot()) %>%
                layout(boxmode = "group")
        })
    })
}
