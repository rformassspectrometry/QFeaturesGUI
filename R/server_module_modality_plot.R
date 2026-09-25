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
#' @importFrom ggplot2 geom_line geom_point facet_grid
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
            selected <- isolate(input$selected_assay)
            if (length(selected) != 1L || !(selected %in% choices)) {
                selected <- if (length(choices) > 0L) unname(choices[1]) else character()
            }
            updateSelectInput(session,
                "selected_assay",
                choices = choices,
                selected = selected
            )
        })

        sub_qfeat <- reactive({
            qfeatures <- assays_to_process()
            req(input$selected_assay, input$selected_assay %in% names(qfeatures))
            selected <- assays_to_process()[, , which(names(qfeatures) %in% input$selected_assay)]
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
            updateSelectizeInput(
                session,
                'featnames',
                choices = featNames,
                server = TRUE)
        })
        modality_data <- reactive({
            req(sub_qfeat())
            req(input$featnames)
            print(input$featnames)
            feat <- assays_to_process()[input$featnames, , ]
            feat <- feat[, , names(sub_qfeat())]
            print(longForm(feat))
            modality_df <- data.frame(longForm(feat))
            modality_df$assay <- factor(modality_df$assay,
                levels = names(sub_qfeat()))
            print(modality_df)
            modality_df
        })

        output$modality_plot <- renderPlotly({
            req(modality_data())
            # wrap into error_wrapper
            plot <- ggplot(data = modality_data(), aes(x = colname, y = value, group = rowname)) + 
                geom_line() +
                geom_point() +
                facet_grid(~assay)
            ggplotly(plot)
        })
    })
}
