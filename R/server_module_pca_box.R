#' PCA settings and plot server module
#'
#' @param id module id
#' @param assays_to_process A reactive containing a QFeatures object with the
#'   assays available for PCA.
#' @param assay_labels A function taking a character vector of assay names and
#'   returning a character vector of display labels of the same length.
#'   Defaults to [identity()]. Original names are retained as selection values.
#'
#' @return A Shiny module server function managing PCA settings and rendering
#' @rdname INTERNAL_server_module_pca_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req reactive updateSelectInput is.reactive isolate
#' @importFrom MultiAssayExperiment getWithColData
#' @importFrom plotly plot_ly renderPlotly layout
#' @importFrom SummarizedExperiment colData rowData
#' @importFrom methods is
#' @importFrom stats setNames
#'
server_module_pca_box <- function(id, assays_to_process, assay_labels = identity) {
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

        single_assay <- reactive({
            qfeatures <- assays_to_process()
            req(input$selected_assay, input$selected_assay %in% names(qfeatures))
            selected <- suppressWarnings(getWithColData(qfeatures, input$selected_assay))
            stopifnot(is(selected, "SummarizedExperiment"))
            selected
        })

        annotation_names <- reactive({
            req(single_assay())
            req(input$pca_type %in% c("samples", "features"))
            if (input$pca_type == "features") {
                c("NULL", colnames(rowData(single_assay())))
            } else {
                c("NULL", colnames(colData(single_assay())))
            }
        })

        observe({
            updateSelectInput(session,
                "pca_color",
                choices = annotation_names(),
                selected = "NULL"
            )
        })

        color_data <- reactive({
            req(single_assay())
            req(input$pca_color, input$pca_color %in% annotation_names())
            if (input$pca_color != "NULL") {
                req(input$color_width)
                if (input$pca_type == "features") {
                    df <- rowData(single_assay())[, input$pca_color, drop = FALSE]
                } else {
                    df <- colData(single_assay())[, input$pca_color, drop = FALSE]
                }
                if (is.character(df[, 1])) {
                    df[, 1] <- ifelse(nchar(df[, 1]) > input$color_width,
                        paste0(substr(df[, 1], 1, input$color_width), "..."), df[, 1]
                    )
                }
                if (all(is.na(df))) {
                    df[, 1] <- "NA"
                }
                colnames(df) <- input$pca_color
                return(df)
            }
        })

        pca_result <- reactive({
            req(input$pca_type %in% c("samples", "features"))
            req(!is.null(input$scale), !is.null(input$center))
            req(single_assay())
            req(!is_empty_set(single_assay()))
            req(ncol(single_assay()) > 0L)
            error_handler(
                nipalsWrapper,
                "QC Nipals",
                sce = single_assay(),
                transpose = input$pca_type == "samples",
                scale = input$scale,
                center = input$center
            )
        })
        dataframe <- reactive({
            req(input$pca_color, input$pca_color %in% annotation_names())
            req(single_assay())
            req(!is_empty_set(single_assay()))
            req(ncol(single_assay()) > 0L)
            req(pca_result())
            if (input$pca_color == "NULL") {
                as.data.frame(
                    data.frame(pca_result()$scores)
                )
            } else {
                req(color_data())
                scores_df <- as.data.frame(data.frame(pca_result()$scores))
                scores_df$.qfeaturesgui_row_id <- rownames(scores_df)
                color_df <- as.data.frame(color_data())
                color_df$.qfeaturesgui_row_id <- rownames(color_df)
                as.data.frame(merge(
                    scores_df,
                    color_df,
                    by = ".qfeaturesgui_row_id",
                    sort = FALSE
                ))
            }
        })

        output$pca <- renderPlotly({
            req(single_assay())
            if (is_empty_set(single_assay()) || ncol(single_assay()) == 0L) {
                message_text <- paste0(
                    "PCA cannot be computed for this set (",
                    nrow(single_assay()), " row", if (nrow(single_assay()) != 1L) "s" else "",
                    ", ",
                    ncol(single_assay()), " column", if (ncol(single_assay()) != 1L) "s" else "",
                    ")."
                )
                empty_plot <- plot_ly(
                    x = numeric(0),
                    y = numeric(0),
                    type = "scatter",
                    mode = "markers"
                )
                empty_plot <- plotly::add_annotations(
                    empty_plot,
                    text = message_text,
                    xref = "paper",
                    yref = "paper",
                    x = 0.5,
                    y = 0.5,
                    showarrow = FALSE
                )
                empty_plot <- layout(
                    empty_plot,
                    showlegend = FALSE,
                    xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
                    yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE)
                )
                return(empty_plot)
            }
            req(input$x_axis, input$y_axis, !is.null(input$show_legend))
            req(dataframe())
            req(pca_result())
            # TODO: Add a table with the selected points.
            error_handler(
                pca_plotly,
                component_name = "PCA quality control plot",
                df = dataframe(),
                pca_result = pca_result(),
                color_name = input$pca_color,
                show_legend = input$show_legend,
                x_component = input$x_axis,
                y_component = input$y_axis
            )
        })
    })
}
