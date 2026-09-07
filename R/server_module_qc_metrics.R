#' Server module for qc metrics box
#'
#' @param id module id
#' @param assays_to_process a reactiveVal that contains the different assays that will be used in the module
#' @return The different assays that will be processed in the page
#' @rdname INTERNAL_server_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_qc_metrics <- function(id, assays_to_process) {
    stopifnot(is.reactive(assays_to_process))
    moduleServer(id, function(input, output, session) {
        assays_choices_vector <- reactive({
            original_names <- names(assays_to_process())
            modified_names <- remove_QFeaturesGUI(original_names)
            choices_vector <- setNames(original_names, modified_names)
            return(choices_vector)
        })
        observe({
            choices <- assays_choices_vector()
            req(choices)
            updateSelectInput(session,
                "selected_assay",
                choices = names(choices)
            )
        })

        single_assay <- reactive({
            req(input$selected_assay)
            req(assays_to_process())
            # Warning appears here
            # Warning message: 'experiments' dropped; see 'drops()'
            # see with Chris
            suppressWarnings(getWithColData(
                assays_to_process(),
                assays_choices_vector()[input$selected_assay]
            ))
        })
        annotation_names <- reactive({
            req(single_assay())
            if (input$assay_type == "features") {
                c("NULL", colnames(rowData(single_assay())))
            } else {
                c("NULL", colnames(colData(single_assay())))
            }
        })

        observe({
            req(single_assay())
            req(annotation_names())
            stopifnot(is(single_assay(), "SummarizedExperiment"))
            updateSelectInput(session,
                "pca_color",
                choices = annotation_names(),
                selected = "NULL"
            )
        })

        server_module_pca_box(
            id = "features",
            single_assay = single_assay,
            pca_type = reactive(input$assay_type),
            scale = reactive(input$scale),
            center = reactive(input$center),
            color = reactive(input$pca_color),
            show_legend = reactive(input$show_legend),
            color_width = reactive(input$color_width),
            x_component = reactive(input$x_axis),
            y_component = reactive(input$y_axis)
        )

        server_module_viz_box("viz_box", assays_to_process)
    })
}

#' PCA Box server module
#'
#' @param id module id
#' @param single_assay a reactiveVal that contains the selected assay
#' @param pca_type sample or features
#' @param scale a boolean that specifies if the data should be scaled
#' @param center a boolen that specifies if the data should be centered
#' @param show_legend a boolean that specifies if the legend should be shown
#' @param color which metadata use for color
#' @param color_width how many letter display in the legend
#' @param x_component the principal component to display on x axis
#' @param y_component the principal component to display on y axis
#'
#' @return A shiny module server function that contains the PCA logic
#' @rdname INTERNAL_server_module_pca_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req reactive updateSelectInput
#' @importFrom plotly plot_ly renderPlotly layout %>%
#' @importFrom SummarizedExperiment colData rowData
#' @importFrom methods is
#'
server_module_pca_box <- function(id, single_assay, pca_type, scale, center, show_legend, color, color_width, x_component, y_component) {
    moduleServer(id, function(input, output, session) {
        stopifnot(is.reactive(pca_type))
        stopifnot(is.reactive(scale))
        stopifnot(is.reactive(center))
        stopifnot(is.reactive(show_legend))
        stopifnot(is.reactive(color))
        stopifnot(is.reactive(color_width))
        stopifnot(is.reactive(x_component))
        stopifnot(is.reactive(y_component))

        color_data <- reactive({
            req(single_assay())
            req(color())
            if (color() != "NULL") {
                if (pca_type() == "features") {
                    df <- rowData(single_assay())[, color(), drop = FALSE]
                } else {
                    df <- colData(single_assay())[, color(), drop = FALSE]
                }
                if (is.character(df[, 1])) {
                    df[, 1] <- ifelse(nchar(df[, 1]) > color_width(),
                        paste0(substr(df[, 1], 1, color_width()), "..."), df[, 1]
                    )
                }
                if (all(is.na(df))) {
                    df[, 1] <- "NA"
                }
                colnames(df) <- color()
                return(df)
            }
        })


        pca_result <- reactive({
            req(single_assay())
            req(!is_empty_set(single_assay()))
            req(ncol(single_assay()) > 0L)
            error_handler(
                nipalsWrapper,
                "QC Nipals",
                sce = single_assay(),
                transpose = pca_type() == "samples",
                scale = scale(),
                center = center()
            )
        })
        dataframe <- reactive({
            req(single_assay())
            req(!is_empty_set(single_assay()))
            req(ncol(single_assay()) > 0L)
            req(pca_result())
            if (color() == "NULL") {
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
            req(dataframe())
            req(pca_result())
            # TODO: Add a table with the selected points.
            error_handler(
                pca_plotly,
                component_name = "PCA quality control plot",
                df = dataframe(),
                pca_result = pca_result(),
                color_name = color(),
                show_legend = show_legend(),
                x_component = x_component(),
                y_component = y_component()
            )
        })
    })
}
