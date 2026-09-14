#' Launch a Shiny application to visualize QFeatures objects
#'
#' @description
#' \code{processQFeatures()} launches an interactive Shiny application
#' that allows users to visualize a \linkS4class{QFeatures} object.
#'
#' The input \code{qfeatures} can be provided as an in-memory
#' \linkS4class{QFeatures} object, as a path to an \code{.rds} file
#' containing one, or omitted. If omitted, the application prompts the user
#' to upload a \linkS4class{QFeatures} object from an \code{.rds} file.
#'
#' @param maxSize An integer that changes the \code{shiny.maxRequestSize}
#'   value, in MB. This controls the maximum upload size for the startup
#'   \code{.rds} file upload modal.
#'
#' @return
#'    NULL
#'
#' @export
#'
#' @importFrom shiny shinyApp runApp onStop
#'
#' @examples
#'
#' library(QFeaturesGUI)
#'
#'
#' app <- visualizeQFeatures()
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
visualizeQFeatures <- function(
      qfeatures = NULL,
      maxSize = 100
) {
    qfeatures_missing <- missing(qfeatures) || is.null(qfeatures)

    if (is.null(qfeatures)) {
        qfeatures <- check_qfeatures(qfeatures)
    }

    oldOptions <- options(shiny.maxRequestSize = maxSize * 1024^2)
    onStop(function() options(oldOptions))
    addResourcePath(
        "app-assets",
        system.file("www", package = "QFeaturesGUI")
    )

    ui <- build_visualize_ui()
    server <- build_visualize_server(
        qfeatures,
        has_qfeatures = !is.null(qfeatures)
    )

    shinyApp(ui = ui, server = server)
}

