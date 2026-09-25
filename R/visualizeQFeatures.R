#' Launch a Shiny application to visualize QFeatures objects
#'
#' @description
#' \code{visualizeQFeatures()} launches an interactive Shiny application
#' that allows users to visualize a \linkS4class{QFeatures} object.
#'
#' The input \code{qfeatures} can be provided as an in-memory
#' \linkS4class{QFeatures} object, as a path to an \code{.rds} file
#' containing one, or omitted. If omitted, the application prompts the user
#' to upload a \linkS4class{QFeatures} object from an \code{.rds} file
#' or use the bundled demo dataset.
#'
#' @param qfeatures Optional \linkS4class{QFeatures} object to visualize,
#'   or a character string specifying the path to an \code{.rds} file
#'   containing one. If omitted or \code{NULL}, the app displays a startup
#'   modal for uploading a file or loading the bundled demo.
#'
#' @param maxSize An integer that changes the \code{shiny.maxRequestSize}
#'   value, in MB. This controls the maximum upload size for the startup
#'   \code{.rds} file upload modal.
#'
#' @return
#' The visualizeQFeatures Shiny application.
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

    if (!qfeatures_missing) {
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
        has_qfeatures = !qfeatures_missing
    )

    shinyApp(ui = ui, server = server)
}
