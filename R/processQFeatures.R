#' Launch a Shiny application to process QFeatures objects
#'
#' @description
#' \code{processQFeatures()} launches an interactive Shiny application
#' that allows users to visually configure and apply pre-processing
#' workflows to a \linkS4class{QFeatures} object.
#'
#' The input \code{qfeatures} can be provided as an in-memory
#' \linkS4class{QFeatures} object, as a path to an \code{.rds} file
#' containing one, or omitted. If omitted, the application prompts the user
#' to upload a \linkS4class{QFeatures} object from an \code{.rds} file.
#'
#' @param qfeatures Optional \linkS4class{QFeatures} object to be processed,
#'   or a character string specifying the path to a \code{.rds} file
#'   containing a \linkS4class{QFeatures} object. If omitted, the app starts
#'   without processing steps and displays a startup modal.
#'
#' @param initialSets An integer, logical, or character vector specifying
#'   which assays (feature sets) should be used as the starting point for
#'   processing. If \code{NULL} and \code{qfeatures} is provided, all assays in
#'   \code{qfeatures} are used. If \code{qfeatures} is omitted, the user chooses
#'   the initial sets after uploading the \code{.rds} file.
#'
#' @param prefilledSteps A character vector specifying the initial workflow
#'   steps to display when the application launches. Steps must be provided
#'   using their internal identifiers (e.g. \code{"sampleFiltering"},
#'   \code{"featureFiltering"}, \code{"normalisation"}).
#'
#' @param maxSize An integer that changes the \code{shiny.maxRequestSize}
#'   value, in MB. This controls the maximum upload size for the startup
#'   \code{.rds} file upload modal.
#'
#' @return
#' The processQFeatures Shiny application.
#'
#' @details
#' The application provides a drag-and-drop workflow builder that allows
#' users to select, order, and configure processing steps such as filtering,
#' normalization, and transformation. The configured workflow can then be
#' applied to the selected assays.
#'
#' @export
#'
#' @importFrom shiny shinyApp runApp addResourcePath onStop
#'
#' @examples
#'
#' library(QFeaturesGUI)
#'
#'
#' app <- processQFeatures()
#'
#' if (interactive()) {
#'     shiny::runApp(app)
#' }
processQFeatures <- function(
      qfeatures = NULL,
      initialSets = NULL,
      prefilledSteps = c(
          "sampleFiltering",
          "featureFiltering",
          "missingValuesFeatures",
          "missingValuesSamples",
          "normalisation",
          "aggregation",
          "join",
          "aggregation"
      ),
      maxSize = 100
) {
    qfeatures_missing <- missing(qfeatures) || is.null(qfeatures)
    initial_steps <- check_prefilled_steps(prefilledSteps)

    if (qfeatures_missing) {
        initial_sets <- integer(0)
    } else {
        ## Validate QFeatures input
        qfeatures <- check_qfeatures(qfeatures)

        ## Normalize initial assay selection
        if (is.null(initialSets)) {
            initialSets <- seq_along(qfeatures)
        }
        initial_sets <- normalise_initial_sets(qfeatures, initialSets)
    }

    oldOptions <- options(shiny.maxRequestSize = maxSize * 1024^2)
    onStop(function() options(oldOptions))
    addResourcePath(
        "app-assets",
        system.file("www", package = "QFeaturesGUI")
    )

    ui <- build_process_ui(initial_steps)
    server <- build_process_server(
        qfeatures,
        initial_sets,
        initial_steps,
        has_qfeatures = !qfeatures_missing
    )

    shinyApp(ui = ui, server = server)
}
