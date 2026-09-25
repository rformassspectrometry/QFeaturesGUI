#' Validate and load a QFeatures object
#'
#' Internal helper to validate the \code{qfeatures} argument. If a character
#' path is provided, the function attempts to read an RDS file and validates
#' that it contains a \linkS4class{QFeatures} object.
#'
#' @param qfeatures A \linkS4class{QFeatures} object or a character path to
#'   an RDS file containing one.
#'
#' @return A validated \linkS4class{QFeatures} object.
#'
#' @keywords internal
#' @noRd
check_qfeatures <- function(qfeatures) {
    if (missing(qfeatures)) {
        stop("`qfeatures` argument is missing")
    }

    from_rds_file <- FALSE
    if (is.character(qfeatures)) {
        from_rds_file <- TRUE
        if (length(qfeatures) != 1L) {
            stop("`qfeatures` must be a single path to an RDS file.")
        }
        if (!file.exists(qfeatures)) {
            stop("The file '", qfeatures, "' does not exist.")
        }

        qfeatures <- tryCatch(
            readRDS(qfeatures),
            error = function(e) {
                stop("Failed to read RDS file: ", e$message)
            }
        )
    }

    if (!inherits(qfeatures, "QFeatures")) {
        if (from_rds_file) {
            stop("The RDS file does not contain a QFeatures object.")
        }
        stop(
            "`qfeatures` must be a QFeatures object or a valid path to an RDS file containing one."
        )
    }

    qfeatures
}

#' Build the bundled demo QFeatures object
#'
#' @return A \linkS4class{QFeatures} object built from the package
#'   \code{inputTable} and \code{sampleTable} example datasets.
#'
#' @keywords internal
#' @noRd
demo_qfeatures <- function() {
    data_env <- new.env(parent = emptyenv())
    utils::data(
        list = c("inputTable", "sampleTable"),
        package = "QFeaturesGUI",
        envir = data_env
    )

    if (!exists("inputTable", envir = data_env, inherits = FALSE) ||
        !exists("sampleTable", envir = data_env, inherits = FALSE)) {
        stop("Bundled demo data could not be loaded.")
    }

    qfeatures <- QFeatures::readQFeatures(
        assayData = data_env$inputTable,
        colData = data_env$sampleTable,
        runCol = "Raw.file",
        quantCols = NULL,
        removeEmptyCols = TRUE,
        verbose = FALSE
    )
    if (length(qfeatures) > 0) {
        qfeatures <- QFeatures::zeroIsNA(qfeatures, i = seq_along(qfeatures))
    }

    qfeatures
}
