#' A function that write code lines
#'
#' @param input_table a reactiveVal containing the input table
#' @param sample_table a reactiveVal containing the sample table
#' @param qfeature a QFeatures Object generate by the eventReactive of server_module_box_readQFeatures
#' @param run_col a variable containing the column name containing batches(can be NULL)
#' @param removeEmptyCols a boolean to remove empty column from QFeatures Object
#' @param quant_cols a variable that indicate which column can be used if there is no quantCols in sample_table
#' @param logTransform a boolean that indicate if logTransformation should be perform or not
#' @param zero_as_na a boolean indicating if zero should considered as NA values
#'
#' @return A vector containing code lines
#' @rdname INTERNAL_code_generator_importQFeatures
#' @keywords internal


code_generator_read_qfeatures <- function(input_table, sample_table, qfeatures, run_col, removeEmptyCols, quant_cols, logTransform, zero_as_NA, singlcelldata) {
    if (is.data.frame(sample_table())) {
        colData <- "sample_table"
        quantCols <- NULL
    } else {
        colData <- "NULL"
        quantCols <- quant_cols
    }

    if (run_col != "NULL") {
        runCol <- paste0("'", run_col, "'")
    } else {
        runCol <- "NULL"
    }

    if (!is.null(quantCols)) {
        quantColumns <- paste(sprintf('"%s"', quantCols), collapse = ",\n\t\t")
        codeLines <- sprintf(
            "\nqfeatures <- readQFeatures(\n\tassayData = input_table,\n\tcolData = %s,\n\trunCol = %s,\n\tquantCols = c(%s),\n\tremoveEmptyCols = %s,\n\tverbose = FALSE\n)",
            colData,
            runCol,
            quantColumns,
            removeEmptyCols
        )
    } else {
        codeLines <- sprintf(
            "\nqfeatures <- readQFeatures(\n\tassayData = input_table,\n\tcolData = %s,\n\trunCol = %s,\n\tquantCols = NULL,\n\tremoveEmptyCols = %s,\n\tverbose = FALSE\n)",
            colData,
            runCol,
            removeEmptyCols
        )
    }

    if (zero_as_NA && length(qfeatures) > 0) {
        codeLines <- c(codeLines, "\nqfeatures <- zeroIsNA(\n\tobject = qfeatures,\n\ti = seq_along(qfeatures)\n)")
    }
    if (logTransform) {
        codeLines <- c(codeLines, "\nfor (i in seq_along(qfeatures)) {\n\tqfeatures[[i]] <- logTransform(\n\t\tobject = qfeatures[[i]],\n\t\tbase = 2\n\t)\n}")
    }
    if (singlcelldata) {
        codeLines <- c(codeLines, "\nqfeatures <- setQFeaturesType(\n\tqfeatures,\n\ttype = 'scp'\n)")
    }
    codeLines <- c(codeLines, "\nnames(qfeatures) <- paste0(\n\tnames(qfeatures),\n\t'_initial_import'\n)")
    codeLines
}

code_generator_read_table <- function(id, arg_as_param, file = NULL, sep = NULL, dec = NULL, skip = NULL, stringAsFactors = NULL, comment = NULL) {
    if (!arg_as_param) {
        codeLines <- sprintf(
            "# insert the path to your '%s' data table here\n#%s_table <- myPath\n\n%s_table <- read.table(%s,\n\tsep = '%s',\n\tdec = '%s',\n\tskip = '%s',\n\tstringsAsFactors = %s,\n\tcomment.char = '%s',\n\theader = TRUE\n)\n",
            id,
            id,
            id,
            file,
            sep,
            dec,
            skip,
            stringAsFactors,
            comment
        )
    } else {
        if (id == "input") {
            codeLines <-
                "# Replace dataFrame1 with the value passed as assayData arg\n\ninput_table <- dataFrame1\n"
        } else {
            codeLines <-
                "# Replace dataFrame2 with the value passed as colData arg\n\nsample_table <- dataFrame2\n"
        }
    }
    codeLines
}
