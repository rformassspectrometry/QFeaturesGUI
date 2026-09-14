#' @title Code generator that prints the set names for the different steps
#' @param qf QFeatures object
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorInitialization
#' @keywords internal
#'

codeGeneratorInitialization <- function(qf, step_number) {
    vec <- names(qf)
    if (step_number == 1) {
        initial_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#0"), vec)]
        initial_setNames <- remove_QFeaturesGUI(initial_setNames)
        step_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec)]
        step_setNames <- remove_QFeaturesGUI(step_setNames)
        codeLines <- sprintf(
            "####################################
######### initial set names ########
####################################
step0_setNames <- c(%s)\n
####################################
####### Step number %s names ########
####################################
step%s_setNames <- c(%s)\n",
            paste(sprintf('"%s"', initial_setNames), collapse = ", \n\t"),
            step_number,
            step_number,
            paste(sprintf('"%s"', step_setNames), collapse = ", \n\t")
        )
    } else {
        step_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec)]
        step_setNames <- remove_QFeaturesGUI(step_setNames)
        codeLines <- sprintf(
            "####################################
####### Step number %s names ########
####################################
step%s_setNames<- c(%s)\n",
            step_number,
            step_number,
            paste(sprintf('"%s"', step_setNames), collapse = ", \n\t")
        )
    }
    codeLines
}

#' @title Check for missing set
#' @param qf QFeatures object
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_check_for_missing_set
#' @keywords internal
#'

check_for_missing_set <- function(qf, step_number) {
    vec <- names(qf)
    indice_to_remove <- c()
    initial_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number - 1), vec, fixed = TRUE)]
    initial <- gsub("_\\(QFeaturesGUI#[0-9]+\\)_*[a-z]*_*[a-z]*_*[0-9]*", "", initial_setNames)
    currentStep_setNames <- vec[grep(pattern = paste0("QFeaturesGUI#", step_number), vec, fixed = TRUE)]
    current <- gsub("_\\(QFeaturesGUI#[0-9]+\\)_*[a-z]*_*[a-z]*_*[0-9]*", "", currentStep_setNames)
    if (length(initial) != length(current)) {
        initial_setNames <- initial_setNames[initial != setdiff(initial, current)]
        initial_setNames <- remove_QFeaturesGUI(initial_setNames)
        codeLines <- sprintf(
            "##After filtering steps one or more set has been deleted.\nstep%s_setNames <- c(%s)\n",
            step_number - 1,
            paste(sprintf('"%s"', initial_setNames), collapse = ", \n\t")
        )
        return(codeLines)
    }
}


#' @title Code generator for aggregation tab
#' @param method the method used to do the aggregation
#' @param fcol `character(1)` naming a `rowData` variable that defines how to aggregate
#' the features within each assay. This variable is either a character or a (possibly
#' sparse) matrix.
#' @param step_number The step number
#'
#' @return code line generated
#' @rdname INTERNAL_codeGeneratorAggregation
#' @keywords internal
#'

codeGeneratorAggregation <- function(method, fcol, step_number) {
    codeLines <- sprintf(
        "####################################
########### Aggregation ############
####################################
qf <- aggregateFeatures(qf,
\ti = step%s_setNames,
\tname = step%s_setNames,
\tfun = %s,
\tfcol = '%s',
\tna.rm = TRUE
)\n",
        step_number - 1,
        step_number,
        method,
        fcol
    )
    codeLines
}

#' @title Code generator for join tab
#' @param step_number The step number
#'
#' @return code line generated
#' @rdname INTERNAL_codeGeneratorJoin
#' @keywords internal
#'

codeGeneratorJoin <- function(step_number) {
    codeLines <- sprintf(
        "####################################
############### Join ###############
####################################
qf <- joinAssays(
\tx = qf,
\ti = step%s_setNames,
\tname = step%s_setNames
)\n",
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for filtering missing values tab
#' @param pNA `float` threshold for filtering missing value
#' @param type feature or sample
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorNA
#' @keywords internal
#'

codeGeneratorNA <- function(qf, pNA, type, step_number) {
    codeLines <- check_for_missing_set(qf, step_number = step_number)
    if (type == "features") {
        codeLines <- c(codeLines, sprintf(
            "####################################
###### Missing value features ######
####################################
for (i in seq_along(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- filterNA(
\t\tobject = qf[[step%s_setNames[i]]],
\t\tpNA = %s
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}",
            step_number - 1,
            step_number,
            step_number - 1,
            pNA,
            step_number - 1,
            step_number
        ))
    } else {
        codeLines <- c(codeLines, sprintf(
            "####################################
###### Missing value samples #######
####################################
for(i in seq_along(step%s_setNames)){
    tableNA <- nNA(
        object = qf,
        i = step%s_setNames[i]
    )
    tableMetadata <- colData(qf[[step%s_setNames[i]]])
    tableMetadata$pNA <- tableNA$nNAcols$pNA[match(rownames(tableMetadata), tableNA$nNAcols$name)]
    qf[[step%s_setNames[i]]] <- qf[[step%s_setNames[i]]][, tableMetadata$pNA <= %s]
    qf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}",
            step_number - 1,
            step_number - 1,
            step_number - 1,
            step_number,
            step_number - 1,
            pNA,
            step_number - 1,
            step_number
        ))
    }
    codeLines
}

#' @title Code generator for normalisation tab
#' @param method the method used to do the normalisation
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorNormalisation
#' @keywords internal
#'

codeGeneratorNormalisation <- function(method, step_number) {
    codeLines <- sprintf(
        "####################################
########## Normalisation ###########
####################################
for(i in seq_along(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- normalize(
\t\tobject = qf[[step%s_setNames[i]]],
\t\tmethod = '%s'
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        method,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for Zero to NA tab
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorZeroToNA
#' @keywords internal
#'
codeGeneratorZeroToNA <- function(step_number) {
    codeLines <- sprintf(
        "####################################
############ Zero to NA ############
####################################
for(i in seq_along(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- zeroIsNA(
\t\tobject = qf[[step%s_setNames[i]]]
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for log transform tab
#' @param base the logarithm base used during transformation
#' @param pseudocount pseudocount added before log transformation
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorLogTransform
#' @keywords internal
#'
codeGeneratorLogTransform <- function(base, pseudocount, step_number) {
    codeLines <- sprintf(
        "####################################
########## Log Transform ###########
####################################
for(i in seq_along(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- logTransform(
\t\tobject = qf[[step%s_setNames[i]]],
\t\tbase = %s,
\t\tpc = %s
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        base,
        pseudocount,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for imputation tab
#' @param method imputation method used by `impute`
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorImpute
#' @keywords internal
#'
codeGeneratorImpute <- function(method, step_number) {
    specs <- imputation_method_specs()
    if (!(method %in% names(specs))) {
        stop("Unknown imputation method: ", method,
            ". Use one of the available methods: ",
            names(specs),
            call. = FALSE
        )
    }

    default_args <- specs[[method]]$call_args
    arg_lines <- paste0("\t\tmethod = '", method, "'")
    for (arg_name in names(default_args)) {
        arg_value <- default_args[[arg_name]]
        if (is.character(arg_value)) {
            arg_value <- paste0("'", arg_value, "'")
        }
        arg_lines <- c(arg_lines, paste0("\t\t", arg_name, " = ", arg_value))
    }
    args_block <- paste(arg_lines, collapse = ",\n")

    codeLines <- sprintf(
        "####################################
############ Imputation ############
####################################
for(i in seq_along(step%s_setNames)){
\tqf[[step%s_setNames[i]]] <- impute(
\t\tobject = qf[[step%s_setNames[i]]],
%s
\t)
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
        step_number - 1,
        step_number,
        step_number - 1,
        args_block,
        step_number - 1,
        step_number
    )
    codeLines
}

#' @title Code generator for filtering tab
#' @param qf QFeatures object
#' @param condition A list of filtering condition specifications
#' @param type feature or sample
#' @param step_number The step number
#'
#' @return code lines generated
#' @rdname INTERNAL_codeGeneratorFiltering
#' @keywords internal
#'

codeGeneratorFiltering <- function(qf, condition, type, step_number) {
    codeLines <- check_for_missing_set(qf, step_number = step_number)
    as_r_string_literal <- function(x) {
        encodeString(as.character(x), quote = "\"")
    }
    as_r_vector_literal <- function(values) {
        if (is.numeric(values[[1]])) {
            paste0("c(", paste(values, collapse = ","), ")")
        } else {
            escaped_values <- vapply(values, as_r_string_literal, character(1))
            paste0("c(", paste(escaped_values, collapse = ","), ")")
        }
    }
    if (type == "features") {
        if (length(condition) == 0) {
            condition_used <- "se <- se"
        } else {
            final <- "se <- se["
            for (i in seq_along(condition)) {
                annotation <- as_r_string_literal(condition[[i]]$annotation)
                if (condition[[i]]$annotation == ".qfeaturesgui_rowname") {
                    if (is_missingness_filter_operator(condition[[i]]$operator)) {
                        build_condition <- if (condition[[i]]$operator == "is_missing") {
                            "is.na(rownames(rowData(se)))"
                        } else {
                            "!is.na(rownames(rowData(se)))"
                        }
                        vector <- ""
                    } else if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("rownames(rowData(se)) %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else {
                        build_condition <- paste0("!(rownames(rowData(se)) %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    }
                } else {
                    if (is_missingness_filter_operator(condition[[i]]$operator)) {
                        build_condition <- if (condition[[i]]$operator == "is_missing") {
                            paste0("is.na(rowData(se)[[", annotation, "]])")
                        } else {
                            paste0("!is.na(rowData(se)[[", annotation, "]])")
                        }
                        vector <- ""
                    } else if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("rowData(se)[[", annotation, "]] %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else if (condition[[i]]$operator == "!=") {
                        build_condition <- paste0("!is.na(rowData(se)[[", annotation, "]]) & !(rowData(se)[[", annotation, "]] %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    } else {
                        build_condition <- paste0("rowData(se)[[", annotation, "]] ", condition[[i]]$operator, " ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    }
                }
                build_condition <- paste0(build_condition, vector)
                if (i == 1) {
                    final <- paste0(final, build_condition)
                } else {
                    final <- paste0(final, " & ", build_condition)
                }
            }
            condition_used <- paste0(final, ",]")
        }
        codeLines <- c(codeLines, sprintf(
            "####################################
######## features filtering ########
####################################
for(i in 1:length(step%s_setNames)){
\tse <- getWithColData(qf, step%s_setNames[i])
\t%s
\tqf[[step%s_setNames[i]]] <- se
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
            step_number - 1,
            step_number - 1,
            condition_used,
            step_number,
            step_number - 1,
            step_number
        ))
    } else {
        if (length(condition) == 0) {
            condition_used <- "se <- se"
        } else {
            final <- "se <- se[,"
            for (i in seq_along(condition)) {
                annotation <- as_r_string_literal(condition[[i]]$annotation)
                if (condition[[i]]$annotation == ".qfeaturesgui_rowname") {
                    if (is_missingness_filter_operator(condition[[i]]$operator)) {
                        build_condition <- if (condition[[i]]$operator == "is_missing") {
                            "is.na(rownames(colData(se)))"
                        } else {
                            "!is.na(rownames(colData(se)))"
                        }
                        vector <- ""
                    } else if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("rownames(colData(se)) %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else {
                        build_condition <- paste0("!(rownames(colData(se)) %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    }
                } else {
                    if (is_missingness_filter_operator(condition[[i]]$operator)) {
                        build_condition <- if (condition[[i]]$operator == "is_missing") {
                            paste0("is.na(colData(se)[[", annotation, "]])")
                        } else {
                            paste0("!is.na(colData(se)[[", annotation, "]])")
                        }
                        vector <- ""
                    } else if (condition[[i]]$operator == "==") {
                        build_condition <- paste0("colData(se)[[", annotation, "]] %in% ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    } else if (condition[[i]]$operator == "!=") {
                        build_condition <- paste0("!is.na(colData(se)[[", annotation, "]]) & !(colData(se)[[", annotation, "]] %in% ")
                        vector <- paste0(as_r_vector_literal(condition[[i]]$value), ")")
                    } else {
                        build_condition <- paste0("colData(se)[[", annotation, "]] ", condition[[i]]$operator, " ")
                        vector <- as_r_vector_literal(condition[[i]]$value)
                    }
                }
                build_condition <- paste0(build_condition, vector)
                if (i == 1) {
                    final <- paste0(final, build_condition)
                } else {
                    final <- paste0(final, " & ", build_condition)
                }
            }
            condition_used <- paste0(final, "]")
        }
        codeLines <- c(codeLines, sprintf(
            "####################################
######## samples filtering #########
####################################
for(i in seq_along(step%s_setNames)){
\tse <- getWithColData(qf, step%s_setNames[i])
\t%s
\tqf[[step%s_setNames[i]]] <- se
\tqf <- addAssayLink(qf, from = step%s_setNames[i], to = step%s_setNames[i])
}\n",
            step_number - 1,
            step_number - 1,
            condition_used,
            step_number,
            step_number - 1,
            step_number
        ))
    }
    codeLines
}
