# Exception menu helpers ----

# This function is inspired by the messageItem function from the shinydashboardPlus package
# This custom version serves 3 goals:
# 1. It changes the icon used for the clock that is no longer available from font-awesome
# 2. It removes the icon from the messageItem and removes its margin
# 3. It returns the clicked exception box to the `input$exception_clicked` variable
#
#' Create a clickable message item for exception notifications.
#'
#' This helper builds an HTML list item that behaves like a clickable exception
#' entry in a dropdown menu. When clicked, it sets the Shiny input
#' \code{input$exception_clicked} to the provided \code{id}.
#'
#' @param id Character identifier of the exception; this value is sent back via
#'   \code{input$exception_clicked} when the item is clicked.
#' @param title Character string used as the main label or summary for the
#'   exception.
#' @param time An object representing the time of the exception (typically
#'   \code{POSIXct}); it will be formatted as \code{"\%H:\%M:\%S"} for display.
#' @param type Type of message; one of \code{"error"} or \code{"warning"}.
#'   This controls the icon and color used for the item.
#'
#' @return A \code{tags$li} HTML element representing the clickable message item,
#'   suitable for use in a dropdown menu or message container.
#' @rdname INTERNAL_interface_clickableMessageItem_custom
#' @keywords internal
clickableMessageItem <- function(id, title, time, type = c("error", "warning")) {
    type <- match.arg(type)

    icon_class <- switch(type,
        error   = "fa fa-times-circle text-red",
        warning = "fa fa-exclamation-triangle text-yellow"
    )

    tags$li(
        tags$a(
            href = "#",
            onclick = sprintf(
                "Shiny.setInputValue('exception_clicked', '%s', {priority: 'event'});",
                id
            ),
            style = "overflow:hidden;",
            tags$div(
                class = "pull-left",
                tags$i(class = icon_class)
            ),
            tags$div(
                style = paste(
                    "margin-left:30px;",
                    "display:flex;",
                    "justify-content:space-between;",
                    "align-items:center;"
                ),
                tags$span(
                    title,
                    style = paste(
                        "font-weight:600;",
                        "color:#444;",
                        "white-space:nowrap;",
                        "overflow:hidden;",
                        "text-overflow:ellipsis;",
                        "max-width:70%;",
                        "display:inline-block;"
                    )
                ),
                tags$span(
                    format(time, "%H:%M:%S"),
                    style = paste(
                        "font-size:85%;",
                        "color:#555;",
                        "white-space:nowrap;",
                        "margin-left:8px;"
                    )
                )
            ),
            tags$p(
                "Click for more details",
                style = "margin:2px 0 0 30px; font-size:85%; color:#777;"
            )
        )
    )
}

# Workflow setup and input validation ----

#' A function that will format the names of the sets of a QFeatures,
#'   initial_sets will be flagged with "_(QFeaturesGUI#0)"
#'
#' @param qfeatures the initial qfeatures that will be formatted
#' @param initial_sets the sets that will be flagged
#'
#' @return a `QFeatures` with initial sets flagged with "_(QFeaturesGUI#0)"
#' @rdname INTERNAL_format_qfeatures
#' @keywords internal
format_qfeatures <- function(qfeatures, initial_sets) {
    names(qfeatures)[initial_sets] <- paste0(
        names(qfeatures)[initial_sets],
        "_(QFeaturesGUI#0)"
    )
    qfeatures
}

#' Normalize and validate set selection indices
#'
#' Internal helper to validate and normalise assay selections for a
#' QFeatures object. This function accepts numeric, logical,
#' or character indices and always returns a validated numeric vector of
#' set positions.
#'
#' This function mirrors the behavior of internal QFeatures index validation
#' helpers, but is implemented locally to avoid relying on non-exported
#' functions.
#'
#' @param qfeatures A QFeatures object.
#'
#' @param initialSets A vector selecting assays in \code{qfeatures}.
#'   Can be:
#'   \itemize{
#'     \item numeric indices
#'     \item logical vector (same length as number of assays)
#'     \item character vector of assay names
#'   }
#'
#' @return An integer vector of validated assay indices.
#'
#' @rdname INTERNAL_normalize_initial_sets
#' @keywords internal
normalise_initial_sets <- function(qfeatures, initialSets) {
    assay_names <- names(qfeatures)
    n <- length(assay_names)

    if (is.null(initialSets) || length(initialSets) == 0) {
        stop("`initialSets` must select at least one assay.")
    }

    ## Logical indexing
    if (is.logical(initialSets)) {
        if (length(initialSets) != n) {
            stop(
                "`initialSets` is logical but its length (", length(initialSets),
                ") does not match the number of assays (", n, ")."
            )
        }
        idx <- which(initialSets)

        ## Numeric indexing
    } else if (is.numeric(initialSets)) {
        if (anyNA(initialSets)) {
            stop("`initialSets` contains NA values.")
        }
        if (any(initialSets < 1 | initialSets > n)) {
            stop("`initialSets` contains out-of-bounds indices.")
        }
        if (any(initialSets != as.integer(initialSets))) {
            stop("`initialSets` must contain whole-number indices.")
        }
        idx <- as.integer(initialSets)

        ## Character indexing (assay names)
    } else if (is.character(initialSets)) {
        missing <- setdiff(initialSets, assay_names)
        if (length(missing) > 0) {
            stop(
                "The following assay(s) were not found: ",
                paste(missing, collapse = ", ")
            )
        }
        idx <- match(initialSets, assay_names)
    } else {
        stop(
            "`initialSets` must be numeric, logical, or character (assay names)."
        )
    }

    if (length(idx) == 0) {
        stop("No assay selected by `initialSets`.")
    }

    unique(idx)
}

#' Validate and map prefilled workflow steps
#'
#' Internal helper to validate workflow step identifiers and convert them
#' to their displayed names used in the UI.
#'
#' @param prefilledSteps Character vector of workflow step identifiers
#'   (e.g. \code{"sampleFiltering"}).
#'
#' @return A character vector of display names for workflow steps.
#'
#' @keywords internal
#' @noRd
check_prefilled_steps <- function(prefilledSteps) {
    valid_steps <- c(
        sampleFiltering = "Sample Filtering",
        normalisation = "Normalisation",
        zeroToNA = "Zero to NA",
        logTransform = "Log Transform",
        imputation = "Imputation",
        featureFiltering = "Feature Filtering",
        missingValuesFeatures = "Filtering NAs by Features",
        missingValuesSamples = "Filtering NAs by Samples",
        aggregation = "Aggregation",
        join = "Join"
    )

    unknown_steps <- setdiff(prefilledSteps, names(valid_steps))
    if (length(unknown_steps) > 0) {
        stop(
            "Unknown workflow steps: ",
            paste(unknown_steps, collapse = ", ")
        )
    }

    unname(valid_steps[prefilledSteps])
}

# Workflow invalidation and naming ----

#' Extract QFeaturesGUI workflow step numbers from set names
#'
#' @param string Character vector of set names.
#'
#' @return Integer vector with the extracted step number, or `NA_integer_`
#'   when a name has no QFeaturesGUI step suffix.
#' @rdname INTERNAL_qfeaturesgui_step_number
#' @keywords internal
#'
qfeaturesgui_step_number <- function(string) {
    matches <- regexec("_\\(QFeaturesGUI#([0-9]+)\\)", string)
    parts <- regmatches(string, matches)

    vapply(parts, function(hit) {
        if (length(hit) == 2L) {
            return(as.integer(hit[[2]]))
        }
        NA_integer_
    }, integer(1))
}

qfeaturesgui_base_name <- function(string) {
    vapply(strsplit(string, "_(QFeaturesGUI#", fixed = TRUE), function(parts) {
        parts[[1]]
    }, character(1))
}

#' Get saved downstream workflow steps
#'
#' @param step_number Integer workflow step where invalidation starts.
#'
#' @return Integer vector of downstream saved step numbers.
#' @rdname INTERNAL_saved_downstream_steps
#' @keywords internal
#'
saved_downstream_steps <- function(step_number) {
    if (is.null(global_rv$step_rvs) ||
        length(global_rv$step_rvs) <= step_number) {
        return(integer(0))
    }

    downstream <- seq.int(step_number + 1L, length(global_rv$step_rvs))
    downstream[vapply(downstream, function(i) {
        global_rv$step_rvs[[i]]() > 0L
    }, logical(1))]
}

#' Build downstream invalidation message
#'
#' @param downstream_steps Integer vector of downstream saved step numbers.
#'
#' @return Character message for confirmation modals. Empty string when no
#'   downstream saved steps were invalidated.
#' @rdname INTERNAL_downstream_invalidation_message
#' @keywords internal
#'
downstream_invalidation_message <- function(downstream_steps) {
    if (length(downstream_steps) == 0L) {
        return("")
    }

    paste0(
        "This save invalidated ",
        length(downstream_steps),
        " downstream saved step",
        if (length(downstream_steps) != 1L) "s" else "",
        ". Re-run ",
        if (length(downstream_steps) != 1L) "these steps" else "this step",
        " before downloading final results."
    )
}

#' Invalidate saved workflow outputs from a step onward
#'
#' @param step_number Integer workflow step where invalidation starts.
#'
#' @return Invisibly returns the downstream saved step numbers that were
#'   invalidated. Called for side effects on `.qf$qfeatures`,
#'   `global_rv$code_lines`, and downstream `global_rv$step_rvs`.
#' @rdname INTERNAL_invalidate_steps_from
#' @keywords internal
#'
invalidate_steps_from <- function(step_number) {
    step_number <- as.integer(step_number)
    if (length(step_number) != 1L || is.na(step_number) || step_number < 1L) {
        stop("`step_number` must be a positive integer.", call. = FALSE)
    }

    downstream_steps <- saved_downstream_steps(step_number)

    if (!is.null(.qf$qfeatures)) {
        assay_steps <- qfeaturesgui_step_number(names(.qf$qfeatures))
        keep_assays <- is.na(assay_steps) | assay_steps < step_number
        .qf$qfeatures <- suppressMessages(suppressWarnings(
            .qf$qfeatures[, , keep_assays]
        ))
    }

    if (!is.null(global_rv$code_lines) && length(global_rv$code_lines) > 0L) {
        code_names <- names(global_rv$code_lines)
        code_steps <- rep(NA_integer_, length(global_rv$code_lines))

        if (!is.null(code_names)) {
            has_step_suffix <- grepl("_[0-9]+$", code_names)
            code_steps[has_step_suffix] <- as.integer(sub(
                ".*_([0-9]+)$",
                "\\1",
                code_names[has_step_suffix]
            ))
        }

        keep_code <- is.na(code_steps) | code_steps < step_number
        global_rv$code_lines <- global_rv$code_lines[keep_code]
    }

    if (!is.null(global_rv$step_rvs) &&
        length(global_rv$step_rvs) > step_number) {
        for (i in seq.int(step_number + 1L, length(global_rv$step_rvs))) {
            global_rv$step_rvs[[i]](0L)
        }
    }

    invisible(downstream_steps)
}

# QC metrics and PCA helpers ----

#' Nipals Wrapper
#'
#' This function performs Principal Component Analysis (PCA) on a SingleCellExperiment object using nipals.
#'
#' @param sce A SingleCellExperiment object. The PCA is performed on the assay of this object.
#' @param center A logical indicating whether the variables should be centered before PCA.
#' @param scale A logical indicating whether the variables should be scaled before PCA.
#' @param transpose A logical indicating whether the assay matrix should be transposed before PCA.
#'
#' @return A list with components eig, scores, loadings, fitted, ncomp, R2, iter, center, scale
#' @rdname INTERNAL_nipalsWrapper
#' @keywords internal
#'
#' @importFrom nipals nipals
#' @importFrom SummarizedExperiment assay
#'
nipalsWrapper <- function(sce, center, scale, transpose = FALSE) {
    mat <- assay(sce)
    dimMat <- dim(mat)
    mat <- mat[rowSums(is.finite(mat)) > 2, colSums(is.finite(mat)) > 2]
    if (!identical(dim(mat), dimMat)) {
        warning("Some variable(s) with less than 3 observations were removed")
    }

    if (transpose) {
        mat <- t(mat)
    }
    pca <- nipals::nipals(mat,
        center = center,
        ncomp = 6,
        scale = scale
    )
    pca
}

#' Create a plotly PCA plot
#'
#' @param df a data.frame that contains the PCA results and the color column
#' @param color_name a character string that contains the name of the color column
#' @param pca_result a pcaRes object that contains the PCA results
#' @param x_component a principal component to show on x axis
#' @param y_component a principal component to show on y axis
#'
#' @return a plotly object
#' @rdname INTERNAL_pca_plotly
#' @keywords internal
#'
#' @importFrom plotly plot_ly layout %>% hide_colorbar config
#' @importFrom stats as.formula
#'
pca_plotly <- function(df, pca_result, color_name, show_legend, x_component, y_component) {
    stopifnot(is.data.frame(df))
    if (color_name == "NULL") {
        colorFormula <- NULL
        text <- row.names(df)
        colorPalette <- grDevices::palette.colors(3, "Set 1")[1]
        hoverText <- "%{text}<extra></extra>"
        customizeData <- NULL
    } else {
        colorFormula <- as.formula(paste0("~", color_name))
        text <- ~.qfeaturesgui_row_id
        colorPalette <- if (is.numeric(df[[color_name]])) {
            grDevices::hcl.colors(10, "Viridis")
        } else {
            n_colors <- max(1L, length(unique(df[[color_name]])))
            base_palette <- grDevices::palette.colors(
                min(max(3L, n_colors), 9L),
                "Set 1"
            )
            if (n_colors > length(base_palette)) {
                grDevices::colorRampPalette(base_palette)(n_colors)
            } else {
                base_palette[seq_len(n_colors)]
            }
        }
        hoverText <- paste(
            "%{text}<br>",
            paste0(color_name, ": %{customdata}<extra></extra>")
        )
        customizeData <- as.formula(paste0("~", color_name))
    }
    plotly_args <- list(
        data = df,
        x = df[[x_component]],
        y = df[[y_component]],
        text = text,
        type = "scatter",
        mode = "markers",
        colors = colorPalette,
        hovertemplate = hoverText
    )
    if (!is.null(colorFormula)) {
        plotly_args$color <- colorFormula
    }
    if (!is.null(customizeData)) {
        plotly_args$customdata <- customizeData
    }

    plotly <- do.call(plot_ly, plotly_args) %>%
        layout(
            xaxis = list(title = paste(
                x_component,
                round(pca_result$R2[as.integer(strsplit(x_component, "PC")[[1]][2])] * 100, 2),
                "% of the variance"
            )),
            yaxis = list(title = paste(
                y_component,
                round(pca_result$R2[as.integer(strsplit(y_component, "PC")[[1]][2])] * 100, 2),
                "% of the variance"
            )),
            showlegend = show_legend,
            legend = list(
                x = 1,
                y = 1,
                traceorder = "normal",
                font = list(
                    family = "sans-serif",
                    size = 10,
                    color = "black"
                ),
                bgcolor = "#E2E2E2",
                bordercolor = "#FFFFFF",
                borderwidth = 2
            )
        ) %>%
        config(displaylogo = FALSE, toImageButtonOptions = list(
            format = "svg",
            filename = "pca_plot",
            height = 500,
            width = 700,
            scale = 1
        ))
    if (!show_legend) {
        plotly <- hide_colorbar(plotly)
    }
    return(plotly)
}

# Saving processed assays ----

#' Check whether an assay set is empty
#'
#' @param assay_object a SummarizedExperiment-like assay object
#'
#' @return a logical scalar
#' @rdname INTERNAL_is_empty_set
#' @keywords internal
is_empty_set <- function(assay_object) {
    nrow(assay_object) == 0L || ncol(assay_object) == 0L
}

#' A function that adds processed assays to the non-reactive global QFeatures store
#'
#' @param processed_qfeatures `QFeatures` object whose assays will be added to
#'   `.qf$qfeatures`. Each assay is renamed with a step-number suffix following
#'   the `_(QFeaturesGUI#<step_number>)_<type>_<step_number>` convention, and
#'   an assay link from the parent assay is recorded.
#'   Empty assays are skipped and not added.
#' @param step_number `int` the workflow step number, used to construct the new
#'   assay names and links
#' @param type `character(1)` label describing the processing type (e.g.
#'   `"samples_filtering"`, `"log_transformation"`), embedded in the new assay
#'   names
#' @param varFrom see [QFeatures::addAssayLink].
#' @param varTo see [QFeatures::addAssayLink].
#' @rdname INTERNAL_add_assays_to_global_rv
#' @keywords internal
#'
#' @return Invisibly `NULL`. Called for its side effect: assays are written into
#'   `.qf$qfeatures` (the non-reactive global environment store).
#' @importFrom QFeatures addAssayLink
#' @importFrom shinyalert shinyalert
add_assays_to_global_rv <- function(processed_qfeatures, step_number, type, varFrom = NULL, varTo = NULL) {
    invalidated_downstream_steps <- invalidate_steps_from(step_number)

    n_added <- 0L
    n_skipped_empty <- 0L
    for (name in names(processed_qfeatures)) {
        assay_to_add <- processed_qfeatures[[name]]
        if (is_empty_set(assay_to_add)) {
            n_skipped_empty <- n_skipped_empty + 1L
            next
        }

        new_name <- paste0(
            qfeaturesgui_base_name(name),
            "_(QFeaturesGUI#", step_number, ")",
            "_", type, "_", step_number
        )

        .qf$qfeatures[[new_name]] <- assay_to_add
        if (is.null(varFrom) || is.null(varTo)) {
            .qf$qfeatures <- addAssayLink(
                .qf$qfeatures,
                from = name,
                to = new_name
            )
            n_added <- n_added + 1L
        } else {
            .qf$qfeatures <- addAssayLink(
                .qf$qfeatures,
                from = name,
                to = new_name,
                varFrom = varFrom,
                varTo = varTo
            )
            n_added <- n_added + 1L
        }
    }
    alert_text <- paste0(
        n_added, " set", if (n_added != 1L) "s" else "",
        " added to QFeatures."
    )
    if (n_skipped_empty > 0L) {
        alert_text <- paste0(
            alert_text, " ",
            n_skipped_empty, " empty set",
            if (n_skipped_empty != 1L) "s were" else " was",
            " skipped."
        )
    }
    invalidation_message <- downstream_invalidation_message(
        invalidated_downstream_steps
    )
    if (nzchar(invalidation_message)) {
        alert_text <- paste(alert_text, invalidation_message, sep = "\n\n")
    }
    shinyalert(
        title = "Step saved",
        text = alert_text,
        type = "success",
        confirmButtonCol = "#3c8dbc",
        closeOnClickOutside = TRUE
    )
}


# Processing step helpers ----

#' A function that will logTransform all the assays of a qfeatures
#' @param qfeatures `QFeatures` object to logTransform
#' @param base `numeric` base of the log transformation
#' @param pseudocount `numeric` pseudocount to add to the data
#' @return `QFeatures` object with the log transformed assays
#' @rdname INTERNAL_log_transform_qfeatures
#' @keywords internal
#' @importFrom QFeatures logTransform QFeatures
#' @importFrom SummarizedExperiment colData
#'

log_transform_qfeatures <- function(qfeatures, base, pseudocount) {
    el <- lapply(names(qfeatures), function(name) {
        QFeatures::logTransform(
            object = qfeatures[[name]],
            base = base,
            pc = pseudocount
        )
    })
    names(el) <- names(qfeatures)
    QFeatures(el, colData = colData(qfeatures))
}

#' A function that will normalise all the assays of a qfeatures
#'
#' @param qfeatures `QFeatures` object to normalise
#' @param method `str` method to use for normalisation (see `QFeatures::normalize`)
#' @return `QFeatures` object with the normalised assays
#' @rdname INTERNAL_normalisation_qfeatures
#' @keywords internal
#' @importFrom QFeatures normalize QFeatures
#' @importFrom SummarizedExperiment colData

normalisation_qfeatures <- function(qfeatures, method) {
    el <- lapply(names(qfeatures), function(name) {
        QFeatures::normalize(
            object = qfeatures[[name]],
            method = method
        )
    })
    names(el) <- names(qfeatures)
    QFeatures(el, colData = colData(qfeatures))
}

#' A function that lists the imputation methods supported by the GUI
#'
#' @return A named list describing supported imputation methods.
#' @rdname INTERNAL_imputation_method_specs
#' @keywords internal
imputation_method_specs <- function() {
    list(
        knn = list(
            package = "impute",
            description = "K-nearest neighbors imputation.",
            default_parameters = c(
                "MARGIN = 1",
                "k = 10",
                "rowmax = 0.5",
                "colmax = 0.8",
                "maxp = 1500",
                "rng.seed = 362436069"
            ),
            call_args = list(MARGIN = 1L)
        ),
        MinDet = list(
            package = NULL,
            description = "Minimum deterministic imputation.",
            default_parameters = c("q = 0.01", "MARGIN = 2"),
            call_args = list(q = 0.01, MARGIN = 2L)
        ),
        zero = list(
            package = NULL,
            description = "Replace missing values with zero.",
            default_parameters = character(0),
            call_args = list()
        )
    )
}


#' A function that returns imputation methods available in the current session
#'
#' @return `character()` vector of method names accepted by the GUI.
#' @rdname INTERNAL_available_imputation_methods
#' @keywords internal
available_imputation_methods <- function() {
    specs <- imputation_method_specs()
    names(specs)[vapply(specs, function(spec) {
        is.null(spec$package) || requireNamespace(spec$package, quietly = TRUE)
    }, logical(1))]
}


#' A function that validates an imputation method before it is used
#'
#' @param method `character(1)` imputation method used by `QFeatures::impute`
#' @return The method specification entry invisibly.
#' @rdname INTERNAL_assert_imputation_method_available
#' @keywords internal
assert_imputation_method_available <- function(method) {
    specs <- imputation_method_specs()
    if (!(method %in% names(specs))) {
        stop("Unknown imputation method: ", method,
            ". Use one of the available methods: ",
            names(specs),
            call. = FALSE
        )
    }

    required_package <- specs[[method]]$package
    if (!is.null(required_package) && !requireNamespace(required_package, quietly = TRUE)) {
        stop(
            "Imputation method '", method, "' requires the optional package '",
            required_package, "'. Install it to use this method.",
            call. = FALSE
        )
    }

    invisible(specs[[method]])
}


#' A function that will impute all assays of a qfeatures object
#'
#' @param object `QFeatures` object to impute
#' @param impute_method `character(1)` imputation method used by `QFeatures::impute`
#' @param ... additional parameters forwarded to `QFeatures::impute`
#' @return `QFeatures` object with imputed assays
#' @rdname INTERNAL_impute_qfeatures
#' @keywords internal
impute_qfeatures <- function(object, impute_method, ...) {
    if (identical(impute_method, "none")) {
        return(object)
    }

    assert_imputation_method_available(impute_method)

    source_names <- names(object)
    imputed_names <- paste0(source_names, "__imputed_tmp")
    imputed_qf <- QFeatures::impute(
        object = object,
        i = seq_along(object),
        name = imputed_names,
        method = impute_method,
        ...
    )

    out <- suppressMessages(suppressWarnings(imputed_qf[, , imputed_names]))
    names(out) <- source_names
    out
}


# Density and distribution plots ----

#' A function that returns ridge density plots of intensities by sample group
#'
#' @param qfeatures `QFeatures` object
#' @param color `character(1)` optional column name in `colData` used to
#'   group samples. If `NULL`, all samples are treated as one group.
#' @param title `character(1)` title to display on the plot
#' @return A plotly object containing one ridge per sample group.
#'
#' @rdname INTERNAL_density_by_sample_plotly
#' @keywords internal
#' @importFrom SummarizedExperiment assay colData
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
density_by_sample_plotly <- function(qfeatures, color = NULL, title = "Density by sample group") {
    if (length(qfeatures) == 0L) {
        return(plot_ly() %>% layout(title = title))
    }

    sample_metadata <- as.data.frame(colData(qfeatures))
    sample_names <- rownames(sample_metadata)
    if (is.null(sample_names) || length(sample_names) == 0L) {
        sample_names <- colnames(qfeatures[[1]])
    }

    if (is.null(color)) {
        sample_groups <- rep("All samples", length(sample_names))
    } else {
        if (!(color %in% colnames(sample_metadata))) {
            stop("Unknown color column: ", color)
        }
        sample_groups <- as.character(sample_metadata[[color]])
        sample_groups[is.na(sample_groups)] <- "NA"
        sample_groups[!nzchar(sample_groups)] <- "NA"
    }
    names(sample_groups) <- sample_names

    combined_df <- data.frame(
        intensity = numeric(),
        group = character(),
        stringsAsFactors = FALSE
    )
    assay_dfs <- lapply(names(qfeatures), function(assayName) {
        assayData <- assay(qfeatures[[assayName]])

        intensities <- as.vector(assayData)
        sampleNames <- rep(colnames(assayData), each = nrow(assayData))
        groups <- as.character(sample_groups[sampleNames])
        groups[is.na(groups)] <- "NA"

        data.frame(
            intensity = intensities,
            group = groups,
            stringsAsFactors = FALSE
        )
    })
    if (length(assay_dfs) > 0L) {
        combined_df <- do.call(rbind, assay_dfs)
    }
    combined_df <- combined_df[is.finite(combined_df$intensity), , drop = FALSE]
    if (nrow(combined_df) == 0L) {
        return(plot_ly() %>% layout(title = title))
    }

    group_levels <- sort(unique(combined_df$group))
    if (length(group_levels) <= 1L) {
        ridge_colors <- setNames("steelblue", group_levels)
    } else {
        base_palette <- grDevices::palette.colors(
            min(8L, length(group_levels)),
            "Set 2"
        )
        if (length(group_levels) > length(base_palette)) {
            base_palette <- grDevices::colorRampPalette(base_palette)(length(group_levels))
        } else {
            base_palette <- base_palette[seq_along(group_levels)]
        }
        ridge_colors <- setNames(base_palette, group_levels)
    }

    p <- plotlyridges(
        data = combined_df,
        vardens = "intensity",
        varcat = "group",
        category_colors = ridge_colors
    )
    p %>% layout(title = title)
}

# This function comes from the github repo rushkin/bitsandends
# Thanks to iliarushkin for this function

#' Build a simple interactive ridge density plot
#'
#' @param data `data.frame` to plot.
#' @param vardens `character(1)` column name in `data` used as the density variable.
#' @param varcat `character(1)` column name in `data` used as the category variable.
#' @param category_colors optional named `character` vector mapping each
#'   category in `varcat` to a color. If `NULL`, a default color is used.
#' @param fillopacity `numeric(1)` alpha used for ridge fill color.
#' @param linewidth `numeric(1)` ridge line width.
#' @param height optional `numeric(1)` plot height passed to `plotly`.
#' @param width optional `numeric(1)` plot width passed to `plotly`.
#' @return a plotly object
#'
#' @rdname INTERNAL_plotlyridges
#' @keywords internal
#' @importFrom plotly plot_ly add_trace add_annotations layout
#'
plotlyridges <- function(data, vardens, varcat, category_colors = NULL, fillopacity = 0.6, linewidth = 0.5, height = NULL, width = NULL) {
    empty_plot <- function() {
        p <- plotly::plot_ly(type = "scatter", mode = "lines", height = height, width = width)
        p <- plotly::add_annotations(
            p,
            text = "No finite values available for density estimation.",
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5,
            showarrow = FALSE
        )
        plotly::layout(
            p,
            showlegend = FALSE,
            xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
            yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE)
        )
    }

    as_rgba <- function(col, alpha) {
        col_rgb <- as.vector(grDevices::col2rgb(col)) / 255
        grDevices::rgb(col_rgb[1], col_rgb[2], col_rgb[3], alpha = alpha)
    }

    density_hover_text <- function(values) {
        values <- values[is.finite(values)]
        if (length(values) == 0L) {
            return("")
        }
        q <- stats::quantile(values)
        paste0(
            "Observations: ", prettyNum(length(values), big.mark = ","),
            "<br>Median: ", round(q[3], 2),
            "<br>Range: [", round(q[1], 2), ", ", round(q[5], 2), "]",
            "<br>Interquartile Range: [", round(q[2], 2), ", ", round(q[4], 2), "]"
        )
    }

    build_density_curve <- function(values) {
        values <- values[is.finite(values)]
        if (length(values) == 0L) {
            return(NULL)
        }

        density_values <- if (length(values) >= 2L) {
            tryCatch(stats::density(values, bw = "nrd0", n = 512), error = function(e) NULL)
        } else {
            # Fallback for one-point groups.
            from <- values[1] - 0.5
            to <- values[1] + 0.5
            x <- seq(from, to, length.out = 512)
            y <- stats::dnorm(x, mean = values[1], sd = 0.1)
            list(x = x, y = y)
        }

        if (is.null(density_values)) {
            return(NULL)
        }
        density_values[c("x", "y")]
    }

    stopifnot(is.data.frame(data))
    stopifnot(vardens %in% colnames(data), varcat %in% colnames(data))

    values <- data[, vardens]
    groups <- as.character(data[, varcat])
    groups[is.na(groups) | !nzchar(groups)] <- "NA"
    keep <- is.finite(values)
    values <- values[keep]
    groups <- groups[keep]
    if (length(values) == 0L) {
        return(empty_plot())
    }

    r <- range(values, finite = TRUE)
    if (!all(is.finite(r))) {
        return(empty_plot())
    }

    split_values <- split(values, groups, drop = TRUE)
    split_values <- split_values[vapply(split_values, function(x) any(is.finite(x)), logical(1))]
    group_names <- names(split_values)
    if (length(group_names) == 0L) {
        return(empty_plot())
    }

    curves <- lapply(split_values, build_density_curve)
    keep_curves <- vapply(curves, Negate(is.null), logical(1))
    curves <- curves[keep_curves]
    split_values <- split_values[keep_curves]
    group_names <- names(curves)
    if (length(group_names) == 0L) {
        return(empty_plot())
    }

    linecolors <- rep("steelblue4", length(group_names))
    fillcolors <- rep(as_rgba("steelblue", fillopacity), length(group_names))
    if (!is.null(category_colors)) {
        category_colors <- as.character(category_colors)
        if (!is.null(names(category_colors))) {
            mapped_colors <- unname(category_colors[group_names])
        } else {
            mapped_colors <- rep(category_colors, length.out = length(group_names))
        }
        mapped_colors[is.na(mapped_colors) | !nzchar(mapped_colors)] <- "steelblue4"
        linecolors <- mapped_colors
        fillcolors <- vapply(linecolors, as_rgba, alpha = fillopacity, character(1))
    }

    hover_text <- vapply(split_values, density_hover_text, character(1))

    ymax <- max(unlist(lapply(curves, `[[`, "y")), na.rm = TRUE)
    if (!is.finite(ymax) || ymax <= 0) {
        ymax <- 1
    }
    curves <- lapply(curves, function(curve) {
        curve$y <- 0.9 * curve$y / ymax
        curve$y[!is.finite(curve$y)] <- 0
        curve
    })

    p <- plotly::plot_ly(type = "scatter", mode = "lines", height = height, width = width)

    for (i in rev(seq_along(group_names))) {
        p <- plotly::add_trace(
            p,
            x = curves[[i]]$x,
            y = i,
            line = list(color = linecolors[i], width = linewidth),
            showlegend = FALSE,
            hoverinfo = "none"
        )
        p <- plotly::add_trace(
            p,
            x = curves[[i]]$x,
            y = curves[[i]]$y + i,
            fill = "tonexty",
            fillcolor = fillcolors[i],
            line = list(color = linecolors[i], width = linewidth),
            showlegend = FALSE,
            name = group_names[i],
            hoverinfo = "text",
            text = hover_text[i]
        )
    }
    plotly::layout(
        p,
        yaxis = list(
            tickmode = "array",
            tickvals = seq_along(group_names),
            ticktext = group_names,
            showline = TRUE
        ),
        xaxis = list(range = r)
    )
}

# Feature visualization helpers ----

#' A function that create a data frame that contains the intensities, the sample names (+ one col of colData and one col of rowData)
#'
#' @param qfeatures `QFeatures` object
#' @param sample_column `str` column of colData to use as sample names
#' @param feature_column `str` column of rowData to use as feature names
#' @return a data.frame
#'
#' @rdname INTERNAL_summarize_assays_to_df
#' @keywords internal
#' @importFrom SummarizedExperiment assay colData rowData
#' @importFrom tidyr pivot_longer
#'

summarize_assays_to_df <- function(qfeatures, sample_column, feature_column = NULL) {
    combined_df <- data.frame(
        PSM = character(0),
        sample = character(0),
        intensity = numeric(0),
        sample_type = character(0),
        stringsAsFactors = FALSE
    )
    if (!is.null(feature_column)) {
        combined_df$feature_type <- character(0)
    }
    assay_dfs <- list()
    for (assayName in names(qfeatures)) {
        assayData <- as.data.frame(assay(qfeatures[[assayName]]))
        assayData$PSM <- rownames(assayData)
        if (ncol(assayData) <= 1L) {
            next
        }
        assayData <- pivot_longer(
            assayData,
            cols = -PSM,
            names_to = "sample",
            values_to = "intensity"
        )

        matched_indices <- match(assayData$sample, rownames(colData(qfeatures)))
        colDataAssay <- colData(qfeatures)
        colDataAssay$Rownames <- rownames(colData(qfeatures))
        assayData$sample_type <- as.vector(colDataAssay[matched_indices, sample_column])

        if (!is.null(feature_column)) {
            matched_indices <- match(assayData$PSM, rownames(rowData(qfeatures[[assayName]])))
            assayData$feature_type <- as.vector(rowData(qfeatures[[assayName]])[matched_indices, feature_column])
        }
        assay_dfs[[length(assay_dfs) + 1L]] <- assayData
    }
    if (length(assay_dfs) > 0L) {
        combined_df <- do.call(rbind, assay_dfs)
    }
    combined_df
}

#' A function that return the boxplot of the intensities of all features by an sample annotation
#'
#' @param assays_df a data.frame that contains the intensities, the sample names (+ one col of colData and one col of rowData)
#'
#' @return a plot
#'
#' @rdname INTERNAL_feature_boxplot
#' @keywords internal
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_violin
#'

features_boxplot <- function(assays_df) {
    plot <- ggplot(assays_df, aes(
        x = sample_type,
        y = intensity,
        colour = sample_type,
        fill = sample_type
    )) +
        geom_violin(aes(alpha = 0.5))

    suppressWarnings(ggplotly(plot))
}

#' A function that will return the boxplot of the intensities of an individual feature by a sample annotation
#'
#' @param assays_df a data.frame that contains the intensities, the sample names (+ one col of colData and one col of rowData)
#' @param feature `str` feature to plot
#'
#' @return a plot
#'
#' @rdname INTERNAL_unique_feature_boxplot
#' @keywords internal
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_boxplot
#'

unique_feature_boxplot <- function(assays_df, feature) {
    df <- assays_df[assays_df$feature_type == feature, , drop = FALSE]
    if (nrow(df) == 0) {
        return(plot_ly(
            x = numeric(0),
            y = numeric(0),
            type = "scatter",
            mode = "markers"
        ) %>%
            add_annotations(
                text = "No values available for this feature.",
                xref = "paper",
                yref = "paper",
                x = 0.5,
                y = 0.5,
                showarrow = FALSE
            ) %>%
            layout(
                showlegend = FALSE,
                xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
                yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE)
            ))
    }
    plot <- ggplot(df, aes(x = sample_type, y = intensity, colour = sample_type)) +
        geom_boxplot()
    suppressWarnings(ggplotly(plot))
}

# Filtering metrics and annotations ----

#' A function that will return the percentage of samples/features that have been removed
#'
#' @param qfeatures_before_filtering a qfeatures object that haven't been filtered.
#' @param qfeatures_after_filtering  a qfeatures object that have been filtered.
#' @param type features or samples.
#'
#' @return a percentage
#'
#' @rdname INTERNAL_percent_removed
#' @keywords internal
#' @importFrom QFeatures rbindRowData
#' @importFrom SummarizedExperiment colData

percent_removed <- function(qfeatures_before_filtering, qfeatures_after_filtering, type) {
    type <- match.arg(type, c("features", "samples"))
    if (type == "features") {
        before_features_nrow <- count_features_rows(qfeatures_before_filtering)
        after_features_nrow <- count_features_rows(qfeatures_after_filtering)
        if (before_features_nrow == 0L) {
            return(0)
        }
        pct_removed <- round(
            (before_features_nrow - after_features_nrow)
            / before_features_nrow * 100,
            digits = 1
        )
    } else {
        ncol_before_filtering <- nrow(
            colData(
                qfeatures_before_filtering
            )
        )
        ncol_after_filtering <- nrow(
            colData(
                qfeatures_after_filtering
            )
        )
        if (ncol_before_filtering == 0L) {
            return(0)
        }
        pct_removed <- round(
            (ncol_before_filtering - ncol_after_filtering)
            / ncol_before_filtering * 100,
            digits = 1
        )
    }
    return(pct_removed)
}

#' A function that will return the number of features by rows
#'
#' @param qfeatures a qfeatures object.
#'
#' @return an integer
#'
#' @rdname INTERNAL_count_features_rows
#' @keywords internal
#'

count_features_rows <- function(qfeatures) {
    sum(vapply(seq_along(qfeatures), function(i) {
        nrow(qfeatures[[i]])
    }, integer(1)))
}

#' A function that will return the number of samples/features that have been removed
#'
#' @param qfeatures_before_filtering a qfeatures object that haven't been filtered.
#' @param qfeatures_after_filtering  a qfeatures object that have been filtered.
#' @param type features or samples.
#'
#' @return an integer
#'
#' @rdname INTERNAL_number_removed
#' @keywords internal
#' @importFrom QFeatures rbindRowData
#' @importFrom SummarizedExperiment colData
#'

number_removed <- function(qfeatures_before_filtering, qfeatures_after_filtering, type) {
    type <- match.arg(type, c("features", "samples"))
    if (type == "features") {
        nb_removed <- count_features_rows(qfeatures_before_filtering) -
            count_features_rows(qfeatures_after_filtering)
    } else {
        nb_removed <- nrow(
            colData(
                qfeatures_before_filtering
            )
        ) - nrow(
            colData(
                qfeatures_after_filtering
            )
        )
    }
    return(nb_removed)
}

#' Internal function that return the available variables (column
#' names) from the sample annotations (colData) or feature annotations
#' (rowData) of a QFeatures object. The function is robust against
#' empty QFeatures objects.
#'
#' @param x a QFeatures object
#' @param what a character(1), either "rowData" or "colData" depending
#'   on whether to fetch feature annotations or sample annotations,
#'   respectively.
#'
#' @return a vector of column names or an empty vector if no columns
#'   are found.
#'
#' @rdname INTERNAL_annotation_cols
#' @keywords internal
annotation_cols <- function(x, what) {
    if (length(x) == 0) {
        character(0)
    } else {
        annot <- switch(what,
            rowData = rowData(x)[[1]],
            colData = colData(x)
        )
        colnames(annot)
    }
}

# Aggregation step helpers ----

#' A function that will aggregate all the assays of a qfeatures
#'
#' @param qfeatures `QFeatures` object to aggregate.
#' @param method `character(1)` naming the aggregation function to use. Must be one of
#'   `"robustSummary"`, `"medianPolish"`, `"colMeans"`, `"colMedians"`, or `"colSums"`.
#' @param fcol `character(1)` naming a `rowData` variable that defines how to aggregate
#'   the features within each assay. This variable is either a character or a (possibly
#'   sparse) matrix.
#' @return A `QFeatures` object with assays aggregated according to `fcol` using the
#'   selected `method`.
#' @rdname INTERNAL_aggregation_qfeatures
#' @keywords internal
#' @importFrom QFeatures normalize QFeatures aggregateFeatures
#' @importFrom SummarizedExperiment colData
#' @importFrom matrixStats colMedians
#' @importFrom MsCoreUtils robustSummary medianPolish
#' @importFrom waiter Waiter spin_fading_circles
#'
aggregation_qfeatures <- function(
      qfeatures, method,
      fcol
) {
    n <- length(qfeatures)
    caption <- if (n > 0L) {
        paste0("Aggregation of 1/", n, " sets")
    } else {
        "Aggregation in progress"
    }
    loader <- task_loader_start(caption)
    on.exit(task_loader_stop(loader), add = TRUE)

    el <- lapply(seq_along(qfeatures), function(i) {
        name <- names(qfeatures)[i]
        task_loader_update(loader, paste0("Aggregation of ", i, "/", n, " sets"))
        aggregateFeatures(
            object = qfeatures[[name]],
            fun = list(
                robustSummary = MsCoreUtils::robustSummary,
                medianPolish = MsCoreUtils::medianPolish,
                colMeans = base::colMeans,
                colMedians = matrixStats::colMedians,
                colSums = base::colSums
            )[[method]],
            fcol = fcol,
            na.rm = TRUE
        )
    })
    names(el) <- names(qfeatures)
    QFeatures(el, colData = colData(qfeatures))
}

# Join step helpers ----

#' A function that will join all the assays of a qfeatures
#'
#' @param qfeatures `QFeatures` object to join
#'
#' @return `QFeatures` object with the joined assays
#'
#' @rdname INTERNAL_join_qfeatures
#'
#' @keywords internal
#'
#' @importFrom QFeatures joinAssays createPrecursorId
#'
join_qfeatures <- function(qfeatures, fcol, fcol2 = NULL) {
    if (!is.null(fcol2)) {
        fcol_combined <- paste0(fcol, "_", fcol2)
        qfeatures <- createPrecursorId(
            qfeatures,
            name = fcol_combined,
            fcols = c(fcol, fcol2)
        )
        fcol <- fcol_combined
    }
    qf <- joinAssays(qfeatures, names(qfeatures), fcol = fcol)
    suppressMessages(suppressWarnings(qf[, , "joinedAssay"]))
}

#' A function that will add the assays to the package-level `.qf$qfeatures`
#' object when performing a joining step, where multiple parent assays
#' are linked to one child assay.
#'
#' @param processed_qfeatures `QFeatures` object whose assays will be added to
#'   the package-level `.qf$qfeatures` object
#' @param step_number `int` number of the step
#' @param type A `character(1)` providing the name of the step.
#' @param featuresType which type of features will be joined
#'
#' @rdname INTERNAL_add_assays_to_global_rv
#' @keywords internal
#'
#' @return (NULL) does not return anything but will add the assays to the
#'   package-level `.qf$qfeatures` object
#' @importFrom QFeatures addAssayLink
#' @importFrom shinyalert shinyalert
#'
add_joined_assay_to_global_rv <- function(processed_qfeatures, step_number, featuresType, type) {
    invalidated_downstream_steps <- invalidate_steps_from(step_number)

    name <- names(processed_qfeatures)[length(processed_qfeatures)]
    new_name <- paste0(
        featuresType, "_",
        qfeaturesgui_base_name(name),
        "_(QFeaturesGUI#", step_number, ")",
        "_", type, "_", step_number
    )
    .qf$qfeatures[[new_name]] <- processed_qfeatures[[name]]
    from_pattern <- paste0("QFeaturesGUI#", step_number - 1, "\\)")
    from_names <- grep(from_pattern, names(.qf$qfeatures), value = TRUE)
    .qf$qfeatures <- addAssayLink(
        .qf$qfeatures,
        from = from_names,
        to = new_name
    )

    alert_text <- "1 set added to QFeatures."
    invalidation_message <- downstream_invalidation_message(
        invalidated_downstream_steps
    )
    if (nzchar(invalidation_message)) {
        alert_text <- paste(alert_text, invalidation_message, sep = "\n\n")
    }
    shinyalert(
        title = "Step saved",
        text = alert_text,
        type = "success",
        confirmButtonCol = "#3c8dbc",
        closeOnClickOutside = TRUE
    )
}
