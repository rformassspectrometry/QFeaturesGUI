# Error and exception handling ----

#' An wrapper function to handle errors and warnings
#' Will create a notification and add the exception to the global exception data
#' @param func function that is wrapped
#' @param component_name `str` name of the component (will be reported in the exception message)
#' @param ... arguments to be passed to the function
#'
#' @return Does not return anything but will create a notification and add the exception to the global exception data
#' @rdname INTERNAL_error_handler
#' @keywords internal
#'
#' @importFrom shiny showNotification
#' @importFrom htmltools HTML div
error_handler <- function(func, component_name, ...) {
    tryCatch(
        {
            func_call <- gsub(
                "\\s+", " ",
                paste(deparse(substitute(func(...))), collapse = " ")
            )
            func(...)
        },
        warning = function(w) {
            time <- Sys.time()
            show_exception_notification(component_name,
                type = "warning",
                time = time
            )
            add_exception(
                title = paste0("Warning in ", component_name),
                type = "warning",
                func_call = func_call,
                message = conditionMessage(w),
                full_message = w,
                time = time
            )
            suppressWarnings(func(...))
        },
        error = function(e) {
            time <- Sys.time()
            show_exception_notification(component_name,
                type = "error",
                time = time
            )
            add_exception(
                title = paste0("Error in ", component_name),
                type = "error",
                func_call = func_call,
                message = conditionMessage(e),
                full_message = e,
                time = time
            )
            return(NULL)
        }
    )
}

#' Show a standardized exception notification
#'
#' Internal helper to display a formatted Shiny notification for warnings
#' and errors. The notification includes the component name, timestamp,
#' and a hint directing the user to the exception dropdown menu.
#'
#' @param component_name `character(1)` Name of the component where the
#'   exception occurred.
#' @param type `character(1)` Notification type. One of `"error"` or
#'   `"warning"`.
#' @param time `POSIXct` Time at which the exception occurred.
#' @param duration `numeric(1)` Duration (in seconds) the notification
#'   should be displayed.
#'
#' @return Invisibly returns `NULL`. Called for its side effect.
#'
#' @keywords internal
#'
#' @importFrom shiny showNotification
#' @importFrom htmltools HTML div
#'
#' @rdname INTERNAL_show_exception_notification
show_exception_notification <- function(
      component_name,
      type = c("error", "warning"),
      time,
      duration = 30
) {
    type <- match.arg(type)

    title <- paste0(
        "<b> ",
        tools::toTitleCase(type),
        " in ",
        component_name,
        " </b> at ",
        format(time, "%H:%M:%S")
    )

    showNotification(
        HTML(
            paste0(
                div(HTML(title)),
                div(HTML(
                    "<i>Check the top right exception dropdown menu for more details</i>"
                ))
            )
        ),
        type = type,
        duration = duration
    )

    invisible(NULL)
}

#' A function that will add an exception entry to the global exception data
#'
#' @param title `str` title of the exception
#' @param type `str` type of the exception c("warning", "error")
#' @param func_call `str` function call that caused the exception
#' @param message `str` message of the exception
#' @param full_message `str` full message of the exception
#' @param time `POSIXct` time of the exception
#'
#' @return does not return anything but adds an exception to the global exception data
#' @rdname INTERNAL_add_exception
#' @keywords internal
#'
#' @importFrom shiny isolate
add_exception <- function(title, type, func_call, message, full_message, time) {
    old_data <- isolate(global_rv$exception_data)
    id <- paste0(
        "exception_",
        format(time, "%Y%m%d%H%M%OS6"),
        "_",
        nrow(old_data) + 1L
    )
    new_row <- data.frame(
        id = id,
        title = title,
        type = type,
        func_call = func_call,
        message = message,
        full_message = as.character(full_message),
        time = time,
        stringsAsFactors = FALSE
    )
    global_rv$exception_data <- rbind(new_row, old_data)
}

# Text helpers ----

#' A function that will capitalize the first letter of a string
#'
#' @param string `str` string to capitalize the first letter
#'
#' @return `str` the string with the first letter capitalized
#' @rdname INTERNAL_upper_first
#' @keywords internal
#'
upper_first <- function(string) {
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    return(string)
}

# Loading and waiter helpers ----

#' A function that will create a loading modal component
#'
#' @param msg `str` message to display in the loading modal
#'
#' @return does not return anything but will display a loading modal
#' @rdname INTERNAL_loading
#' @keywords internal
#'
#' @importFrom shiny showModal modalDialog
#' @importFrom htmltools div HTML
#'
loading <- function(msg) {
    showModal(modalDialog(
        title = "Loading",
        div(
            class = "progress",
            div(
                class = "progress-bar progress-bar-striped active",
                role = "progressbar",
                style = "width: 100%;"
            )
        ),
        HTML(paste0("<i>", msg, "</i>"))
    ))
}

#' Wrap output UI with a waiter loader that supports tag lists
#'
#' `waiter::withWaiter()` reads the target id from `element$attribs$id`.
#' Some Shiny outputs (`plotlyOutput`, `DT::dataTableOutput`) are
#' `shiny.tag.list` objects where the id is on the first child.
#' This helper resolves the id robustly and injects the waiter script
#' without mutating the output tag structure.
#'
#' @param element Shiny output element to wrap.
#' @param html Loader HTML content.
#' @param color Overlay background color.
#' @param image Optional overlay image path.
#'
#' @return A UI element wrapped with waiter behavior.
#' @rdname INTERNAL_with_output_waiter
#' @keywords internal
with_output_waiter <- function(
      element,
      html = waiter::spin_fading_circles(),
      color = "rgba(0, 0, 0, 0.25)",
      image = ""
) {
    output_id <- element$attribs$id
    if (is.null(output_id) && is.list(element) && length(element) > 0L) {
        first_child <- element[[1]]
        if (is.list(first_child) && !is.null(first_child$attribs$id)) {
            output_id <- first_child$attribs$id
        }
    }
    if (is.null(output_id)) {
        stop("`element` must be a Shiny output tag with an `id` attribute.")
    }

    escape_for_js <- function(value) {
        value <- as.character(value)[1]
        value <- gsub("\\\\", "\\\\\\\\", value)
        value <- gsub("'", "\\\\'", value, fixed = TRUE)
        value <- gsub("\r", "", value, fixed = TRUE)
        value <- gsub("\n", "", value, fixed = TRUE)
        value
    }

    output_id_js <- escape_for_js(output_id)
    html_string <- escape_for_js(html)
    color_string <- escape_for_js(color)
    image_string <- escape_for_js(image)
    event_namespace <- gsub("[^A-Za-z0-9_]", "_", as.character(output_id)[1])

    script <- paste0(
        "(function() {\n",
        "  var outputId = '", output_id_js, "';\n",
        "  var namespace = '.qfgwaiter_", event_namespace, "';\n",
        "  var hasRenderEvent = false;\n",
        "  var stateStore = window.__qfg_waiter_state || (window.__qfg_waiter_state = {});\n",
        "  var state = { visible: false };\n",
        "  stateStore[outputId] = state;\n\n",
        "  function showWaiter() {\n",
        "    if (state.visible) {\n",
        "      return;\n",
        "    }\n",
        "    waiter.show({\n",
        "      id: outputId,\n",
        "      html: '", html_string, "',\n",
        "      color: '", color_string, "',\n",
        "      image: '", image_string, "'\n",
        "    });\n",
        "    state.visible = true;\n",
        "  }\n\n",
        "  function hideWaiter() {\n",
        "    if (!state.visible) {\n",
        "      return;\n",
        "    }\n",
        "    waiter.hide(outputId);\n",
        "    state.visible = false;\n",
        "  }\n\n",
        "  function startupShow(attempt) {\n",
        "    var el = document.getElementById(outputId);\n",
        "    if (!el || !el.classList || !el.classList.contains('recalculating') || state.visible || hasRenderEvent) {\n",
        "      return;\n",
        "    }\n",
        "    if (el.offsetWidth > 0 && el.offsetHeight > 0) {\n",
        "      showWaiter();\n",
        "      return;\n",
        "    }\n",
        "    if (attempt < 8) {\n",
        "      setTimeout(function() { startupShow(attempt + 1); }, 50);\n",
        "    }\n",
        "  }\n\n",
        "  $(document).off(namespace);\n",
        "  $(document).on('shiny:outputinvalidated' + namespace + ' shiny:recalculating' + namespace, function(event) {\n",
        "    if (event.target.id !== outputId) {\n",
        "      return;\n",
        "    }\n",
        "    hasRenderEvent = true;\n",
        "    showWaiter();\n",
        "  });\n\n",
        "  $(function() {\n",
        "    setTimeout(function() { startupShow(0); }, 0);\n",
        "  });\n\n",
        "  $(document).on('shiny:value' + namespace + ' shiny:error' + namespace, function(event) {\n",
        "    if (event.target.id !== outputId) {\n",
        "      return;\n",
        "    }\n",
        "    hideWaiter();\n",
        "  });\n",
        "})();"
    )

    tagList(
        htmltools::singleton(HTML(paste0("<script>", script, "</script>"))),
        element
    )
}

#' Build the markup used by the full-page task loader
#'
#' @param caption Optional loader caption shown under the spinner
#'
#' @return HTML tags used by waiter.
#' @rdname INTERNAL_task_loader_markup
#' @keywords internal
task_loader_markup <- function(caption = NULL) {
    if (is.null(caption) || length(caption) == 0L) {
        return(waiter::spin_fading_circles())
    }
    caption <- as.character(caption)[1]
    if (is.na(caption) || !nzchar(caption)) {
        return(waiter::spin_fading_circles())
    }
    tagList(
        waiter::spin_fading_circles(),
        tags$h4(caption, style = "margin-top: 12px; color: #FFFFFF;")
    )
}

#' Start a full-page loader for long-running tasks
#'
#' @param caption Optional loader caption.
#'
#' @return A waiter loader object.
#' @rdname INTERNAL_task_loader_start
#' @keywords internal
task_loader_start <- function(caption = NULL) {
    loader <- waiter::Waiter$new(
        html = task_loader_markup(caption),
        color = "rgba(0, 0, 0, 0.35)"
    )
    loader$show()
    loader
}

#' Update the caption of a running full-page task loader
#'
#' @param loader A loader object returned by `task_loader_start()`.
#' @param caption Optional updated caption.
#'
#' @return Invisibly `NULL`.
#' @rdname INTERNAL_task_loader_update
#' @keywords internal
task_loader_update <- function(loader, caption = NULL) {
    if (is.null(loader)) {
        return(invisible(NULL))
    }
    loader$update(html = task_loader_markup(caption))
    invisible(NULL)
}

#' Stop a full-page task loader
#'
#' @param loader A loader object returned by `task_loader_start()`.
#'
#' @return Invisibly `NULL`.
#' @rdname INTERNAL_task_loader_stop
#' @keywords internal
task_loader_stop <- function(loader) {
    if (!is.null(loader)) {
        loader$hide()
    }
    invisible(NULL)
}

#' Execute an expression with a full-page task loader
#'
#' @param caption Optional loader caption.
#' @param expr Expression to execute while the loader is displayed.
#'
#' @return The value of `expr`.
#' @rdname INTERNAL_with_task_loader
#' @keywords internal
with_task_loader <- function(caption = NULL, expr) {
    loader <- task_loader_start(caption)
    on.exit(task_loader_stop(loader), add = TRUE)
    force(expr)
}

# Shared QFeatures summaries and names ----

#' Will convert a qfeatures object to a summary data.frame object
#'
#' @param qfeatures a qfeatures object
#' @param assay_labels A function taking assay names and returning display
#'   labels of the same length. Defaults to [identity()].
#'
#' @return a data.frame object
#' @rdname INTERNAL_qfeatures_to_df
#' @keywords internal
#'
qfeatures_to_df <- function(qfeatures, assay_labels = identity) {
    assay_names <- assay_labels(names(qfeatures))
    stopifnot(length(assay_names) == length(qfeatures))
    df <- data.frame(
        "Name" = rep.int(0, length(qfeatures)),
        "Class" = rep.int(0, length(qfeatures)),
        "nFeatures" = rep.int(0, length(qfeatures)),
        "nSamples" = rep.int(0, length(qfeatures)),
        "nFeaturesMetadata" = rep.int(0, length(qfeatures)),
        "nSamplesMetadata" = rep.int(0, length(qfeatures))
    )
    for (i in seq_along(qfeatures)) {
        df[i, "Name"] <- assay_names[[i]]
        df[i, "Class"] <- class(qfeatures[[i]])[[1]]
        df[i, "nFeatures"] <- nrow(qfeatures[[i]])[[1]]
        df[i, "nSamples"] <- ncol(qfeatures[[i]])[[1]]
        df[i, "nFeaturesMetadata"] <- ncol(rowData(qfeatures[[i]]))[[1]]
        df[i, "nSamplesMetadata"] <- suppressWarnings(ncol(colData(qfeatures)))
    }

    df
}


#' A function that remove the "(QFeaturesGUI#x)" suffix from a string
#' @param string `str` string to remove the suffix from
#' @return `str` the string without the suffix
#' @rdname INTERNAL_remove_QFeaturesGUI
#' @keywords internal
#'
#'
remove_QFeaturesGUI <- function(string) {
    return(gsub("_\\(QFeaturesGUI#[0-9]+\\)", "", string))
}

#' A function that will subset the assays of a QFeatures object
#' @param qfeatures `QFeatures` object to subset
#' @param pattern `str` pattern to match the assays names
#' @return `QFeatures` object with the subsetted assays
#' @rdname INTERNAL_page_assays_subset
#' @keywords internal
#'
page_assays_subset <- function(qfeatures, pattern) {
    to_process <- grepl(
        pattern,
        names(qfeatures),
        fixed = TRUE
    )
    suppressMessages(suppressWarnings(qfeatures[, , to_process]))
}

# Shared UI helpers ----

#' Create tooltip
#'
#' @description
#' A wrapper that creates a Bootstrap 3 compatible tooltip.
#'
#' @param trigger A tag or character(1) to which
#'   the tooltip is attached. Can be any Shiny UI element such as a
#'   actionButton, tags$span, or
#'   plain text. When character(1) is provided, an info icon is
#'   appended to the text.
#' @param tooltipText character(1) The text to display inside the
#'   tooltip popup.
#' @param placement character(1) The placement of the tooltip relative
#'   to the trigger element. One of "right" (default), "left",
#'   "top", or "bottom".
#' @param icon character(1) The FontAwesome icon class to use as the
#'   tooltip indicator when trigger is a character(1).
#'   Defaults to "fa-info-circle". Ignored if trigger is a tag.
#'
#' @return A tagList containing the trigger element with
#'   the tooltip attached.
#'
#' @importFrom shiny tags tagAppendAttributes
#'
#' @rdname INTERNAL_bs3Tooltip
#' @keywords internal
#'
bs3Tooltip <- function(
      trigger,
      tooltipText,
      placement = c("right", "left", "top", "bottom"),
      icon = "fa-info-circle"
) {
    stopifnot(
        is.character(tooltipText), length(tooltipText) == 1L,
        is.character(icon), length(icon) == 1L
    )
    placement <- match.arg(placement)

    if (is.character(trigger)) {
        stopifnot(length(trigger) == 1L)
        trigger <- tags$span(
            trigger,
            tags$i(
                class = paste("fa", icon),
                style = "cursor: pointer; margin-left: 5px;"
            )
        )
    } else if (!inherits(trigger, "shiny.tag")) {
        stop("'trigger' must be a character(1) or a Shiny tag object.")
    }

    existing_style <- trigger$attribs$style
    merged_style <- if (is.null(existing_style) || !nzchar(existing_style)) {
        "cursor: pointer;"
    } else {
        paste0(sub(";?\\s*$", "", existing_style), "; cursor: pointer;")
    }

    htmltools::tagList(
        htmltools::singleton(
            shiny::tags$script(shiny::HTML(
                "$(function() {
            $('body').tooltip({
                selector: '[data-toggle=\"tooltip\"]',
                container: 'body'
            });
        });"
            ))
        ),
        tagAppendAttributes(
            trigger,
            title = tooltipText,
            `data-toggle` = "tooltip",
            `data-placement` = placement,
            style = merged_style
        )
    )
}
