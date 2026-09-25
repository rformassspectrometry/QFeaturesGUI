test_that("normalise_initial_sets supports valid selector types", {
    qf <- make_test_qfeatures()

    expect_equal(normalise_initial_sets(qf, c(2, 1, 2)), c(2L, 1L))
    expect_equal(normalise_initial_sets(qf, c(TRUE, FALSE)), 1L)
    expect_equal(normalise_initial_sets(qf, c("set2", "set1")), c(2L, 1L))
})

test_that("normalise_initial_sets rejects invalid selectors", {
    qf <- make_test_qfeatures()

    expect_error(
        normalise_initial_sets(qf, NULL),
        "select at least one assay"
    )
    expect_error(
        normalise_initial_sets(qf, logical(0)),
        "select at least one assay"
    )
    expect_error(
        normalise_initial_sets(qf, c(TRUE, FALSE, TRUE)),
        "does not match the number of assays"
    )
    expect_error(
        normalise_initial_sets(qf, 3),
        "out-of-bounds"
    )
    expect_error(
        normalise_initial_sets(qf, 1.5),
        "whole-number"
    )
    expect_error(
        normalise_initial_sets(qf, NA_real_)
    )
    expect_error(
        normalise_initial_sets(qf, "missing"),
        "not found"
    )
    expect_error(
        normalise_initial_sets(qf, list("set1")),
        "must be numeric, logical, or character"
    )
})

test_that("check_prefilled_steps maps valid workflow identifiers", {
    expect_equal(
        check_prefilled_steps(c("sampleFiltering", "normalisation", "join")),
        c("Sample Filtering", "Normalisation", "Join")
    )
    expect_equal(check_prefilled_steps(character()), character())
    expect_error(
        check_prefilled_steps(c("sampleFiltering", "badStep")),
        "Unknown workflow steps: badStep"
    )
})

test_that("process workflow name helpers add and parse step suffixes", {
    qf <- make_test_qfeatures()
    formatted <- format_qfeatures(qf, c(1, 2))

    expect_equal(
        names(formatted),
        c("set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#0)")
    )
    expect_equal(
        qfeaturesgui_step_number(
            c("raw", "set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#12)_x")
        ),
        c(NA_integer_, 0L, 12L)
    )
    expect_equal(
        qfeaturesgui_base_name(
            c("set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#12)_x", "raw")
        ),
        c("set1", "set2", "raw")
    )
})

test_that("downstream invalidation messages describe affected workflow steps", {
    expect_equal(downstream_invalidation_message(integer()), "")
    expect_match(
        downstream_invalidation_message(2L),
        "1 downstream saved step"
    )
    expect_match(
        downstream_invalidation_message(c(2L, 3L)),
        "2 downstream saved steps"
    )
})

test_that("summarize_assays_to_df returns long assay data with annotations", {
    qf <- make_test_qfeatures()

    summary <- summarize_assays_to_df(
        qf,
        sample_column = "condition",
        feature_column = "feature_class"
    )

    expect_named(
        summary,
        c("PSM", "sample", "intensity", "sample_type", "feature_type"),
        ignore.order = TRUE
    )
    expect_equal(nrow(summary), 18L)

    row <- summary[summary$PSM == "f1" & summary$sample == "s1", ]
    expect_equal(row$intensity, 1)
    expect_equal(row$sample_type, "ctrl")
    expect_equal(row$feature_type, "pep")
})

test_that("annotation_cols reports row and column annotation names", {
    qf <- make_test_qfeatures()

    expect_equal(
        annotation_cols(qf, "rowData"),
        c("feature_class", "score", "protein")
    )
    expect_equal(
        annotation_cols(qf, "colData"),
        c("batch", "condition", "sample_score")
    )
    empty_qf <- suppressWarnings(suppressMessages(qf[, , FALSE]))
    expect_equal(annotation_cols(empty_qf, "rowData"), character())
})

test_that("feature and sample removal metrics are computed from dimensions", {
    before <- make_test_qfeatures()
    after_features <- before
    after_features[["set1"]] <- after_features[["set1"]][1:2, ]
    after_features[["set2"]] <- after_features[["set2"]][1, , drop = FALSE]
    after_samples <- before[, c(TRUE, FALSE, TRUE), ]

    expect_equal(count_features_rows(before), 6L)
    expect_equal(number_removed(before, after_features, "features"), 3L)
    expect_equal(percent_removed(before, after_features, "features"), 50)
    expect_equal(number_removed(before, after_samples, "samples"), 1L)
    expect_equal(percent_removed(before, after_samples, "samples"), 33.3)
    expect_error(number_removed(before, after_samples, "bad_type"))
})

test_that("is_empty_set detects assays without rows or columns", {
    qf <- make_test_qfeatures()

    expect_true(is_empty_set(qf[["set1"]][0, ]))
    expect_true(is_empty_set(qf[["set1"]][, 0]))
    expect_false(is_empty_set(qf[["set1"]]))
})

test_that("log_transform_qfeatures transforms all assays and preserves names", {
    qf <- make_test_qfeatures()

    logged <- log_transform_qfeatures(qf, base = 10, pseudocount = 1)

    expect_equal(names(logged), names(qf))
    expect_equal(
        SummarizedExperiment::assay(logged[["set2"]]),
        log10(SummarizedExperiment::assay(qf[["set2"]]) + 1),
        tolerance = 1e-12
    )
})

test_that("imputation method metadata is internally consistent", {
    specs <- imputation_method_specs()

    expect_named(specs, c("knn", "MinDet", "zero"))
    expect_equal(specs$knn$call_args, list(MARGIN = 1L))
    expect_equal(specs$MinDet$call_args, list(q = 0.01, MARGIN = 2L))
    expect_equal(specs$zero$call_args, list())

    expect_true(all(c("MinDet", "zero") %in% available_imputation_methods()))
    expect_type(assert_imputation_method_available("zero"), "list")
    expect_error(
        assert_imputation_method_available("unknown"),
        "Unknown imputation method"
    )
})

test_that("pca_plotly handles no color and many categorical levels", {
    pca_result <- nipals::nipals(
        matrix(seq_len(48), nrow = 12),
        center = TRUE
    )
    pca_df <- data.frame(
        PC1 = seq_len(12),
        PC2 = rev(seq_len(12)),
        row.names = paste0("row", seq_len(12))
    )

    no_color_plot <- pca_plotly(
        pca_df,
        pca_result = pca_result,
        color_name = "NULL",
        show_legend = TRUE,
        x_component = "PC1",
        y_component = "PC2"
    )
    expect_s3_class(no_color_plot, "plotly")
    expect_null(no_color_plot$x$attrs[[1]]$customdata)
    expect_false(grepl("customdata", no_color_plot$x$attrs[[1]]$hovertemplate))
    expect_error(plotly::plotly_build(no_color_plot), NA)

    pca_df$.qfeaturesgui_row_id <- rownames(pca_df)
    pca_df$group <- paste0("group", seq_len(12))
    many_groups_plot <- pca_plotly(
        pca_df,
        pca_result = pca_result,
        color_name = "group",
        show_legend = TRUE,
        x_component = "PC1",
        y_component = "PC2"
    )
    expect_s3_class(many_groups_plot, "plotly")
    expect_error(plotly::plotly_build(many_groups_plot), NA)
})
