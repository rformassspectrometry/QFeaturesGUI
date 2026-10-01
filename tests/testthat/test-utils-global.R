test_that("global name helpers strip GUI suffixes and format strings", {
    expect_equal(
        remove_QFeaturesGUI(c("set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#12)")),
        c("set1", "set2")
    )
    expect_equal(upper_first("protein"), "Protein")
    expect_equal(upper_first("protein group"), "Protein group")
})

test_that("page_assays_subset selects assays by fixed name pattern", {
    qf <- make_test_qfeatures()
    names(qf) <- c("set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#1)")

    initial <- page_assays_subset(qf, "_(QFeaturesGUI#0)")
    step1 <- page_assays_subset(qf, "_(QFeaturesGUI#1)")
    missing <- page_assays_subset(qf, "_(QFeaturesGUI#9)")

    expect_equal(names(initial), "set1_(QFeaturesGUI#0)")
    expect_equal(names(step1), "set2_(QFeaturesGUI#1)")
    expect_length(missing, 0)
})

test_that("qfeatures_to_df summarises dimensions and strips GUI suffixes", {
    qf <- make_test_qfeatures()
    names(qf) <- c("set1_(QFeaturesGUI#0)", "set2_(QFeaturesGUI#0)")

    summary <- qfeatures_to_df(qf, assay_labels = remove_QFeaturesGUI)

    expect_equal(summary$Name, c("set1", "set2"))
    expect_equal(summary$nFeatures, c(4, 2))
    expect_equal(summary$nSamples, c(3, 3))
    expect_equal(summary$nFeaturesMetadata, c(3, 3))
    expect_equal(summary$nSamplesMetadata, c(3, 3))
})
