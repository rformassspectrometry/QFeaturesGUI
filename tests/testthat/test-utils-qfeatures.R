test_that("check_qfeatures validates objects and RDS paths", {
    qf <- make_test_qfeatures()

    expect_s4_class(check_qfeatures(qf), "QFeatures")

    path <- tempfile(fileext = ".rds")
    saveRDS(qf, path)
    expect_s4_class(check_qfeatures(path), "QFeatures")

    expect_error(check_qfeatures(), "argument is missing")
    expect_error(
        check_qfeatures(tempfile(fileext = ".rds")),
        "does not exist"
    )
    expect_error(
        check_qfeatures(c("first.rds", "second.rds")),
        "single path"
    )
    expect_error(
        check_qfeatures(data.frame(x = 1)),
        "must be a QFeatures object"
    )

    bad_path <- tempfile(fileext = ".rds")
    saveRDS(data.frame(x = 1), bad_path)
    expect_error(
        check_qfeatures(bad_path),
        "RDS file does not contain a QFeatures object"
    )
})

test_that("demo_qfeatures builds the bundled zero-to-NA demo object", {
    data("inputTable", package = "QFeaturesGUI")
    data("sampleTable", package = "QFeaturesGUI")

    expected <- QFeatures::readQFeatures(
        assayData = inputTable,
        colData = sampleTable,
        runCol = "Raw.file",
        quantCols = NULL,
        removeEmptyCols = TRUE,
        verbose = FALSE
    )
    expected <- QFeatures::zeroIsNA(expected, i = seq_along(expected))

    object <- demo_qfeatures()

    expect_qfeatures_equal(object, expected)
})
