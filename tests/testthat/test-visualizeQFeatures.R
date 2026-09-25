test_that("visualizeQFeatures accepts omitted, object, and RDS inputs", {
    qf <- make_test_qfeatures()
    path <- tempfile(fileext = ".rds")
    saveRDS(qf, path)

    expect_s3_class(visualizeQFeatures(), "shiny.appobj")
    expect_s3_class(visualizeQFeatures(NULL), "shiny.appobj")
    expect_s3_class(visualizeQFeatures(qf), "shiny.appobj")
    expect_s3_class(visualizeQFeatures(path), "shiny.appobj")
    expect_error(visualizeQFeatures(data.frame(x = 1)), "must be a QFeatures object")
    expect_error(visualizeQFeatures(tempfile()), "does not exist")
})

test_that("visualization updates after an upload and hides the startup button", {
    qf <- make_test_qfeatures()
    path <- tempfile(fileext = ".rds")
    saveRDS(qf, path)

    shiny::testServer(build_visualize_server(NULL, has_qfeatures = FALSE), {
        session$flushReact()
        expect_null(current_qfeatures())
        expect_match(output$startup_upload_ui$html, "startup_show_upload")

        session$setInputs(startup_qfeatures_rds = data.frame(datapath = path))
        session$flushReact()
        expect_null(current_qfeatures())
        session$setInputs(startup_load_qfeatures = 1)
        expect_qfeatures_equal(current_qfeatures(), qf)
        expect_null(output$startup_upload_ui)
        expect_match(output[["visualize-qfeatures_plot"]], "set1")
        expect_match(output[["visualize-qfeatures_plot"]], "set2")
    })
})

test_that("visualization loads the bundled demo and retains existing assay names", {
    demo <- demo_qfeatures()

    shiny::testServer(build_visualize_server(NULL, has_qfeatures = FALSE), {
        session$flushReact()
        session$setInputs(startup_use_demo_qfeatures = 1)
        session$flushReact()
        expect_qfeatures_equal(current_qfeatures(), demo)
        expect_null(output$startup_upload_ui)
    })

    shiny::testServer(build_visualize_server(demo, has_qfeatures = TRUE), {
        session$flushReact()
        expect_qfeatures_equal(current_qfeatures(), demo)
        expect_null(output$startup_upload_ui)
    })
})
