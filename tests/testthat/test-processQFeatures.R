test_that("processQFeatures can be constructed without a QFeatures object", {
    app <- processQFeatures()

    expect_s3_class(app, "shiny.appobj")
})

test_that("process startup initializes the selected assays and workflow", {
    qf <- make_test_qfeatures()
    path <- tempfile(fileext = ".rds")
    saveRDS(qf, path)

    shiny::testServer(
        build_process_server(NULL, integer(), character(), has_qfeatures = FALSE),
        {
            session$flushReact()
            expect_null(.qf$qfeatures)
            session$setInputs(startup_qfeatures_rds = data.frame(datapath = path))
            session$flushReact()
            session$setInputs(
                startup_initial_sets = "set2",
                startup_load_qfeatures = 1
            )
            expect_identical(names(.qf$qfeatures), c("set1", "set2_(QFeaturesGUI#0)"))
            expect_qfeatures_equal(summary_qfeatures(), .qf$qfeatures)
            expect_identical(global_rv$workflow_config, character())
        }
    )
})

