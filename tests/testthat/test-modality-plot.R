test_that("modality plots retain feature links through unselected assays", {
    qf <- make_test_qfeatures()
    qf <- suppressMessages(QFeatures::aggregateFeatures(
        qf,
        i = "set1",
        fcol = "protein",
        name = "peptides",
        fun = colMeans
    ))
    SummarizedExperiment::rowData(qf[["peptides"]])$group <-
        c("G1", "G2", "G2")
    qf <- suppressMessages(QFeatures::aggregateFeatures(
        qf,
        i = "peptides",
        fcol = "group",
        name = "proteins",
        fun = colMeans
    ))

    suppressWarnings(suppressMessages(shiny::testServer(
        server_module_modality_plot,
        args = list(assays_to_process = shiny::reactive(qf)),
        {
            session$setInputs(
                selected_assay = c("set1", "proteins"),
                reference_modality = "proteins",
                featnames = "G1"
            )

            data <- modality_data()
            expect_setequal(as.character(data$assay), c("set1", "proteins"))
            expect_equal(levels(data$assay), c("set1", "proteins"))
            expect_setequal(data$rowname[data$assay == "set1"], c("f1", "f2"))
            expect_setequal(data$rowname[data$assay == "proteins"], "G1")
            expect_equal(sort(data$value[data$assay == "set1"]), as.numeric(1:6))
            expect_no_error(output$modality_plot)
        }
    )))
})
