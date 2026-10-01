#' import tab (section) server builder
#'
#' @param input input parameter that should be given
#'  by the higher level server builder
#' @param output output parameter that should be given
#'  by the higher level server builder
#' @param session session parameter that should be given
#'  by the higher level server builder
#' @param sample_table a dataframe that contains the sample table given by the user
#' @param input_table a dataframe that contains the input table given by the user
#'
#' @return A shiny server function that contains the import tab server logic
#' @rdname INTERNAL_server_import_tab
#' @keywords internal
#'
server_import_tab <- function(
      input,
      output,
      session,
      sample_table,
      input_table
) {
    imported_input <- box_read_table_server(
        id = "input",
        given_table = input_table
    )
    imported_sample <- box_read_table_server(
        id = "sample",
        given_table = sample_table
    )
    box_readqfeatures_server(
        "readqfeatures",
        imported_input,
        imported_sample
    )
}
