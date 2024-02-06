#' Calculates industry energy shares for IEA data
#'
#' Calculates both for input (from the **U** matrix)
#' and for outputs (from the **V** matrix).
#'
#' @param psut_df A data frame of PSUT matrices
#' @param U_matname The name of the **U** matrix in `psut_df`.
#'                  Default is "U".
#' @param V_matname The name of the **V** matrix in `psut_df`.
#'                  Default is "V".
#' @param input_fractions_column The name of the output column that contains vectors of input fractions.
#' @param output_fractions_column The name of the output column that contains vectors of output fractions.
#'
#' @return A version of `psut_df` with input and output industry shares added as column vectors at the right side of the data frame.
#'
#' @export
calc_iea_tech_shares <- function(psut_df,
                                 U_matname = "U",
                                 V_matname = "V",
                                 input_fractions_column = "U_fractions",
                                 output_fractions_column = "V_fractions") {

  rowcolsums <- psut_df |>
    dplyr::filter(Last.stage == "Final", IEAMW == "IEA") |>
    dplyr::mutate(
      U_colsums = .data[[U_matname]] |>
        matsbyname::colsums_byname(),
      V_rowsums = .data[[V_matname]] |>
        matsbyname::rowsums_byname(),
      U_sum = matsbyname::sumall_byname(U_colsums),
      V_sum = matsbyname::sumall_byname(V_rowsums),
      "{input_fractions_column}" := U_colsums |>
        matsbyname::transpose_byname() |>
        matsbyname::quotient_byname(U_sum),
      "{output_fractions_column}" := V_rowsums |>
        matsbyname::quotient_byname(V_sum),
      # Get rid of columns we don't want on output.
      U_colsums = NULL,
      V_rowsums = NULL,
      U_sum = NULL,
      V_sum = NULL
    )
}
