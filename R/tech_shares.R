#' Calculates industry energy shares for IEA data
#'
#' Calculates both for input (from the **U** matrix)
#' and for outputs (from the **V** matrix).
#' All incoming PSUT matrix columns in `psut_df` are dropped.
#' The output is two coumns of column vectors.
#'
#' @param psut_df A data frame of PSUT matrices
#' @param U_matname The name of the **U** matrix in `psut_df`.
#'                  This column is deleted on output.
#'                  Default is "U".
#' @param V_matname The name of the **V** matrix in `psut_df`.
#'                  This column is deleted on output.
#'                  Default is "V".
#' @param input_fractions_column The name of the output column that contains vectors of input fractions.
#' @param output_fractions_column The name of the output column that contains vectors of output fractions.
#' @param R_matname,U_feed_matname,U_eiou_matname,r_eiou_matname,V_matname,Y_matname,S_units_matname,Y_fu_details_matname,U_eiou_matname_matname Columns deleted on output.
#'
#' @return A version of `psut_df` with input and output industry shares added as column vectors at the right side of the data frame.
#'
#' @export
calc_iea_industry_shares <- function(psut_df,
                                     R_matname = Recca::psut_cols$R,
                                     U_matname = Recca::psut_cols$U,
                                     U_feed_matname = Recca::psut_cols$U_feed,
                                     U_eiou_matname = Recca::psut_cols$U_eiou,
                                     r_eiou_matname = Recca::psut_cols$r_eiou,
                                     V_matname = Recca::psut_cols$V,
                                     Y_matname = Recca::psut_cols$Y,
                                     S_units_matname = Recca::psut_cols$S_units,
                                     Y_fu_details_matname = Recca::psut_cols$Y_fu_details,
                                     U_eiou_fu_details_matname = Recca::psut_cols$U_eiou_fu_details,
                                     input_fractions_column = "U_fractions",
                                     output_fractions_column = "V_fractions") {

  psut_df |>
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
      V_sum = NULL,
      "{R_matname}" := NULL,
      "{U_matname}" := NULL,
      "{U_feed_matname}" := NULL,
      "{U_eiou_matname}" := NULL,
      "{r_eiou_matname}" := NULL,
      "{V_matname}" := NULL,
      "{Y_matname}" := NULL,
      "{S_units_matname}" := NULL,
      "{Y_fu_details_matname}" := NULL,
      "{U_eiou_fu_details_matname}" := NULL
    )
}


#' Expand a data frame of industry shares
#'
#' @param tech_shares_vectors A data frame produced by `calc_iea_industry_shares()`.
#' @param input_fractions_column The name of the output column that contains vectors of input fractions.
#' @param output_fractions_column The name of the output column that contains vectors of output fractions.
#'
#' @return A data frame with `tech_shares_vectors` expanded to tidy format.
#'
#' @export
expand_tech_shares <- function(tech_shares_vectors,
                               input_fractions_column = "U_fractions",
                               output_fractions_column = "V_fractions") {
  tech_shares_vectors |>
    tidyr::pivot_longer(cols = dplyr::all_of(c(input_fractions_column, output_fractions_column)),
                        names_to = "matnames",
                        values_to = "fractions") |>
    matsindf::expand_to_tidy(matnames = "matnames", matvals = "fractions",
                             rownames = "Industry", colnames = "colnames", drop = 0) |>
    dplyr::mutate(
      colnames = NULL,
      rowtypes = NULL,
      coltypes = NULL
    ) |>
    tidyr::pivot_wider(names_from = "matnames", values_from = "fractions")}
