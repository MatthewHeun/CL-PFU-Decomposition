#' Calculates industry energy shares for IEA data
#'
#' Calculates both for input (from the **U** matrix)
#' and for outputs (from the **V** matrix).
#' All incoming PSUT matrix columns in `psut_df` are dropped.
#' The output is two coumns of column vectors.
#'
#' @param psut_df A data frame of PSUT matrices
#' @param trim_cols A boolean that tells whether to eliminate matrix columns on output.
#'                  Default is `TRUE`.
#' @param U The name of the **U** matrix in `psut_df`.
#'          This column is deleted on output.
#'          Default is "U".
#' @param V The name of the **V** matrix in `psut_df`.
#'          This column is deleted on output.
#'          Default is "V".
#' @param input_fractions_column The name of the output column that contains vectors of input fractions.
#' @param output_fractions_column The name of the output column that contains vectors of output fractions.
#' @param R,U_feed,U_eiou,r_eiou,Y,S_units,Y_fu_details,U_eiou_fu_details Columns deleted on output.
#'
#' @return A version of `psut_df` with input and output industry shares added as column vectors at the right side of the data frame.
#'
#' @export
calc_iea_industry_shares <- function(psut_df,
                                     trim_cols = TRUE,
                                     R = Recca::psut_cols$R,
                                     U = Recca::psut_cols$U,
                                     U_feed = Recca::psut_cols$U_feed,
                                     U_eiou = Recca::psut_cols$U_eiou,
                                     r_eiou = Recca::psut_cols$r_eiou,
                                     V = Recca::psut_cols$V,
                                     Y = Recca::psut_cols$Y,
                                     S_units = Recca::psut_cols$S_units,
                                     Y_fu_details = Recca::psut_cols$Y_fu_details,
                                     U_eiou_fu_details = Recca::psut_cols$U_eiou_fu_details,
                                     input_fractions_column = "U_fractions",
                                     output_fractions_column = "V_fractions") {

  industry_share_func <- function(U_mat, V_mat) {
    U_colsums <- matsbyname::colsums_byname(U_mat)
    V_rowsums <- matsbyname::rowsums_byname(V_mat)
    U_sum <- matsbyname::sumall_byname(U_colsums)
    V_sum <- matsbyname::sumall_byname(V_rowsums)
    input_fracs <- U_colsums |>
      matsbyname::transpose_byname() |>
      matsbyname::quotient_byname(U_sum) |>
      matsbyname::replaceNaN_byname()
    output_fracs <- V_rowsums |>
      matsbyname::quotient_byname(V_sum) |>
      matsbyname::replaceNaN_byname()

    list(input_fracs, output_fracs) |>
      magrittr::set_names(c(input_fractions_column, output_fractions_column))
  }

  out <- matsindf::matsindf_apply(psut_df, FUN = industry_share_func, U_mat = U, V_mat = V)
  if (is.data.frame(psut_df) & trim_cols) {
    out <- out |>
      dplyr::mutate(
        "{R}" := NULL,
        "{U}" := NULL,
        "{U_feed}" := NULL,
        "{U_eiou}" := NULL,
        "{r_eiou}" := NULL,
        "{V}" := NULL,
        "{Y}" := NULL,
        "{S_units}" := NULL,
        "{Y_fu_details}" := NULL,
        "{U_eiou_fu_details}" := NULL
      )
  }
  return(out)
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
    tidyr::pivot_wider(names_from = "matnames", values_from = "fractions")
}
