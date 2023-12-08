#' Load an aggregation map
#'
#' An aggregation file is an Excel file that
#' contains an `aggregation_tab` with
#' `many_colname` and `few_colname`,
#' by default "Many" and "Few".
#' This function loads the aggregation table
#' and converts to an aggregation map
#' using `matsbyname::agg_table_to_agg_map()`.
#'
#' @param aggregation_file The path to the industry aggregations file.
#' @param aggregation_tab The tab in `industry_aggregations_file` that contains
#'                        an industry aggregation table.
#'                        Default is `aggregation_file |> basename() |> tools::file_path_sans_ext()`
#' @param country_colname The name of the country column in `aggregation_tab`.
#'                        Default is "Country".
#' @param year_colname The name of the year column in `aggregation_tab`.
#'                     Default is "Year".
#' @param many_colname The name of the many column on the `industry_aggregations_tab`.
#'                     Default is "Many".
#' @param few_colname The name of the few column on the `industry_aggregations_tab`.
#'                    Default is "Few".
#'
#' @return An aggregation map.
#'
#' @export
load_agg_map <- function(aggregation_file,
                         aggregation_tab = aggregation_file |>
                           basename() |>
                           tools::file_path_sans_ext(),
                         country_colname = "Country",
                         year_colname = "Year",
                         many_colname = "Many",
                         few_colname = "Few") {

  # Load the file to get an aggregation table
  agg_table <- aggregation_file |>
    readxl::read_excel(sheet = aggregation_tab)

  # Create the aggregation maps by Country and Year
  agg_table |>
    # Create a nested data frame where the aggregation tables
    # are now per Country and per Year.
    tidyr::nest(.by = c(country_colname, year_colname), .key = "agg_table") |>
    dplyr::mutate(
      agg_map = lapply(X = .data[["agg_table"]],
                       FUN = matsbyname::agg_table_to_agg_map,
                       many_colname = many_colname,
                       few_colname = few_colname)
    ) |>
    dplyr::mutate(
      # Delete the agg_table column, because it is no longer needed.
      agg_table = NULL
    )
}



#' Targeted product and industry aggregation according to an aggregation map
#'
#' `matsbyname::aggregate_byname()` performs aggregation of
#' rows and columns but is unaware of countries and years.
#' This function performs targeted aggregation
#' for some or all countries and years
#' by including a `country_colname` and a `year_colname`
#' in the `aggregation_map`.
#' In addition,
#' the Country and Year columns
#' may contain optional "All" strings
#' to indicate all countries or years should be aggregated
#' as specified.
#'
#' @param psut_df A data frame of PSUT matrices with `country_colname`, `year_colname`,
#'                and columns of PSUT matrices (`matcols`).
#' @param aggregation_map An aggregation map with `country_colname`, `year_colname`, and `agg_map_colname`
#'                        in which `agg_map_colname` contains aggregation maps
#'                        to be applied for the country and year specified.
#' @param rowcoltype The row or column types to be aggregated.
#'                   Default is `c("Product", "Industry")`.
#'
#' @return A version of `psut_df` with aggregations according to `aggregation_map` and
#'         `rowcoltype`.
#'
#' @export
targeted_aggregation <- function(psut_df,
                                 aggregation_map,
                                 country_colname = "Country",
                                 year_colname = "Year",
                                 agg_map_colname = "agg_map",
                                 margin = c("Industry", "Product"),
                                 matcols = c("R", "U", "U_feed", "U_EIOU", "r_EIOU", "V", "Y", "S_units"),
                                 matnames_colname = "matnames",
                                 matvals_colname = "matvals") {
  psut_df_long <- psut_df |>
    # Change shape of data frame by matcols to put all matrices in one column
    tidyr::pivot_longer(cols = tidyr::any_of(matcols), names_to = "matnames", values_to = "matvals")
  out <- psut_df_long

  # Look at rows in aggregation_map where both Country and Year are "all" (case-insensitive)
  agg_map_all_all <- aggregation_map |>
    dplyr::filter(tolower(.data[[country_colname]]) == "all" &
                    tolower(.data[[year_colname]]) == "all") |>
    # The result will be list frame with 0 or 1 rows.
    # Extract the piece we need.
    magrittr::extract2(agg_map_colname)
  if (length(agg_map_all_all) > 0) {
    # Perform an aggregation for agg_map_all_all
    out <- out |>
      dplyr::mutate(
        "{agg_map_colname}" := agg_map_all_all,
        "{matvals_colname}" := matsbyname::aggregate_byname(a = .data[[matvals_colname]],
                                                            aggregation_map = .data[[agg_map_colname]],
                                                            margin = margin),
        "{agg_map_colname}" := NULL
      )
  }

  # Look at rows in aggregation_map where Country is "all" and Year is not
  agg_map_country_all <- aggregation_map |>
    dplyr::filter(tolower(.data[[country_colname]]) == "all" &
                    tolower(.data[[year_colname]]) != "all") |>
    dplyr::mutate(
      "{country_colname}" := NULL,
      "{year_colname}" := as.numeric(.data[[year_colname]])
    )
  if (length(agg_map_country_all) > 0) {
    # Do the aggregation
    out <- out |>
      dplyr::left_join(agg_map_country_all, by = year_colname) |>
      dplyr::mutate(
        "{matvals_colname}" := matsbyname::aggregate_byname(a = .data[[matvals_colname]],
                                                            aggregation_map = .data[[agg_map_colname]],
                                                            margin = margin),
        "{agg_map_colname}" := NULL
      )
  }

  # Look at rows in aggregation_map where Year is "all" and Country is not
  agg_map_year_all <- aggregation_map |>
    dplyr::filter(tolower(.data[[country_colname]]) != "all" &
                    tolower(.data[[year_colname]]) == "all") |>
    dplyr::mutate(
      "{year_colname}" := NULL
    )
  if (length(agg_map_year_all) > 0) {
    out <- out |>
      dplyr::left_join(agg_map_year_all, by = country_colname) |>
      dplyr::mutate(
        "{matvals_colname}" := matsbyname::aggregate_byname(a = .data[[matvals_colname]],
                                                            aggregation_map = .data[[agg_map_colname]],
                                                            margin = margin),
        "{agg_map_colname}" := NULL
      )
  }

  # Look at rows in aggregation_map where neither Country nor Year is "all"
  agg_map_country_year <- aggregation_map |>
    dplyr::filter(tolower(.data[[country_colname]]) != "all" &
                    tolower(.data[[year_colname]]) != "all") |>
    dplyr::mutate(
      "{year_colname}" := as.numeric(.data[[year_colname]])
    )
  if (length(agg_map_country_year) > 0) {
    out <- out |>
      dplyr::left_join(agg_map_country_year, by = c(country_colname, year_colname)) |>
      dplyr::mutate(
        "{matvals_colname}" := matsbyname::aggregate_byname(a = .data[[matvals_colname]],
                                                            aggregation_map = .data[[agg_map_colname]],
                                                            margin = margin),
        "{agg_map_colname}" := NULL
      )
  }

  out |>
    tidyr::pivot_wider(names_from = matnames_colname,
                       values_from = matvals_colname)
}








