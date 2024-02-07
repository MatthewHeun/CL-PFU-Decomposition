#' Create a targets workflow for decomposition work
#'
#' This is a target factory whose arguments
#' specify the details of a targets workflow to be constructed.
#'
#' @param countries A string vector of 3-letter country codes.
#'                  Default is "all", meaning all available countries should be analyzed.
#' @param years A numeric vector of years to be analyzed.
#'              Default is "all", meaning all available years should be analyzed.
#' @param database_version The version of the database we'll use.
#'                         See details.
#' @param targeted_aggregations_file The Excel file that describes industry aggregations
#'                                   (in an "industry_aggregations" tab) and product aggregations
#'                                   (in a "product_aggregations" tab).
#'                                   Both tabs should contain
#'                                   a Many column, a Few column,
#'                                   a Country column, and a Year column.
#'                                   The string "All" in Country or Year
#'                                   designates that all countries or years should be aggregated.
#' @param pipeline_releases_folder The path to a folder where releases of output targets are pinned.
#' @param pipeline_caches_folder The path to a folder where releases of pipeline caches are stored.
#' @param reports_dest_folder The destination folder for reports.
#' @param release Boolean that tells whether to do a release of the results.
#'                Default is `FALSE`.
#'
#' @return A list of `tar_target`s to be executed in a workflow.
#'
#' @export
get_pipeline <- function(countries = "all",
                         years = "all",
                         database_version,
                         targeted_aggregations_file,
                         pipeline_releases_folder,
                         pipeline_caches_folder,
                         reports_dest_folder,
                         release = FALSE) {

  # Avoid warnings for some target names
  Country <- NULL
  Year <- NULL
  Etai <- NULL
  PSUT <- NULL
  PSUT_Re_all <- NULL
  PSUT_Re_all_Chop_all_Ds_all_Gr_all <- NULL
  PSUT_Re_World <- NULL

  list(

    # Preliminary setup --------------------------------------------------------

    # Store some incoming data as targets
    # These targets are invariant across incoming psut_releases
    targets::tar_target_raw("Countries", list(countries)),
    targets::tar_target_raw("Years", list(years)),
    targets::tar_target_raw("DatabaseVersion",database_version),
    targets::tar_target_raw("PinboardFolder", pipeline_releases_folder),
    targets::tar_target_raw("PipelineCachesFolder", pipeline_caches_folder),
    targets::tar_target_raw("ReportsDestFolder", reports_dest_folder),
    targets::tar_target_raw("Release", release),

    # PSUT ---------------------------------------------------------------------

    # Pull in the PSUT data frame
    targets::tar_target_raw(
      "PSUT",
      quote(PFUPipelineTools::read_pin_version(pin_name = "psut",
                                               database_version = database_version,
                                               pipeline_releases_folder = PinboardFolder) |>
              PFUPipelineTools::filter_countries_years(countries = Countries, years = Years))
    ),
    tarchetypes::tar_group_by(
      name = "PSUTByCountry",
      command = PSUT,
      Country
    ),

    # # Etai ---------------------------------------------------------------------
    #
    # # Pull in the Etai data frame
    # targets::tar_target_raw(
    #   "Etai",
    #   quote(PFUPipelineTools::read_pin_version(pin_name = "eta_i",
    #                                            database_version = database_version,
    #                                            pipeline_releases_folder = PinboardFolder) |>
    #           PFUPipelineTools::filter_countries_years(countries = Countries, years = Years))
    # ),
    #
    #
    # # Aggregation file ---------------------------------------------------------
    #
    # targets::tar_target_raw(
    #   "TargetedAggregationsFile",
    #   targeted_aggregations_file
    # ),
    #
    #
    # # Industry aggregations ----------------------------------------------------
    #
    # targets::tar_target_raw(
    #   "IndustryAggregationMaps",
    #   quote(load_agg_map(TargetedAggregationsFile, aggregation_tab = "industry_aggregations"))
    # ),
    # targets::tar_target_raw(
    #   name = "PSUT_Agg_In",
    #   command = quote(targeted_aggregation(psut_df = PSUTByCountry,
    #                                        aggregation_map = IndustryAggregationMaps,
    #                                        margin = "Industry")),
    #   pattern = quote(map(PSUTByCountry))
    # ),
    #
    #
    # # Product aggregations -----------------------------------------------------
    #
    # targets::tar_target_raw(
    #   "ProductAggregationMaps",
    #   quote(load_agg_map(TargetedAggregationsFile, aggregation_tab = "product_aggregations"))
    # ),
    # targets::tar_target_raw(
    #   name = "PSUT_Agg_InPr",
    #   command = quote(targeted_aggregation(psut_df = PSUT_Agg_In,
    #                                        aggregation_map = ProductAggregationMaps,
    #                                        margin = "Product")),
    #   pattern = quote(map(PSUT_Agg_In))
    # ),


    # Technology shares --------------------------------------------------------

    targets::tar_target_raw(
      "IndustryShares",
      quote(calc_iea_industry_shares(psut_df = PSUTByCountry)),
      pattern = quote(map(PSUTByCountry))
    ),
    targets::tar_target_raw(
      "IndustrySharesExpanded",
      quote(expand_tech_shares(IndustryShares)),
      pattern = quote(map(IndustryShares))
    )




  )
}


