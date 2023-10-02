#' Create a targets workflow for decomposition work
#'
#' This is a target factory whose arguments
#' specify the details of a targets workflow to be constructed.
#'
#' @param countries A string vector of 3-letter country codes.
#'                  Default is "all", meaning all available countries should be analyzed.
#' @param years A numeric vector of years to be analyzed.
#'              Default is "all", meaning all available years should be analyzed.
#' @param psut_release The release we'll use from `pipeline_releases_folder`.
#'                     See details.
#' @param eta_i_release The release we'll use from `pipeline_releases_folder`.
#'                      See details.
#' @param industry_aggregations_file The Excel file that describes industry aggregations.
#'                                   It should have a tab named "industry_aggregations" that contains
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
                         psut_release,
                         eta_i_release,
                         industry_aggregations_file,
                         pipeline_releases_folder,
                         pipeline_caches_folder,
                         reports_dest_folder,
                         release = FALSE) {

  # Avoid warnings for some target names
  Country <- NULL
  Year <- NULL
  Etai <- NULL
  PSUT_Re_all <- NULL
  PSUT_Re_all_Chop_all_Ds_all_Gr_all <- NULL
  PSUT_Re_World <- NULL

  list(

    # Preliminary setup --------------------------------------------------------

    # Store some incoming data as targets
    # These targets are invariant across incoming psut_releases
    targets::tar_target_raw("Countries", list(countries)),
    targets::tar_target_raw("Years", list(years)),
    targets::tar_target_raw("PinboardFolder", pipeline_releases_folder),
    targets::tar_target_raw("PipelineCachesFolder", pipeline_caches_folder),
    targets::tar_target_raw("ReportsDestFolder", reports_dest_folder),
    targets::tar_target_raw("Release", release),

    # PSUT ---------------------------------------------------------------------

    targets::tar_target_raw(
      name = "PSUTRelease",
      command = unname(psut_release)
    ),
    # Pull in the PSUT data frame
    targets::tar_target_raw(
      name = "PSUT",
      command = quote(pins::board_folder(PinboardFolder, versioned = TRUE) |>
                        pins::pin_read("psut", version = PSUTRelease) |>
                        PFUPipelineTools::filter_countries_years(countries = Countries, years = Years))
    ),
    tarchetypes::tar_group_by(
      name = "PSUTByCountry",
      command = PSUT,
      Country
    ),

    # Etai ---------------------------------------------------------------------

    targets::tar_target_raw(
      "EtaiRelease",
      unname(eta_i_release)
    ),
    # Pull in the Etai data frame
    targets::tar_target_raw(
      "Etai",
      quote(pins::board_folder(PinboardFolder, versioned = TRUE) |>
              pins::pin_read("eta_i", version = EtaiRelease) |>
              PFUPipelineTools::filter_countries_years(countries = Countries, years = Years))
    ),


    # Industry aggregations -------------------------------------------------------------

    targets::tar_target_raw(
      "IndustryAggregationsFile",
      industry_aggregations_file
    ),
    targets::tar_target_raw(
      "IndustryAggregationMaps",
      quote(load_agg_map(IndustryAggregationsFile))
    ),
    targets::tar_target_raw(
      name = "PSUT_Agg_In",
      command = quote(psut_aggregation(PSUTByCountry,
                                       IndustryAggregationMaps,
                                       margin = "Industry")),
      pattern = quote(map(PSUTByCountry))
    ),


    # Create an efficiencies report --------------------------------------------

    targets::tar_target_raw(
      "IEAEtaiReports",
      quote(Etai |>
              create_iea_eta_i_reports(reports_dest_folder = ReportsDestFolder, release = Release))
    ),


    # Zip the cache and store in the pipeline_caches_folder --------------------

    targets::tar_target_raw(
      "StoreCache",
      quote(PFUPipelineTools::stash_cache(pipeline_caches_folder = PipelineCachesFolder,
                                          cache_folder = "_targets",
                                          file_prefix = "pfu_decomposition_pipeline_cache",
                                          dependency = c(Etai),
                                          release = Release))
    )
  )
}


