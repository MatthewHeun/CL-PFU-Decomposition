#' Create a targets workflow for decomposition work
#'
#' This is a target factory whose arguments
#' specify the details of a targets workflow to be constructed.
#'
#' @param countries A string vector of 3-letter country codes.
#'                  Default is "all", meaning all available countries should be analyzed.
#' @param years A numeric vector of years to be analyzed.
#'              Default is "all", meaning all available years should be analyzed.
#' @param eta_i_tables_release The release we'll use from `pipeline_releases_folder`.
#'                             See details.
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
                         eta_i_release,
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

    # Set the pin and release as targets
    targets::tar_target_raw(
      "EtaiRelease",
      unname(eta_i_release)
    ),


    # Etai ---------------------------------------------------------------------

    # Pull in the PSUT data frame
    targets::tar_target_raw(
      "Etai",
      quote(pins::board_folder(PinboardFolder, versioned = TRUE) |>
              pins::pin_read("eta_i", version = EtaiRelease) |>
              PFUPipelineTools::filter_countries_years(countries = Countries, years = Years))
    ),


    # Create an efficiencies report --------------------------------------------

    targets::tar_target_raw(
      "IEAEtaiReports",
      quote(Etai |>
              create_iea_eta_i_reports(reports_dest_folder = ReportsDestFolder))
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


