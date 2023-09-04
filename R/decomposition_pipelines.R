#' Create a targets workflow for decomposition work
#'
#' This is a target factory whose arguments
#' specify the details of a targets workflow to be constructed
#'
#' @param countries A string vector of 3-letter country codes.
#'                  Default is "all", meaning all available countries should be analyzed.
#' @param years A numeric vector of years to be analyzed.
#'              Default is "all", meaning all available years should be analyzed.
#' @param psut_release The release we'll use from `pipeline_releases_folder`.
#'                     See details.
#' @param pipeline_releases_folder The path to a folder where releases of output targets are pinned.
#' @param pipeline_caches_folder The path to a folder where releases of pipeline caches are stored.
#' @param release Boolean that tells whether to do a release of the results.
#'                Default is `FALSE`.
#'
#' @return A list of `tar_target`s to be executed in a workflow.
#'
#' @export
get_pipeline <- function(countries = "all",
                         years = "all",
                         psut_release,
                         pipeline_releases_folder,
                         pipeline_caches_folder,
                         release = FALSE) {

  # Avoid warnings for some target names
  Country <- NULL
  Year <- NULL
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
    targets::tar_target_raw("PinboardFolder", pipeline_releases_folder),
    targets::tar_target_raw("Release", release),

    # Set the pin and release as targets
    targets::tar_target_raw(
      "PSUTRelease",
      unname(psut_release)
    ),


    # PSUT ---------------------------------------------------------------------

    # Pull in the PSUT data frame
    targets::tar_target_raw(
      "PSUT",
      quote(pins::board_folder(PinboardFolder, versioned = TRUE) |>
              pins::pin_read("psut", version = PSUTRelease) |>
              PFUPipelineTools::filter_countries_years(countries = Countries, years = Years))
    ),
    tarchetypes::tar_group_by(
      name = "PSUTbyYear",
      command = PSUT,
      Year
    ),







    # Zip the cache and store in the pipeline_caches_folder --------------------

    targets::tar_target_raw(
      "StoreCache",
      quote(PFUPipelineTools::stash_cache(pipeline_caches_folder = PipelineCachesFolder,
                                          cache_folder = "_targets",
                                          file_prefix = "pfu_agg_pipeline_cache_",
                                          dependency = c(ReleasePSUT_Re_all_Chop_all_Ds_all_Gr_all, # The RUVY matrices for ECCs
                                                         ReleaseSectorAggEtaFU,                     # Product C
                                                         ReleaseSectorAggEtaFUCSV,                  # Product D
                                                         ReleaseAggEtaPFU,                          # Product E
                                                         ReleaseAggEtaPFUCSV,                       # Product F
                                                         ReleaseSectorAggEtaFUWorld,                # Product G
                                                         ReleaseAggEtaPFUWorld),                    # Product H
                                          release = Release))
    )
  )
}


