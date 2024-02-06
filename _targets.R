
library(targets)
# targets::tar_make() to run the pipeline in a single thread.
# targets::tar_make_future(workers = 8) to execute across multiple cores.
# targets::tar_make(callr_function = NULL) to debug.
# targets::tar_read(<<target_name>>) to view the results.
# targets::tar_invalidate(<<target_name>>) to re-compute <<target_name>> and its dependents.
# targets::tar_destroy() to start over with everything,








# Set control parameters for the pipeline.

# Set the countries to be analyzed.
# countries <- PFUPipelineTools::canonical_countries |> as.character()
countries <- c("USA", "ITA")
# countries <- c("GBR", "USA", "MEX")
# countries <- c("ZWE", "USA", "WRLD")
# countries <- "USA"
# countries <- "WRLD"
# countries <- "CHNM"
# countries <- "GHA"
# countries <- "all" # Run all countries in the PSUT target.
# countries <- c(PFUPipelineTools::canonical_countries, "WRLD") |> as.character()
# Countries with unique allocation data plus BEL and TUR (for Pierre).
# countries <- c("BRA", "CAN", "CHNM", "DEU", "DNK", "ESP", "FRA", "GBR", "GHA", "GRC",
#                "HKG", "HND", "IDN", "IND", "JOR", "JPN", "KOR", "MEX", "NOR", "PRT",
#                "RUS", "USA", "WABK", "WMBK", "ZAF", "BEL", "TUR")


# Set the years to be analyzed.
# years <- 1960:2020
# years <- 2002
years <- 1971:1973
# years <- 1971:1978
# years <- 1971
# years <- 1960:1961
# years <- 2016:2018

# Set aggregation files
aggregation_tables_dir <- "aggregation_tables"
targeted_aggregations_file <- system.file(aggregation_tables_dir, "targeted_aggregations.xlsx",
                                          package = "CLPFUDecompositionDatabase")

# Set the database version to be used for this analysis
database_version <- "v1.2"

# Should we release the results?
release <- FALSE

# End user-adjustable parameters.





#
# Set up some machine-specific parameters,
# mostly for input and output locations.
#

sys_info <- Sys.info()
if (startsWith(sys_info[["nodename"]], "Mac")) {
  setup <- PFUSetup::get_abs_paths()
} else if (endsWith(sys_info[["nodename"]], "arc4.leeds.ac.uk")) {
  uname <- sys_info[["user"]]
  setup <- PFUSetup::get_abs_paths(home_path <- "/nobackup",
                                   dropbox_path = uname)
  # Set the location for the _targets folder.
  targets::tar_config_set(store = file.path(setup[["output_data_path"]], "_targets/"))
} else {
  stop("Unknown system in _targets.R for PFUAggDatabase. Can't set input and output locations.")
}

# Set up for multithreaded work on the local machine.
future::plan(future.callr::callr)

# Set options for all targets.
targets::tar_option_set(
  packages = "CLPFUDecompositionDatabase",
  # Indicate that storage and retrieval of subtargets
  # should be done by the worker thread,
  # not the main thread.
  # These options set defaults for all targets.
  # Individual targets can override.
  storage = "worker",
  retrieval = "worker",
  # Tell targets to NOT keep everything in memory ...
  memory = "transient",
  # ... and to garbage-collect the memory when done.
  garbage_collection = TRUE
)

# Pull in the pipeline
CLPFUDecompositionDatabase::get_pipeline(countries = countries,
                                         years = years,
                                         database_version = database_version,
                                         targeted_aggregations_file = targeted_aggregations_file,
                                         pipeline_releases_folder = setup[["pipeline_releases_folder"]],
                                         pipeline_caches_folder = setup[["pipeline_caches_folder"]],
                                         reports_dest_folder = setup[["reports_dest_folder"]],
                                         release = release)




