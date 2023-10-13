
library(targets)
# targets::tar_make() to run the pipeline in a single thread.
# targets::tar_make_future(workers = 8) to execute across multiple cores.
# targets::tar_make(callr_function = NULL) to debug.
# targets::tar_read(<<target_name>>) to view the results.
# targets::tar_invalidate(<<target_name>>) to re-compute <<target_name>> and its dependents.
# targets::tar_destroy() to start over with everything,








# Set control parameters for the pipeline.

# Set the countries to be analyzed.
# countries <- c("GBR", "USA", "MEX")
# countries <- c("ZWE", "USA", "WRLD")
# countries <- "USA"
# countries <- "WRLD"
# countries <- "CHNM"
# countries <- "GHA"
# countries <- "all" # Run all countries in the PSUT target.
# countries <- c(PFUPipelineTools::canonical_countries, "WRLD") |> as.character()
countries <- PFUPipelineTools::canonical_countries |> as.character()
# Countries with unique allocation data plus BEL and TUR (for Pierre).
# countries <- c("BRA", "CAN", "CHNM", "DEU", "DNK", "ESP", "FRA", "GBR", "GHA", "GRC",
#                "HKG", "HND", "IDN", "IND", "JOR", "JPN", "KOR", "MEX", "NOR", "PRT",
#                "RUS", "USA", "WABK", "WMBK", "ZAF", "BEL", "TUR")


# Set the years to be analyzed.
years <- 1960:2020
# years <- 1971:1973
# years <- 1971:1978
# years <- 1971
# years <- 1960:1961
# years <- 2016:2018

# Set aggregation files
aggregation_tables_dir <- "aggregation_tables"
industry_aggregations_file <- system.file(aggregation_tables_dir, "industry_aggregations.xlsx",
                                          package = "CLPFUDecompositionDatabase")

# Set the releases to be used for this analysis
psut_release <- "20230618T131003Z-4c70f"
eta_i_release <- "20230925T185136Z-7b43b"

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
                                         psut_release = psut_release,
                                         eta_i_release = eta_i_release,
                                         industry_aggregations_file = industry_aggregations_file,
                                         pipeline_releases_folder = setup[["pipeline_releases_folder"]],
                                         pipeline_caches_folder = setup[["pipeline_caches_folder"]],
                                         reports_dest_folder = setup[["reports_dest_folder"]],
                                         release = release)








# # Set target options:
# tar_option_set(
#   packages = c("tibble") # packages that your targets need to run
#   # format = "qs", # Optionally set the default storage format. qs is fast.
#   #
#   # For distributed computing in tar_make(), supply a {crew} controller
#   # as discussed at https://books.ropensci.org/targets/crew.html.
#   # Choose a controller that suits your needs. For example, the following
#   # sets a controller with 2 workers which will run as local R processes:
#   #
#   #   controller = crew::crew_controller_local(workers = 2)
#   #
#   # Alternatively, if you want workers to run on a high-performance computing
#   # cluster, select a controller from the {crew.cluster} package. The following
#   # example is a controller for Sun Grid Engine (SGE).
#   #
#   #   controller = crew.cluster::crew_controller_sge(
#   #     workers = 50,
#   #     # Many clusters install R as an environment module, and you can load it
#   #     # with the script_lines argument. To select a specific verison of R,
#   #     # you may need to include a version string, e.g. "module load R/4.3.0".
#   #     # Check with your system administrator if you are unsure.
#   #     script_lines = "module load R"
#   #   )
#   #
#   # Set other options as needed.
# )
#
# # tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# # in {targets}, and its configuration for your machine is below.
# options(clustermq.scheduler = "multicore")
#
# # tar_make_future() is an older (pre-{crew}) way to do distributed computing
# # in {targets}, and its configuration for your machine is below.
# # Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.
#
# # Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# # source("other_functions.R") # Source other scripts as needed.
#
# # Replace the target list below with your own:
# list(
#   tar_target(
#     name = data,
#     command = tibble(x = rnorm(100), y = rnorm(100))
#     # format = "feather" # efficient storage for large data frames
#   ),
#   tar_target(
#     name = model,
#     command = coefficients(lm(y ~ x, data = data))
#   )
# )
