# This script was used to generate data for work with Zeus and Matt.
# We're investigating details of specified IEA data.
# In particular,
# Cuba (Biogasoline),
# Austria, Switzerland, Sweden, Turkey, USA (Non-specified primary biofuels and waste).
# All countries (Manufacture [of Other sources], Other sources, Non-specified, Transfers)
# This script should be run from a place where we can get the SpecifiedIEA data frame.
# At this time, that is the PFUPipeline project.

specified_iea_data <- tar_read(SpecifiedIEA)

# Cuba biogasoline
cuba_biogasoline <- specified_iea_data |>
  dplyr::filter(Country == "CUB", startsWith(Product, "Biogasoline"))
cuba_biogasoline |>
  write.csv("~/Desktop/IEADetails/cuba_biogasoline.csv", row.names = FALSE)

# Non-specified primary biofuels and waste
nspbw <- specified_iea_data |>
  dplyr::filter(Country %in% c("AUT", "CHE", "SWE", "TUR", "USA"),
                startsWith(Product, "Non-specified primary biofuels and waste"))
nspbw |>
  write.csv("~/Desktop/IEADetails/Non-specified primary biofuels and waste.csv", row.names = FALSE)

# Other sources
other_sources <- specified_iea_data |>
  dplyr::filter(startsWith(Product, "Other sources"))
other_sources |>
  write.csv("~/Desktop/IEADetails/Other sources.csv", row.names = FALSE)

# Non-specified flows
non_specified <- specified_iea_data |>
  dplyr::filter(grepl("specified", .data[["Flow"]], fixed = TRUE))
non_specified |>
  write.csv("~/Desktop/IEADetails/Non-specified flows.csv", row.names = FALSE)

