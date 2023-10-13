#' Create a series of efficiency reports
#'
#' Creates reports of efficiencies for machines in PSUT data frames.
#' Each report contains efficiencies for a single machine
#' across all countries.
#'
#' The report is focused IEA data where the last stage is final energy.
#'
#' @param eta_i_df A data frame of machine efficiencies in vectors.
#' @param reports_dest_folder The output folder for these reports.
#' @param release A boolean that tells whether to write the reports.
#'                Default is `FALSE`.
#'
#' @return Nothing. This function should be called for its side effect of
#'         creating reports.
#'
#' @export
create_iea_eta_i_reports <- function(eta_i_df, reports_dest_folder, release = FALSE) {
  if (!release) {
    return("Release not requested.")
  }
  # Expand the machine efficiency data.
  expanded_eta_i_data <- eta_i_df |>
    dplyr::filter(IEAMW == "IEA", Last.stage == "Final", Energy.type == "E") |>
    # Delete columns containing original PSUT matrices (if present)
    dplyr::mutate(
      R = NULL, U = NULL, U_feed = NULL, U_EIOU = NULL, r_EIOU = NULL,
      V = NULL, Y = NULL, S_units = NULL
    ) |>
    # Put the data in the correct format for expanding the vectors
    tidyr::pivot_longer(cols = "eta_i", names_to = "matnames", values_to = "matvals") |>
    # Expand the vectors
    matsindf::expand_to_tidy() |>
    # Clean up
    dplyr::rename(
      machine = rownames,
      eta_i = matvals
    ) |>
    dplyr::mutate(matnames = NULL, colnames = NULL)

  # Nest the efficiency data so that each item in the
  # column is a tibble of data for the graph.
  nested_eta_i_data <- expanded_eta_i_data |>
    tidyr::nest(.by = tidyr::all_of(c("Country", "Method", "Energy.type",
                                      "Last.stage", "IEAMW", "machine")),
                .key = "year_eta_i")

  # The function to create one graph
  create_eta_i_plot <- function(tibble_data, country_name, energy_type, machine_name) {
    ggplot2::ggplot() +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.1) +
      ggplot2::geom_hline(yintercept = 1, linewidth = 0.1) +
      ggplot2::geom_text(ggplot2::aes(x = 1990, y = -0.1,
                                      label = paste(country_name,
                                                    energy_type,
                                                    machine_name, sep = "; "))) +
      ggplot2::geom_point(data = tibble_data,
                          mapping = ggplot2::aes(x = Year, y = eta_i)) +
      ggplot2::labs(x = NULL, y = expression(eta[i])) +
      ggplot2::xlim(c(1960, 2020)) +
      ggplot2::ylim(c(-0.5, 1.5)) +
      MKHthemes::xy_theme()
  }

  # Code to make a column of plots.
  eta_i_graphs <- nested_eta_i_data |>
    dplyr::mutate(
      # plots = purrr::map(.x = year_eta_i, .f = create_plot)
      plots = purrr::pmap(.l = list(tibble_data = year_eta_i,
                                    country_name = Country,
                                    energy_type = Energy.type,
                                    machine_name = machine),
                          .f = create_eta_i_plot)
    )

  # Split the data frame by machines.
  split_by_machines_df <- split(eta_i_graphs, eta_i_graphs$machine)

  # A function that writes only one report
  save_plots_to_pdf <- function(plot, machine_name) {
    output_folder <- file.path(reports_dest_folder, "IEA eta_i reports", Sys.Date())
    dir.create(output_folder, showWarnings = FALSE)
    report_name <- gsub(pattern = "/",
                        replacement = "_",
                        machine_name,
                        fixed = TRUE)
    pdf(file = file.path(output_folder, paste0(report_name, ".pdf")),
        width = 8, height = 5)
    print(plot)
    dev.off()
    return(NULL)
  }

  # Save plots to PDF files for each machine
  lapply(names(split_by_machines_df), function(machine) {
    save_plots_to_pdf(plot = split_by_machines_df[[machine]]$plots,
                      machine_name = machine)
  })

  return(paste0("eta_i reports saved at `", reports_dest_folder, "`."))
}


