
# Loading data ------------------------------------------------------------

# Path to data
# EA:
# path_to_iea_data <- "/home/eeear/Documents/Datasets/IEA/WEEBs/IEA Extended Energy Balances 2022 (TJ).csv"
# MKH:
path_to_iea_data <- "~/OneDrive - University of Leeds/Fellowship 1960-2015 PFU database research/IEA extended energy balance data/IEA 2022 energy balance data/IEA Extended Energy Balances 2022 (TJ).csv"

# Loading tidy IEA data
tidy_iea_raw <- IEATools::load_tidy_iea_df(path_to_iea_data)

# Loading electricity/heat data
# MKH: Here I have to change "chp" to "CHP" in Machines to be able to match
# with the tidy_iea_raw data frame.
# Probably worth going back to load_electricity_heat_output() and
# updating the machine names used with the IEATools::main_act_plants constant.
iea_elec_heat_generation <- IEATools::load_electricity_heat_output(path_to_iea_data)


# In the following code, I do the following checks:
# (1) Checking (Machine, OutputProduct):
# (1.a) Whether each (Machine, OutputProduct) has a match, for instance,
#       whether (Main activity producer electricity plants, Electricity)
#       is present in both datasets
# (1.b) The relative difference in (Machine, OutputProduct) when there is a match

# (2) Checking (Machine, InputProduct):
# (2.a) Whether each combination of (Machine, InputProduct) has a match
# (2.b) The relative difference in (Machine, InputProduct) when there is a match


# (1) Checking (Machine, OutputProduct) ------------------------------------------

# Calculating output of each (Machine, OutputProduct) combination in generation (gwh) data
iea_elec_heat_total_generation <- iea_elec_heat_generation |>
  dplyr::group_by(Country, Year, Machine, OutputProduct, Unit) |>
  dplyr::summarise(Edot = sum(Edot), .groups = "keep")

# Calculating output of (Machine, OutputProduct) in balanced tidy_iea_raw df
elec_heat_output_weeb <- tidy_iea_raw |>
  dplyr::filter(FlowAggregationPoint == "Transformation processes") |>
  dplyr::filter(Product %in% c("Electricity", "Heat")) |>
  # removing irrelevant flows, here we only look at main activity/autoproducer plants
  dplyr::filter(Flow %in% IEATools::main_act_plants) |>
  dplyr::filter(Edot>0)

# (1.a) Checking observations for which the (Machine, OutputProduct) present in the balances is not matched in the Gwh data
anti_bind_1 <- iea_elec_heat_total_generation |>
  dplyr::anti_join(elec_heat_output_weeb,
                   by = dplyr::join_by(Country, Year, Machine == Flow, OutputProduct == Product, Unit))

# (1.b) Now check the relative difference in output when there is a match, and order by descending order of difference
compare_outputs <- iea_elec_heat_total_generation |>
  dplyr::inner_join(elec_heat_output_weeb,
                    by = dplyr::join_by(Country, Year, Machine == Flow, OutputProduct == Product, Unit)) |>
  dplyr::mutate(diff = abs(Edot.x - Edot.y) / Edot.x) |>
  dplyr::arrange(dplyr::desc(diff))


# Note: the fact that there is a result in an anti_join here, for elec/heat plants,
# means that there is not even a match for all industries/flows.
# See example here:
a <- iea_elec_heat_total_generation |>
  dplyr::anti_join(elec_heat_output_weeb,
                   by = dplyr::join_by(Country, Year, Machine == Flow, Unit))

# and:
b <- elec_heat_output_weeb |>
  dplyr::anti_join(iea_elec_heat_total_generation,
                   by = dplyr::join_by(Country, Year, Flow == Machine, Unit))



# (2) Checking (Machine, InputProduct) ----------------------------

# Calculating input of each (Machine, InputProduct) combination in generation (gwh) data
iea_elec_heat_total_input <- iea_elec_heat_generation |>
  dplyr::group_by(Country, Year, Machine, InputProduct, Unit) |>
  dplyr::summarise(Edot = sum(Edot))

# Calculating input of each (Machine, InputProduct) in balanced tidy_iea_raw df
elec_heat_input_weeb <- tidy_iea_raw |>
  dplyr::filter(FlowAggregationPoint == "Transformation processes") |>
  dplyr::filter(Flow %in% IEATools::main_act_plants) |>
  dplyr::filter(Edot<0) |>
  dplyr::mutate(Edot = abs(Edot))

# (2.a) Checking whether each combination of (Machine, InputProduct) has a match
anti_bind_2 <- elec_heat_input_weeb |>
  dplyr::anti_join(iea_elec_heat_total_input,
                   by = dplyr::join_by(Country, Year, Flow == Machine, Product == InputProduct, Unit))

# (2.b) Checking the relative difference in inputs when there is a match
bind_2 <- elec_heat_input_weeb |>
  dplyr::inner_join(iea_elec_heat_total_input,
                    by = dplyr::join_by(Country, Year, Flow == Machine, Product == InputProduct, Unit)) |>
  dplyr::mutate(diff = abs(Edot.x - Edot.y) / Edot.x) |>
  dplyr::arrange(dplyr::desc(diff))
