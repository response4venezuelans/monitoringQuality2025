library(activityinfo)
activityInfoToken(Sys.getenv("ACTIVITYINFOTOKEN"), prompt = FALSE)
getDataFromAI <- function(typeOfRequest, filterRequest) {
  if (typeOfRequest == "country" & filterRequest != "All") {
    value <- filterRequest
    Monitoring5WqueryTable <- queryTable("cua8ntdm07falxuu7r",
                                         "Record ID" = "cua8ntdm07falxuu7r._id",
                                         "Country Country" = "LOOKUPKEY(c3qnjdvm4vqhg8s2, cbdjswrm8dkqm6e4)",
                                         "Country Admin1" = "LOOKUPKEY(c3qnjdvm4vqhg8s2, ciin9drm8dkqms05)",
                                         "Admin2" = "LOOKUPKEY(cdm4lv6m4vqi3by7, [4enfgfm4vqi7g49])",
                                         "Appealing organisation Name" = "LOOKUPKEY(c8qpb9tm8946o023, cggtvzem8dksakw8)",
                                         #"Appealing organisation AOIDORG" = "LOOKUPKEY(c5648gjkx69ra2v9, rzbjp5m6fkic3m3)",
                                         "Implementation Set up" = "ckjtet4kx69smeog",
                                         #"Implementing partner AOIDORG" = "LOOKUPKEY(cdocy6flctaah8c4, 1)",
                                         "Implementing partner Name" = "LOOKUPKEY(cmjno06m8943iuj2, c3tco7m8dksobwa)",
                                         "Month" = "clqgqrqkyueahma8",
                                         "Indicator Sector" = "LOOKUPKEY(cco8s7klctg5i192q, w5wdfam658oem73)",
                                         "Indicator Indicator" = "LOOKUPKEY(cco8s7klctg5i192q, fut7ttm658oem84)",
                                         "Activity Name" = "c3p669wkx6a7oyo4",
                                         "Activity Description" = "c8hxf50kx6a7vp65",
                                         "RMRP Activity" = "cuf3og8kx6amylmf",
                                         "CVA" = "cbvqg4jkx6b1kii7",
                                         "Value (in USD)" = "clwkfmckx6b2msu9",
                                         "Delivery mechanism" = "cg3rikqkx6b3z1kf",
                                         "Quantity of output" = "cm6no26kx6b8fqoh",
                                         "Total monthly beneficiaries" = "cto1biukx6kwvnj4k",
                                         "New beneficiaries of the month" = "c43j49ikx6kxyyc4l",
                                         "Refugees and Migrants IN DESTINATION" = "cz3yof2kx6l024p4m",
                                         "Refugees and Migrants IN TRANSIT" = "c8kl5o2kx6l0jip4n",
                                         "Host Communities Beneficiaries" = "c5z8bvakx6l10d84o",
                                         "Refugees and Migrants PENDULARS" = "c72dmskkx6l1hl04p",
                                         "Colombian Returnees" = "cmoqhuckx6l4q9z4q",
                                         "Women under 18" = "cwrxeaekx6l63na4s",
                                         "Men under 18" = "ccx7xhekx6l6jnk4t",
                                         "Women above 18" = "c3l36n2kx6l70kp4u",
                                         "Men above 18" = "ctd27ackx6l7g814v",
                                         "Other under 18" = "ckjcuiokx6l9a504w",
                                         "Other above 18" = "cq4hs3skx6lggpj4x",
                                         "Indicator Indicator Type" = "cuskmf7lctcszoga",
                                         "Country countryISO" = "c1u8kphm4vqtemz2",
                                         "Country Admin1ISOCode" = "cl3sspjkxeqq8yq6",
                                         "Platform" = "cuhb8obl0wjzz9r3",
                                         filter = sprintf("LOOKUPKEY(c3qnjdvm4vqhg8s2, cbdjswrm8dkqm6e4) == '%s'", value))
  } else if (typeOfRequest == "partner" & filterRequest != "All") {
    value <- filterRequest
    Monitoring5WqueryTable <- queryTable("cua8ntdm07falxuu7r",
                                         "Record ID" = "cua8ntdm07falxuu7r._id",
                                         "Country Country" = "LOOKUPKEY(c3qnjdvm4vqhg8s2, cbdjswrm8dkqm6e4)",
                                         "Country Admin1" = "LOOKUPKEY(c3qnjdvm4vqhg8s2, ciin9drm8dkqms05)",
                                         "Admin2" = "LOOKUPKEY(cdm4lv6m4vqi3by7, [4enfgfm4vqi7g49])",
                                         "Appealing organisation Name" = "LOOKUPKEY(c8qpb9tm8946o023, cggtvzem8dksakw8)",
                                         #"Appealing organisation AOIDORG" = "LOOKUPKEY(c5648gjkx69ra2v9, rzbjp5m6fkic3m3)",
                                         "Implementation Set up" = "ckjtet4kx69smeog",
                                         #"Implementing partner AOIDORG" = "LOOKUPKEY(cdocy6flctaah8c4, 1)",
                                         "Implementing partner Name" = "LOOKUPKEY(cmjno06m8943iuj2, c3tco7m8dksobwa)",
                                         "Month" = "clqgqrqkyueahma8",
                                         "Indicator Sector" = "LOOKUPKEY(cco8s7klctg5i192q, w5wdfam658oem73)",
                                         "Indicator Indicator" = "LOOKUPKEY(cco8s7klctg5i192q, fut7ttm658oem84)",
                                         "Activity Name" = "c3p669wkx6a7oyo4",
                                         "Activity Description" = "c8hxf50kx6a7vp65",
                                         "RMRP Activity" = "cuf3og8kx6amylmf",
                                         "CVA" = "cbvqg4jkx6b1kii7",
                                         "Value (in USD)" = "clwkfmckx6b2msu9",
                                         "Delivery mechanism" = "cg3rikqkx6b3z1kf",
                                         "Quantity of output" = "cm6no26kx6b8fqoh",
                                         "Total monthly beneficiaries" = "cto1biukx6kwvnj4k",
                                         "New beneficiaries of the month" = "c43j49ikx6kxyyc4l",
                                         "Refugees and Migrants IN DESTINATION" = "cz3yof2kx6l024p4m",
                                         "Refugees and Migrants IN TRANSIT" = "c8kl5o2kx6l0jip4n",
                                         "Host Communities Beneficiaries" = "c5z8bvakx6l10d84o",
                                         "Refugees and Migrants PENDULARS" = "c72dmskkx6l1hl04p",
                                         "Colombian Returnees" = "cmoqhuckx6l4q9z4q",
                                         "Women under 18" = "cwrxeaekx6l63na4s",
                                         "Men under 18" = "ccx7xhekx6l6jnk4t",
                                         "Women above 18" = "c3l36n2kx6l70kp4u",
                                         "Men above 18" = "ctd27ackx6l7g814v",
                                         "Other under 18" = "ckjcuiokx6l9a504w",
                                         "Other above 18" = "cq4hs3skx6lggpj4x",
                                         "Indicator Indicator Type" = "cuskmf7lctcszoga",
                                         "Country countryISO" = "c1u8kphm4vqtemz2",
                                         "Country Admin1ISOCode" = "cl3sspjkxeqq8yq6",
                                         "Platform" = "cuhb8obl0wjzz9r3",
                                         filter = sprintf("LOOKUPKEY(c8qpb9tm8946o023, cggtvzem8dksakw8) == '%s'", value))
  } else if (typeOfRequest == "partner" & filterRequest == "All" || 
             typeOfRequest == "country" & filterRequest == "All") {
    Monitoring5WqueryTable <- queryTable("cua8ntdm07falxuu7r",
                                         "Record ID" = "cua8ntdm07falxuu7r._id",
                                         "Country Country" = "LOOKUPKEY(c3qnjdvm4vqhg8s2, cbdjswrm8dkqm6e4)",
                                         "Country Admin1" = "LOOKUPKEY(c3qnjdvm4vqhg8s2, ciin9drm8dkqms05)",
                                         "Admin2" = "LOOKUPKEY(cdm4lv6m4vqi3by7, [4enfgfm4vqi7g49])",
                                         "Appealing organisation Name" = "LOOKUPKEY(c8qpb9tm8946o023, cggtvzem8dksakw8)",
                                         #"Appealing organisation AOIDORG" = "LOOKUPKEY(c5648gjkx69ra2v9, rzbjp5m6fkic3m3)",
                                         "Implementation Set up" = "ckjtet4kx69smeog",
                                         #"Implementing partner AOIDORG" = "LOOKUPKEY(cdocy6flctaah8c4, 1)",
                                         "Implementing partner Name" = "LOOKUPKEY(cmjno06m8943iuj2, c3tco7m8dksobwa)",
                                         "Month" = "clqgqrqkyueahma8",
                                         "Indicator Sector" = "LOOKUPKEY(cco8s7klctg5i192q, w5wdfam658oem73)",
                                         "Indicator Indicator" = "LOOKUPKEY(cco8s7klctg5i192q, fut7ttm658oem84)",
                                         "Activity Name" = "c3p669wkx6a7oyo4",
                                         "Activity Description" = "c8hxf50kx6a7vp65",
                                         "RMRP Activity" = "cuf3og8kx6amylmf",
                                         "CVA" = "cbvqg4jkx6b1kii7",
                                         "Value (in USD)" = "clwkfmckx6b2msu9",
                                         "Delivery mechanism" = "cg3rikqkx6b3z1kf",
                                         "Quantity of output" = "cm6no26kx6b8fqoh",
                                         "Total monthly beneficiaries" = "cto1biukx6kwvnj4k",
                                         "New beneficiaries of the month" = "c43j49ikx6kxyyc4l",
                                         "Refugees and Migrants IN DESTINATION" = "cz3yof2kx6l024p4m",
                                         "Refugees and Migrants IN TRANSIT" = "c8kl5o2kx6l0jip4n",
                                         "Host Communities Beneficiaries" = "c5z8bvakx6l10d84o",
                                         "Refugees and Migrants PENDULARS" = "c72dmskkx6l1hl04p",
                                         "Colombian Returnees" = "cmoqhuckx6l4q9z4q",
                                         "Women under 18" = "cwrxeaekx6l63na4s",
                                         "Men under 18" = "ccx7xhekx6l6jnk4t",
                                         "Women above 18" = "c3l36n2kx6l70kp4u",
                                         "Men above 18" = "ctd27ackx6l7g814v",
                                         "Other under 18" = "ckjcuiokx6l9a504w",
                                         "Other above 18" = "cq4hs3skx6lggpj4x",
                                         "Indicator Indicator Type" = "cuskmf7lctcszoga",
                                         "Country countryISO" = "c1u8kphm4vqtemz2",
                                         "Country Admin1ISOCode" = "cl3sspjkxeqq8yq6",
                                         "Platform" = "cuhb8obl0wjzz9r3")
    
  }
  return(Monitoring5WqueryTable)
}


#' Check if the output data column is valid for a given indicator type
#'
#' This function checks whether the `outputDataColumn` is `NA` when the 
#' `indicatorType` is "Direct Assistance". If the condition is met, it 
#' returns `TRUE`, otherwise `FALSE`.
#'
#' @param indicator_type A character string specifying the indicator type.
#' @param output_data_column A value that can be `NA` or any other data type.
#'
#' @return An integer vector (`1` for errors, `0` for valid cases).
#' 
#' @examples
#' is_valid_output("Direct Assistance", NA) # Returns 1
#' is_valid_output("Indirect Assistance", 5) # Returns 0
#'
is_valid_output <- function(indicator_type, output_data_column) {
  
  # Ensure the input is character
  if (!is.character(indicator_type)) {
    stop("indicator_type must be a character string.")
  }
  
  # Validate the condition
  if (indicator_type == "Direct Assistance" && is.na(output_data_column)) {
    return(1L)
  } else {
    return(0L)
  }
}

#' Validate the total beneficiaries for a given indicator type
#'
#' This function checks whether `total_monthly_beneficiaries_data_column` is `NA` or `0` 
#' for certain `indicator_type` values. If it meets the error conditions, 
#' it returns `1`, otherwise it returns `0`.
#'
#' @param indicator_type A character vector specifying the indicator types.
#' @param total_monthly_beneficiaries_data_column A numeric vector or NA values.
#'
#' @return An integer vector (`1` for errors, `0` for valid cases).
#' 
#' @examples
#' is_valid_total_beneficiaries_of_month(c("Infrastructure", "Health"), c(NA, 5)) # Returns c(0, 0)
#' is_valid_total_beneficiaries_of_month(c("Health", "Other"), c(NA, 0)) # Returns c(1, 1)
#'
is_valid_total_beneficiaries_of_month <- function(indicator_type, total_monthly_beneficiaries_data_column) {
  
  # Ensure the input is a character vector
  if (!is.character(indicator_type)) {
    stop("indicator_type must be a character vector.")
  }
  
  # Define valid types that should NOT trigger an error
  optional_types <- c("Infrastructure", "Mechanism/Advocacy", "Other")
  
  # Logical condition: Should return 1 if NOT in valid types and has NA or zero
  error_condition <- (!indicator_type %in% optional_types) & 
    (is.na(total_monthly_beneficiaries_data_column) | 
       total_monthly_beneficiaries_data_column == 0)
  
  # Explicitly return numeric values (1 for error, 0 for valid cases)
  return(ifelse(error_condition, 1L, 0L))
}



#' Validate new beneficiaries count based on indicator type
#'
#' This function checks the validity of `new_beneficiaries_of_month` based on `indicator_type`. 
#' It returns `1` for invalid values and `0` for valid cases.
#'
#' **Validation Rules:**
#' - If `indicator_type` is `"Direct Assistance"`, `new_beneficiaries_of_month` **must be greater than 0**. (Error if `≤ 0` or `NA`).
#' - If `indicator_type` is `"Capacity Building"`, `new_beneficiaries_of_month` **must be ≥ 0**. (Error if `NA` or `< 0`).
#' - For **all other types**, `new_beneficiaries_of_month` **must be `NA`**. (Error if it has any numeric value).
#'
#' @param indicator_type A character vector specifying the type of indicator.
#' @param new_beneficiaries_of_month A numeric vector indicating new beneficiaries for the month.
#'
#' @return An integer vector (`1` for invalid values, `0` for valid values).
#'
#' @examples
#' is_valid_new_beneficiaries_of_month("Direct Assistance", 5)  # Returns 0 (valid)
#' is_valid_new_beneficiaries_of_month("Direct Assistance", 0)  # Returns 1 (error)
#' is_valid_new_beneficiaries_of_month("Capacity Building", -1) # Returns 1 (error)
#' is_valid_new_beneficiaries_of_month("Other", 5)              # Returns 1 (error)
#' is_valid_new_beneficiaries_of_month("Other", NA)             # Returns 0 (valid)
#'
is_valid_new_beneficiaries_of_month <- function(indicator_type, new_beneficiaries_of_month) {
  
  # Ensure indicator_type is a character vector
  if (!is.character(indicator_type)) {
    stop("indicator_type must be a character vector.")
  }
  
  # Logical conditions for each rule
  direct_assistance_error <- (indicator_type == "Direct Assistance") & (is.na(new_beneficiaries_of_month) | new_beneficiaries_of_month <= 0)
  capacity_building_error <- (indicator_type == "Capacity Building") & (is.na(new_beneficiaries_of_month) | new_beneficiaries_of_month < 0)
  other_types_error <- !(indicator_type %in% c("Direct Assistance", "Capacity Building")) & (!is.na(new_beneficiaries_of_month))
  
  # Combine conditions and return 1 for errors, 0 for valid cases
  error_condition <- direct_assistance_error | capacity_building_error | other_types_error
  
  return(ifelse(error_condition, 1L, 0L))
}

#' Validate population type disaggregation
#'
#' This function checks if the sum of `population_columns` matches `new_beneficiaries_of_month`
#' for `"Direct Assistance"`, while ensuring `new_beneficiaries_of_month` is `NA` for other indicator types.
#' The function is vectorized to work efficiently inside `dplyr::mutate()`.
#'
#' **Validation Rules:**
#' - If `indicator_type == "Direct Assistance"`:
#'   - Return `0` (valid) if `sum(population_columns, na.rm = TRUE) == new_beneficiaries_of_month`
#'   - Otherwise, return `1` (error)
#' - If `indicator_type != "Direct Assistance"`:
#'   - Return `0` (valid) if `new_beneficiaries_of_month` is `NA`
#'   - Otherwise, return `1` (error)
#'
#' @param indicator_type A character vector indicating the type of indicator.
#' @param population_columns A list-column of numeric vectors representing disaggregated population counts.
#' @param new_beneficiaries_of_month A numeric vector representing total new beneficiaries per row.
#'
#' @return An integer vector (`1` for error, `0` for valid cases).
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#'
#' df <- tibble(
#'   indicator_type = c("Direct Assistance", "Direct Assistance", "Other", "Capacity Building"),
#'   population_columns = list(c(3, 2, 5), c(3, 2, 4), c(1, 1, 2), c(2, 3)),
#'   new_beneficiaries_of_month = c(10, 9, NA, 5)
#' )
#'
#' df <- df %>%
#'   mutate(validation_result = is_valid_population_type_disaggregation_dplyr(
#'     indicator_type, population_columns, new_beneficiaries_of_month
#'   ))
#'
#' print(df)
is_valid_population_type_disaggregation <- function(indicator_type, population_columns, new_beneficiaries_of_month) {
  
  # Ensure inputs are vectors of the same length
  if (length(indicator_type) != length(new_beneficiaries_of_month)) {
    stop("indicator_type and new_beneficiaries_of_month must be vectors of the same length.")
  }
  
  # Case 1: Direct Assistance - Sum of population_columns must match new_beneficiaries_of_month
  direct_assistance_check <- indicator_type == "Direct Assistance" &
    map_dbl(population_columns, ~ sum(.x, na.rm = TRUE)) != new_beneficiaries_of_month
  
  # Case 2: Other indicator types - new_beneficiaries_of_month must be NA
  other_check <- indicator_type != "Direct Assistance" & !is.na(new_beneficiaries_of_month)
  
  # Return 1 for errors, 0 for valid cases
  return(ifelse(direct_assistance_check | other_check, 1L, 0L))
}

#' Validate population type AGD for Direct Assistance and Capacity Building
#'
#' This function checks if the sum of `population_columns` matches `new_beneficiaries_of_month`
#' for `"Direct Assistance"` and `"Capacity Building"`, applying different validation rules:
#'
#' **Validation Rules:**
#' - If `indicator_type == "Direct Assistance"`:
#'   - Must have values in both `new_beneficiaries_of_month` and `population_columns`
#'   - Return `0` (valid) if `sum(population_columns, na.rm = TRUE) == new_beneficiaries_of_month`
#'   - Otherwise, return `1` (error)
#' - If `indicator_type == "Capacity Building"`:
#'   - If **both columns have values**, they must match.
#'   - If either column is **missing (NA)**, it's valid.
#'   - Return `0` if valid, `1` if mismatch.
#' - Other indicator types are ignored (return `NA`).
#'
#' @param indicator_type A character vector indicating the type of indicator.
#' @param population_columns A list-column of numeric vectors representing disaggregated population counts.
#' @param new_beneficiaries_of_month A numeric vector representing total new beneficiaries per row.
#'
#' @return An integer vector (`1` for error, `0` for valid cases, `NA` for other indicator types).
is_valid_age_gender_disaggregation <- function(indicator_type, population_columns, new_beneficiaries_of_month) {
  
  # Ensure inputs are vectors of the same length
  if (length(indicator_type) != length(new_beneficiaries_of_month)) {
    stop("indicator_type and new_beneficiaries_of_month must be vectors of the same length.")
  }
  
  # Direct Assistance: population sum must match new beneficiaries
  direct_assistance_check <- indicator_type == "Direct Assistance" &
    map_dbl(population_columns, ~ sum(.x, na.rm = TRUE)) != new_beneficiaries_of_month
  
  # Capacity Building: If data exists, it must match; if NA, it's valid
  capacity_building_check <- indicator_type == "Capacity Building" &
    (!is.na(new_beneficiaries_of_month) & !map_lgl(population_columns, is.null)) &
    (map_dbl(population_columns, ~ sum(.x, na.rm = TRUE)) != new_beneficiaries_of_month)
  
  # Return 1 for errors, 0 for valid cases, NA for other indicators
  return(ifelse(indicator_type %in% c("Direct Assistance", "Capacity Building"),
                ifelse(direct_assistance_check | capacity_building_check, 1L, 0L), NA_integer_))
}

#' Check Validity of CVA Data
#'
#' This function checks the validity of CVA (Cash and Voucher Assistance) data based on specified conditions.
#'
#' @param cva_column A character vector indicating whether CVA is applicable ("Yes" or "No").
#' @param cva_value A numeric vector representing the CVA value.
#' @param cva_type A character vector specifying the type of CVA. Valid types include "Beneficiary Bank Account", "Mobile Money", "Prepaid Card", "Cash Collection Over the Counter (OTC)", "Direct Cash (Cash in Hand)", "Cardless ATM Withdrawal", "Voucher", and "Other".
#'
#' @return An integer vector. Returns 0 for valid cases, 1 for error cases, and NA for cases where the check is not applicable.
#'
#' @examples
#' cva_column <- c("yes", "no", "yes")
#' cva_value <- c(100, 0, -50)
#' cva_type <- c("Mobile Money", "Voucher", "Direct Cash (Cash in Hand)")
#' is_valid_cva(cva_column, cva_value, cva_type)
#'
#' @export
is_valid_cva <- function(cva_column, cva_value, cva_type) {
  
  # Ensure inputs are vectors of the same length
  if (length(cva_column) != length(cva_value) || length(cva_column) != length(cva_type)) {
    stop("cva_column, cva_value, and cva_type must be vectors of the same length.")
  }
  
  # Define valid CVA types
  valid_cva_types <- c("Beneficiary Bank Account", "Mobile Money", "Prepaid Card", 
                       "Cash Collection Over the Counter (OTC)", "Direct Cash (Cash in Hand)", 
                       "Cardless ATM Withdrawal", "Voucher", "Other")
  
  # Check conditions
  cva_check <- cva_column == "Yes" & (cva_value <= 0 | !cva_type %in% valid_cva_types)
  
  # Return 1 for errors, 0 for valid cases, NA for other cases
  return(ifelse(cva_column == "Yes", ifelse(cva_check, 1L, 0L), NA_integer_))
}


#' Check Validity of Admin0 and Admin1 using dplyr
#'
#' This function checks if the reported admin0 and admin1 values belong to the provided table using dplyr.
#'
#' @param reported_admin0 A character vector of reported admin0 values.
#' @param reported_admin1 A character vector of reported admin1 values.
#' @param valid_admin_table A data frame containing valid admin0 and admin1 values.
#'
#' @return A logical vector. Returns TRUE if both admin0 and admin1 values are valid, FALSE otherwise.
#'
#' @examples
#' reported_admin0 <- c("RegionA", "RegionB", "RegionC")
#' reported_admin1 <- c("SubregionA1", "SubregionB1", "SubregionC1")
#' valid_admin_table <- data.frame(admin0 = c("RegionA", "RegionB"), admin1 = c("SubregionA1", "SubregionB1"))
#' check_admin_validity_dplyr(reported_admin0, reported_admin1, valid_admin_table)

check_admin_validity <- function(reported_admin0, reported_admin1, valid_admin_table) {
  
  # Ensure inputs are vectors of the same length
  if (length(reported_admin0) != length(reported_admin1)) {
    stop("reported_admin0 and reported_admin1 must be vectors of the same length.")
  }
  
  # Create a data frame from reported admin0 and admin1
  reported_admin_df <- data.frame(admin0 = reported_admin0, admin1 = reported_admin1)
  
  # Check validity using dplyr
  validity_check <- reported_admin_df |>
    left_join(valid_admin_table, by = c("admin0", "admin1")) |>
    mutate(valid = !is.na(admin0.y) & !is.na(admin1.y)) |>
    pull(valid)
  
  return(validity_check)
}







