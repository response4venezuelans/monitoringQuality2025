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
                                         "Platform" = "cuhb8obl0wjzz9r3",
                                         "Indicator Indicator Type" = "cuskmf7lctcszoga",
                                         "Country countryISO" = "c1u8kphm4vqtemz2",
                                         "Country Admin1ISOCode" = "cl3sspjkxeqq8yq6",
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
                                         "Platform" = "cuhb8obl0wjzz9r3",
                                         "Indicator Indicator Type" = "cuskmf7lctcszoga",
                                         "Country countryISO" = "c1u8kphm4vqtemz2",
                                         "Country Admin1ISOCode" = "cl3sspjkxeqq8yq6",
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
                                         "Platform" = "cuhb8obl0wjzz9r3",
                                         "Indicator Indicator Type" = "cuskmf7lctcszoga",
                                         "Country countryISO" = "c1u8kphm4vqtemz2",
                                         "Country Admin1ISOCode" = "cl3sspjkxeqq8yq6")
    
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
#' is_valid_output("Other", 5) # Returns 0
#'
is_valid_output <- function(indicator_type, output_data_column) {
  # Ensure the input is character
  if (!is.character(indicator_type)) {
    stop("indicator_type must be a character string.")
  }
  
  # Validate the condition
  result <- ifelse(
    indicator_type == "Direct Assistance" & is.na(output_data_column),
    0L,
    ifelse(
      indicator_type != "Direct Assistance" & !is.na(output_data_column) & output_data_column == 0,
      1L,
      0L
    )
  )
  
  return(result)
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

#' Check population disaggregation validity
#'
#' This function checks if the sum of specified population columns matches
#' the new beneficiaries for rows where the indicator type is "Direct Assistance".
#' It is designed to be used inside a dplyr::mutate() pipeline.
#'
#' @param data A data frame
#' @param population_columns A character vector of population column names
#' @param indicator_type_col Name of the column containing indicator type
#' @param new_beneficiaries_col Name of the column with new beneficiaries count
#'
#' @return A data frame with a new column `is_disaggregation_invalid` (1 = invalid, 0 = valid)
is_valid_population_type_disaggregation <- function(indicator_type, population_columns, new_beneficiaries_of_month) {
  case_when(
    indicator_type == "Direct Assistance" &
      sum(population_columns, na.rm = TRUE) != new_beneficiaries_of_month ~ 1L,
    TRUE ~ 0L
  )
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
  case_when(
    (indicator_type == "Direct Assistance" | indicator_type == "Capacity Building") &
      sum(population_columns, na.rm = TRUE) != new_beneficiaries_of_month ~ 1L,
    TRUE ~ 0L
  )
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
  return(ifelse(cva_column == "Yes", ifelse(cva_check, 1L, 0L), 0L))
}


#' Check Validity of Admin0 and Admin1
#'
#' This function checks if the reported admin0 and admin1 values belong to the provided table.
#'
#' @param reported_admin0 A character vector of reported admin0 values.
#' @param reported_admin1 A character vector of reported admin1 values.
#' @param valid_admin_table A data frame containing valid admin0 and admin1 values.
#'
#' @return A logical vector. Returns 0 if both admin0 and admin1 values are valid, 1 otherwise.

check_admin_validity <- function(reported_admin0, reported_admin1, valid_admin_table) {
  # Combine input into a dataframe
  input_df <- tibble(
    Country_Country = reported_admin0,
    Country_Admin1 = reported_admin1
  )
  # Join with valid admin table
  joined_df <- input_df |>
    dplyr::left_join(valid_admin_table, by = c(
      "Country_Country" = "Country", 
      "Country_Admin1" = "Admin1"
    ))
  # Return logical vector if match found
  validity_check <- ifelse(
    is.na(joined_df$countryISO) | is.na(joined_df$Admin1ISOCode),
    1,  # No match found
    0   # Match found
  )
  
  return(validity_check)
}

#' Check Validity of indicator and sector correspondance
#'
#' This function checks if the reported Sector and indicator values belong to the provided table.
#'
#' @param reported_admin0 A character vector of reported admin0 values.
#' @param reported_admin1 A character vector of reported admin1 values.
#' @param valid_admin_table A data frame containing valid admin0 and admin1 values.
#'
#' @return A logical vector. Returns 0 if both admin0 and admin1 values are valid, 1 otherwise.

check_indicator_validity <- function(reported_sector, reported_indicator, valid_indicator_table) {
  # Combine input into a dataframe
  input_df <- tibble(
    Sector = reported_sector,
    Indicator = reported_indicator
  )
  # Join with valid admin table
  joined_df <- input_df |>
    dplyr::left_join(valid_indicator_table, by = c(
      "Sector" = "Sector", 
      "Indicator" = "Indicator"
    ))
  # Return logical vector if match found
  validity_check <- ifelse(
    is.na(joined_df$CODE),
    1,  # No match found
    0   # Match found
  )
  
  return(validity_check)
}

## Functions for metrics in boxes
get_total_activities <- function(dataframe){
  return(nrow(dataframe))
}

get_total_activities_to_review <- function(dataframe, column_name) {
  dataframe %>%
    summarize(total = sum(.data[[column_name]], na.rm = TRUE)) %>%
    pull(total)
}

get_percentage_activities <- function(total_errors, total_activities) {
  if (total_activities == 0) {
    return(0)
  }
  
  round((total_errors / total_activities) * 100,2)
}

## functions for processing external excel file

#' Check if imported dataframe is compliant with the template
#'
#' This function checks if the uploaded excel file is compliant with template
#'
#' @param dataframe A compliant dataframe with 5W info.
#'
#' @return TRUE or FALSE de pending in compliancy test


check_dataframe_structure <- function(dataframe, template_file_path, sheet = 1) {
  # Read the template Excel file
  template <- read_excel(template_file_path, sheet = sheet)
  # Get column names of the template
  template_colnames <- colnames(template)
  # Get column names of the dataframe
  dataframe_colnames <- colnames(dataframe)
  # Check if the number of columns match
  if (length(template_colnames) != length(dataframe_colnames)) {
    return(FALSE)
  }
  # Check if column names match
  colnames_match <- all(template_colnames == dataframe_colnames)
  # Return the result
  if (colnames_match) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


rename_columns <- function(dataframe) {
  # Replace spaces, "(" and ")" with "."
  colnames(dataframe) <- colnames(dataframe) |>
    gsub(" ", ".", x = _) |>
    gsub("\\(", ".", x = _) |>
    gsub("\\)", ".", x = _)
  
  return(dataframe)
}

add_platform_column <- function(df) {
  df <- df %>%
    mutate(Platform = case_when(
      Country.Country == "Brazil" ~ "Brazil",
      Country.Country == "Chile" ~ "Chile",
      Country.Country == "Colombia" ~ "Colombia",
      Country.Country == "Ecuador" ~ "Ecuador",
      Country.Country == "Peru" ~ "Peru",
      Country.Country %in% c("Aruba", "Curacao", "Guyana", "Dominican Republic", "Trinidad and Tobago") ~ "Caribbean",
      Country.Country %in% c("Costa Rica", "Mexico", "Panama") ~ "Central America and Mexico",
      Country.Country %in% c("Argentina", "Paraguay", "Uruguay", "Bolivia") ~ "Southern Cone",
      
      TRUE ~ NA_character_
    ))
  return(df)
}

addIndicatorType<-function(df, indicatordf){
  
  indicatordf <- indicatordf %>%
    select(Sector, Indicator, Indicator.Type)
  
  df<-df%>%
    left_join(
      indicatordf, by = c(
        "Indicator.Sector" = "Sector", 
        "Indicator.Indicator" = "Indicator"
      )
    )%>%
    rename(Indicator.Indicator.Type = Indicator.Type)
           
  return(df)
    
}

addCountryISOCodes<-function(df, countryDF){
  df <- df %>%
    left_join(
      countryDF, by = c(
        "Country.Country" = "Country",
        "Country.Admin1" = "Admin1"
      )
    )%>%
    rename(
      "Country.countryISO"= "countryISO",
      "Country.Admin1ISOCode" = "Admin1ISOCode"
    )
  return(df)
}








