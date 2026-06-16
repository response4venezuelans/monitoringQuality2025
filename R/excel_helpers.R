library(dplyr)
library(stringr)
library(readxl)

# Returns TRUE if the dataframe column structure matches the template
check_dataframe_structure <- function(dataframe, template_file_path, sheet = 1) {
  template_colnames <- read_excel(template_file_path, sheet = sheet) |> names()
  identical(names(dataframe), template_colnames)
}

# Replaces spaces and parentheses in column names with dots
rename_columns <- function(dataframe) {
  dataframe |>
    rename_with(~ str_replace_all(.x, c(" " = ".", "\\(" = ".", "\\)" = ".")))
}

add_platform_column <- function(df) {
  df |>
    mutate(Platform = case_when(
      Country.Country == "Brazil"   ~ "Brazil",
      Country.Country == "Chile"    ~ "Chile",
      Country.Country == "Colombia" ~ "Colombia",
      Country.Country == "Ecuador"  ~ "Ecuador",
      Country.Country == "Peru"     ~ "Peru",
      Country.Country %in% c("Aruba", "Curacao", "Guyana", "Dominican Republic", "Trinidad and Tobago") ~ "Caribbean",
      Country.Country %in% c("Costa Rica", "Mexico", "Panama") ~ "Central America and Mexico",
      Country.Country %in% c("Argentina", "Paraguay", "Uruguay", "Bolivia") ~ "Southern Cone",
      .default = NA_character_
    ))
}

add_indicator_type <- function(df, indicator_df) {
  df |>
    left_join(
      indicator_df |> select(Sector, Indicator, Indicator.Type),
      by = c("Indicator.Sector" = "Sector", "Indicator.Indicator" = "Indicator")
    ) |>
    rename(Indicator.Indicator.Type = Indicator.Type)
}

add_country_iso_codes <- function(df, country_df) {
  df |>
    left_join(country_df, by = c("Country.Country" = "Country", "Country.Admin1" = "Admin1")) |>
    rename(Country.countryISO = countryISO, Country.Admin1ISOCode = Admin1ISOCode)
}

# Computes New.Population.Type.Sum / New.AGD.Sum for Excel uploads, where
# ActivityInfo's calc_new_pop_type / calc_new_agd fields don't exist
add_population_sums <- function(df) {
  df |>
    rowwise() |>
    mutate(
      New.Population.Type.Sum = sum(
        c(Refugees.and.Migrants.IN.DESTINATION, Refugees.and.Migrants.IN.TRANSIT,
          Host.Communities.Beneficiaries, Refugees.and.Migrants.PENDULARS, Colombian.Returnees),
        na.rm = TRUE
      ),
      New.AGD.Sum = sum(
        c(Women.under.18, Men.under.18, Women.above.18, Men.above.18, Other.under.18, Other.above.18),
        na.rm = TRUE
      )
    ) |>
    ungroup()
}
