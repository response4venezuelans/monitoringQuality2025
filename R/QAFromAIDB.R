qa_check <- function(data) {
  # Perform quality assurance checks on 'data'
  # e.g., fix column names, check missing values, etc.
  data <- data |>
    ## Check for Output column with valid types
    mutate(QA_output = is_valid_output(Indicator.Indicator.Type, Quantity.of.output)) |>
    mutate(
      QA_TotalMonthlyBeneficiaries = is_valid_total_beneficiaries_of_month(Indicator.Indicator.Type, Total.monthly.beneficiaries)
    ) |>
    mutate(
      QA_NewBeneficiariesMonth = is_valid_new_beneficiaries_of_month(Indicator.Indicator.Type, New.beneficiaries.of.the.month)
    ) |>
    rowwise() |>
    mutate(
      QA_check_population_disaggregation = is_valid_population_type_disaggregation(
        Indicator.Indicator.Type,
        c(
          Refugees.and.Migrants.IN.DESTINATION,
          Refugees.and.Migrants.IN.TRANSIT,
          Host.Communities.Beneficiaries,
          Refugees.and.Migrants.PENDULARS,
          Colombian.Returnees
        ),
        New.beneficiaries.of.the.month
      ),
      QA_check_AGD = is_valid_age_gender_disaggregation(
        Indicator.Indicator.Type,
        c(
          Women.under.18,
          Men.under.18,
          Women.above.18,
          Men.above.18,
          Other.under.18,
          Other.above.18
        ),
        New.beneficiaries.of.the.month
      ),
      ## Specific Sector QA
      QA_Education = is_valid_agd_sector_specific(
        Indicator.Sector,
        "Education",
        Indicator.Indicator.Type,
        c(Women.under.18, Men.under.18, Other.under.18)),
        QA_ChildProtection = is_valid_agd_sector_specific(
          Indicator.Sector,
          "Child Protection",
          Indicator.Indicator.Type,
          c(Women.under.18, Men.under.18, Other.under.18)

        )
    ) |>
    ungroup() |>
    mutate(QA_valid_cva = is_valid_cva(CVA, Value..in.USD., Delivery.mechanism)) |>
    mutate(QA_admin = check_admin_validity(Country.Country, Country.Admin1, countryListDF)) |>
    mutate(QA_indicator = check_indicator_validity(Indicator.Sector, Indicator.Indicator, indicatorDF)) %>%
    mutate(QA_sum = rowSums(select(., starts_with("QA_"))))
  
  return(data)
}