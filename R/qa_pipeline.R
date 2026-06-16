library(dplyr)

qa_check <- function(data) {
  data |>
    mutate(
      QA_output = is_valid_output(Indicator.Indicator.Type, Quantity.of.output),
      QA_TotalMonthlyBeneficiaries = is_valid_total_beneficiaries_of_month(
        Indicator.Indicator.Type, Total.monthly.beneficiaries
      ),
      QA_NewBeneficiariesMonth = is_valid_new_beneficiaries_of_month(
        Indicator.Indicator.Type, New.beneficiaries.of.the.month
      ),
      QA_check_population_disaggregation = is_valid_population_type_disaggregation(
        Indicator.Indicator.Type, New.Population.Type.Sum, New.beneficiaries.of.the.month
      ),
      QA_check_AGD = is_valid_age_gender_disaggregation(
        Indicator.Indicator.Type, New.AGD.Sum, New.beneficiaries.of.the.month
      )
    ) |>
    rowwise() |>
    mutate(
      QA_Education = is_valid_agd_sector_specific(
        Indicator.Sector, "Education", Indicator.Indicator.Type,
        c(Women.under.18, Men.under.18, Other.under.18)
      ),
      QA_ChildProtection = is_valid_agd_sector_specific(
        Indicator.Sector, "Child Protection", Indicator.Indicator.Type,
        c(Women.under.18, Men.under.18, Other.under.18)
      )
    ) |>
    ungroup() |>
    mutate(
      QA_valid_cva = is_valid_cva(CVA, Value..in.USD., Delivery.mechanism),
      QA_admin     = check_admin_validity(Country.Country, Country.Admin1, country_list_df),
      QA_indicator = check_indicator_validity(Indicator.Sector, Indicator.Indicator, indicator_df)
    ) |>
    rowwise() |>
    mutate(QA_sum = max(c_across(starts_with("QA_")), na.rm = TRUE)) |>
    ungroup()
}
