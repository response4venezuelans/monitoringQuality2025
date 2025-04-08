qa_check <- function(data) {
  # Perform quality assurance checks on 'data'
  # e.g., fix column names, check missing values, etc.
  print(names(data))
  data <- data |>
    ## Check for Output column with valid types
    mutate(QAoutput = is_valid_output(Indicator.Indicator.Type, Quantity.of.output)) |>
    mutate(
      QATotalMonthlyBeneficiaries = is_valid_total_beneficiaries_of_month(Indicator.Indicator.Type, Total.monthly.beneficiaries)
    ) |>
    mutate(
      QANewBeneficiariesMonth = is_valid_new_beneficiaries_of_month(Indicator.Indicator.Type, New.beneficiaries.of.the.month)
    ) |>
    rowwise() |>
    mutate(
      check_population_disaggregation = is_valid_population_type_disaggregation(
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
      check_AGD = is_valid_age_gender_disaggregation(
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
      )
    )|>
    ungroup()|>
    mutate(
      QA_valid_cva = is_valid_cva(CVA,Value..in.USD.,Delivery.mechanism)
    )
  
  return(data)
}