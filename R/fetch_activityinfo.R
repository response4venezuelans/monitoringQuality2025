library(httr2)
library(dplyr)
library(stringr)

fetch_ai_form <- function(form_id) {
  request(str_c("https://www.activityinfo.org/resources/query/v43/form/", form_id)) |>
    req_auth_bearer_token(Sys.getenv("ACTIVITYINFOTOKEN")) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE, flatten = TRUE) |>
    as_tibble()
}

get_data_from_ai <- function(type_of_request, filter_request) {
  data <- fetch_ai_form("cov6wkemnorsfs62i") |>
    rename(
      Record.ID                            = `_id`,
      Country.Country                      = `country.Country`,
      Country.Admin1                       = `country.Admin1`,
      Appealing.organisation.Name          = `appealing_org.Name`,
      Implementation.Set.up                = implementation_setup,
      Implementing.partner.Name            = `implementing_org.Name`,
      Month                                = month,
      Indicator.Sector                     = `indicator.sector`,
      Indicator.Indicator                  = `indicator.indicator_simplified`,
      Activity.Name                        = activity,
      Activity.Description                 = activity_description,
      RMRP.Activity                        = rmrp_activity,
      CVA                                  = cva,
      Value..in.USD.                       = cva_value,
      Delivery.mechanism                   = cva_mechanism,
      Quantity.of.output                   = output,
      Total.monthly.beneficiaries          = total_monthly_beneficiaries,
      Refugees.and.Migrants.IN.DESTINATION = new_indestination,
      Refugees.and.Migrants.IN.TRANSIT     = new_intransit,
      Host.Communities.Beneficiaries       = new_hostcomm,
      Refugees.and.Migrants.PENDULARS      = new_pendulars,
      Colombian.Returnees                  = new_col_returnees,
      Women.under.18                       = new_women_under18,
      Men.under.18                         = new_men_under18,
      Women.above.18                       = new_women_above18,
      Men.above.18                         = new_men_above18,
      Other.under.18                       = new_other_under18,
      Other.above.18                       = new_other_above18,
      Platform                             = platform,
      New.Population.Type.Sum              = calc_new_pop_type,
      New.AGD.Sum                          = calc_new_agd,
      indicator_ref                        = indicator
    ) |>
    select(
      -any_of(c("_lastEditTime", "_recordStatus", "intro_review",
                "appealing_org", "implementing_org", "country")),
      -starts_with("calc_")
    ) |>
    mutate(New.beneficiaries.of.the.month = Total.monthly.beneficiaries) |>
    left_join(indicators_2026_types, by = "indicator_ref") |>
    rename(Indicator.Indicator.Type = indicator_type) |>
    select(-indicator_ref)

  if (type_of_request == "country" && filter_request != "All") {
    data <- data |> filter(Country.Country == filter_request)
  } else if (type_of_request == "partner" && filter_request != "All") {
    data <- data |> filter(Appealing.organisation.Name == filter_request)
  }

  data
}
