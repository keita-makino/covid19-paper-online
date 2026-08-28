# Single source of truth for the socio complete-case conditions.
# filter_by_socio() and phase0_log_condition_attrition() share these so the
# per-condition breakdown sums exactly to what the filter drops.
# Note the intentional but inconsistent missing-code thresholds (reported,
# not fixed): education and income use > 0, the others use > -1.
socio_filter_conditions <- list(
  b01_yearborn_gt_1900 = rlang::quo(as.numeric(b01_yearborn) > 1900),
  b05_gender_present = rlang::quo(b05_gender > -1),
  b06_age_driver_license_present = rlang::quo(b06_age_driver_license > -1),
  b07_educational_background_gt_0 = rlang::quo(b07_educational_background > 0),
  b08_hh_income_gt_0 = rlang::quo(b08_hh_income > 0),
  d03_neighborhood_type_present = rlang::quo(d03_neighborhood_type > -1)
)

filter_by_socio <- function(data) {
  return(
    data %>%
      # unname(): dplyr::filter rejects named inputs when splicing.
      dplyr::filter(!!!unname(socio_filter_conditions))
  )
}

factorize_socio <- function(data) {
  return(
    data %>%
      mutate(
        b01_yearborn = b01_yearborn %>%
          as.numeric() %>%
          cut(
            c(1900, 1956, 1971, 1986, 2003),
            c("65+", "50-64", "35-49", "18-34")
          ) %>%
          factor(levels = c(
            "18-34",
            "35-49",
            "50-64",
            "65+"
          )),
        b06_age_driver_license = case_when(
          b06_age_driver_license < 0 ~ "No data",
          b06_age_driver_license == 0 ~ "No",
          T ~ "Yes"
        ) %>%
          factor(
            levels = c(
              "No",
              "Yes"
            )
          ),
        b07_educational_background = case_when(
          b07_educational_background < 1 ~ "No data",
          b07_educational_background < 3 ~ "High school or less",
          b07_educational_background < 5 ~ "Associate or college",
          T ~ "Graduate or professional",
        ) %>%
          factor(
            levels = c(
              "High school or less",
              "Associate or college",
              "Graduate or professional"
            )
          ),
        b08_hh_income = case_when(
          b08_hh_income <= 0 ~ NA_character_,
          b08_hh_income < 3 ~ "$0 - $49K",
          b08_hh_income < 5 ~ "$50K - $99K",
          b08_hh_income < 6 ~ "$100K - $149K",
          T ~ "$150K or more",
        ) %>%
          factor(
            levels = c(
              "$0 - $49K",
              "$50K - $99K",
              "$100K - $149K",
              "$150K or more"
            )
          ),
        d03_neighborhood_type = case_when(
          d03_neighborhood_type == 1 ~ "Urban",
          d03_neighborhood_type == 2 ~ "Suburban",
          d03_neighborhood_type == 3 |
            d03_neighborhood_type == 4 ~ "Town/Rural"
        ) %>%
          factor(
            levels = c(
              "Urban",
              "Suburban",
              "Town/Rural"
            )
          )
      ) %>%
      mutate(
        b05_gender = b05_gender %>% as_factor(),
        b06_age_driver_license = b06_age_driver_license %>% as_factor(),
        b07_educational_background = b07_educational_background %>% as_factor(),
        b08_hh_income = b08_hh_income %>% as_factor(),
        b13_smartphone_dv = b13_smartphone_dv %>% as_factor(),
      )
  )
}
