filter_by_socio <- function(data) {
  return(
    data %>%
      filter(
        b01_yearborn %>% as.numeric() > 1900 &
          b05_gender > -1 &
          b06_age_driver_license > -1 &
          b07_educational_background > 0 &
          b08_hh_income > 0 &
          d03_neighborhood_type > -1
      )
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

lr_data_attitudinal <- cbind(
  al %>%
    filter(if_all(starts_with("a"), ~ !is.na(.))) %>%
    filter(if_all(starts_with("a"), ~ . > 0)),
  al_attitudinal_factor_6$scores
) %>%
  filter_by_socio() %>%
  tibble() %>%
  factorize_socio()

lr_data_remote <-
  al %>%
  filter_by_socio() %>%
  mutate(
    work_status_precovid =
      case_when(
        (
          is.na(c06a_work_location_primary_precovid) |
            c06a_work_location_primary_precovid < 1
        ) &
          (
            is.na(c06c_work_location_home_precovid) |
              c06c_work_location_home_precovid < 1
          ) ~ "Non-worker",
        (
          c06a_work_location_primary_precovid > 0 &
            (
              is.na(c06c_work_location_home_precovid) |
                c06c_work_location_home_precovid < 1
            )
        ) |
          (
            c06a_work_location_primary_precovid > 2 &
              (
                c06c_work_location_home_precovid == 1 |
                  c06c_work_location_home_precovid == 2
              )
          ) ~ "Commuter",
        (
          (
            is.na(c06a_work_location_primary_precovid) |
              c06a_work_location_primary_precovid < 1
          ) &
            c06c_work_location_home_precovid > 0
        ) |
          (
            (
              c06a_work_location_primary_precovid == 1 |
                c06a_work_location_primary_precovid == 2
            ) &
              c06c_work_location_home_precovid > 2
          ) ~ "Teleworker",
        T ~ "Hybrid-worker"
      ) %>% factor(
        levels = c(
          "Non-worker",
          "Commuter",
          "Hybrid-worker",
          "Teleworker"
        )
      ),
    work_status_2021 =
      case_when(
        (
          is.na(c07a_work_location_primary_2021) |
            c07a_work_location_primary_2021 < 1
        ) &
          (
            is.na(c07c_work_location_home_2021) |
              c07c_work_location_home_2021 < 1
          ) ~ "Non-worker",
        (
          c07a_work_location_primary_2021 > 0 &
            (
              is.na(c07c_work_location_home_2021) |
                c07c_work_location_home_2021 < 1
            )
        ) |
          (
            c07a_work_location_primary_2021 > 2 &
              (
                c07c_work_location_home_2021 == 1 |
                  c07c_work_location_home_2021 == 2
              )
          ) ~ "Commuter",
        (
          (
            is.na(c07a_work_location_primary_2021) |
              c07a_work_location_primary_2021 < 1
          ) &
            c07c_work_location_home_2021 > 0
        ) |
          (
            (
              c07a_work_location_primary_2021 == 1 |
                c07a_work_location_primary_2021 == 2
            ) &
              c07c_work_location_home_2021 > 2
          ) ~ "Teleworker",
        T ~ "Hybrid-worker"
      ) %>% factor(
        levels = c(
          "Non-worker",
          "Commuter",
          "Hybrid-worker",
          "Teleworker"
        )
      ),
    work_status_2022 =
      case_when(
        (
          is.na(c08a_work_location_primary_2022) |
            c08a_work_location_primary_2022 < 1
        ) &
          (
            is.na(c08c_work_location_home_2022) |
              c08c_work_location_home_2022 < 1
          ) ~ "Non-worker",
        (
          c08a_work_location_primary_2022 > 0 &
            (
              is.na(c08c_work_location_home_2022) |
                c08c_work_location_home_2022 < 1
            )
        ) |
          (
            c08a_work_location_primary_2022 > 2 &
              (
                c08c_work_location_home_2022 == 1 |
                  c08c_work_location_home_2022 == 2
              )
          ) ~ "Commuter",
        (
          (
            is.na(c08a_work_location_primary_2022) |
              c08a_work_location_primary_2022 < 1
          ) &
            c08c_work_location_home_2022 > 0
        ) |
          (
            (
              c08a_work_location_primary_2022 == 1 |
                c08a_work_location_primary_2022 == 2
            ) &
              c08c_work_location_home_2022 > 2
          ) ~ "Teleworker",
        T ~ "Hybrid-worker"
      ) %>% factor(
        levels = c(
          "Non-worker",
          "Commuter",
          "Hybrid-worker",
          "Teleworker"
        )
      ),
    source = source %>%
      factor(
        levels = c(
          "op",
          "lg",
          "mm",
          "mo"
        )
      )
  ) %>%
  factorize_socio()
