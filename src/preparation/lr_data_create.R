al_complete_attitudinal <- al %>%
  filter(if_all(starts_with("a"), ~ !is.na(.))) %>%
  phase0_track("lr_data_attitudinal_filter_missing", "al") %>%
  filter(if_all(starts_with("a"), ~ . > 0)) %>%
  phase0_track("lr_data_attitudinal_filter_nonpositive", "al")

stopifnot(nrow(al_complete_attitudinal) == nrow(al_attitudinal_factor_6$scores))

lr_data_attitudinal <- cbind(
  al_complete_attitudinal,
  al_attitudinal_factor_6$scores
) %>%
  phase0_track("lr_data_attitudinal_cbind_factor_scores", "lr_data_attitudinal") %>%
  filter(!is.na(source)) %>%
  phase0_track("lr_data_attitudinal_filter_missing_source", "lr_data_attitudinal") %>%
  phase0_log_condition_attrition("lr_data_attitudinal", socio_filter_conditions) %>%
  filter_by_socio() %>%
  phase0_track("lr_data_attitudinal_filter_by_socio", "lr_data_attitudinal") %>%
  tibble() %>%
  factorize_socio() %>%
  phase0_track("lr_data_attitudinal_factorize_socio", "lr_data_attitudinal")

lr_data_remote <-
  al %>%
  phase0_track("lr_data_remote_initial", "lr_data_remote") %>%
  phase0_log_condition_attrition("lr_data_remote", socio_filter_conditions) %>%
  filter_by_socio() %>%
  phase0_track("lr_data_remote_filter_by_socio", "lr_data_remote") %>%
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
          "cs",
          "mm",
          "mo"
        )
      )
  ) %>%
  phase0_track("lr_data_remote_mutate_work_status", "lr_data_remote") %>%
  factorize_socio() %>%
  phase0_track("lr_data_remote_factorize_socio", "lr_data_remote")
