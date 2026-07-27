al_remote_mwu <-
  lr_data_remote %>%
  select(
    starts_with("c0"),
    starts_with("work_status"),
    source
  ) %>%
  mutate(
    work_status_precovid = work_status_precovid %>% as.numeric(),
    work_status_2021 = work_status_2021 %>% as.numeric(),
    work_status_2022 = work_status_2022 %>% as.numeric(),
    source = source %>%
      factor(
        levels = c(
          "lg",
          "op",
          "mm",
          "mo"
        )
      )
  )

for (i in c(
  "work_status_precovid",
  "work_status_2021",
  "work_status_2022"
)) {
  temp_data_n <-
    al_remote_mwu %>%
    filter(.data[[i]] > 1) %>%
    select(i) %>%
    deframe()
  temp_data_f <-
    al_remote_mwu %>%
    filter(.data[[i]] > 1) %>%
    select(source) %>%
    deframe()
  pairwise.wilcox.test(
    temp_data_n,
    temp_data_f,
    alternative = "l"
  ) %>%
    print()
  pairwise.wilcox.test(
    temp_data_n,
    temp_data_f,
    alternative = "g"
  ) %>%
    print()
  print("--------------------------------------------")
}

for (i in c(
  "c06a_work_location_primary_precovid",
  "c07a_work_location_primary_2021",
  "c08a_work_location_primary_2022"
)) {
  work_status <- paste0("work_status_", strsplit(i, "_")[[1]][5])
  home <- paste0(
    strsplit(i, "_")[[1]][1] %>% substr(1, 3),
    "c_",
    strsplit(i, "_")[[1]][2] %>% paste0("_"),
    strsplit(i, "_")[[1]][3],
    "_home_",
    strsplit(i, "_")[[1]][5]
  )
  temp_data_p <-
    al_remote_mwu %>%
    filter(.data[[work_status]] != "Non-worker") %>%
    select(i) %>%
    mutate(!!i := pmax(.data[[i]], 0))
  temp_data_h <-
    al_remote_mwu %>%
    filter(.data[[work_status]] != "Non-worker") %>%
    select(home) %>%
    mutate(!!home := pmax(.data[[home]], 0))
  temp_data_n <- (temp_data_h - temp_data_p) %>% deframe()
  temp_data_f <-
    al_remote_mwu %>%
    filter(.data[[i]] > 1) %>%
    select(source) %>%
    deframe()
  pairwise.wilcox.test(
    temp_data_n,
    temp_data_f,
    alternative = "l"
  ) %>%
    print()
  pairwise.wilcox.test(
    temp_data_n,
    temp_data_f,
    alternative = "g"
  ) %>%
    print()
  print("--------------------------------------------")
}
