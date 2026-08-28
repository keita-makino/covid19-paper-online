al_attitudinal_mwu <-
  al_attitudinal_factor_6$scores %>%
  as.data.frame() %>%
  tibble() %>%
  mutate(
    source = al_attitudinal$source
  ) %>%
  phase0_track("mwu_attitudinal_initial", "al_attitudinal_mwu") %>%
  filter(!is.na(source)) %>%
  phase0_track("mwu_attitudinal_filter_missing_source", "al_attitudinal_mwu") %>%
  mutate(
    source = factor(
      source,
      levels = c("op", "lg", "cs", "mm", "mo")
    )
  )

phase0_mwu_attitudinal <- list()

for (i in 1:n) {
  for (alternative in c("l", "g")) {
    test <- pairwise.wilcox.test(
      al_attitudinal_mwu[, i] %>%
        deframe(),
      al_attitudinal_mwu[, n + 1] %>%
        deframe(),
      alternative = alternative,
      p.adjust.method = "BH"
    )

    print(test)

    tidy <- test$p.value %>%
      as.data.frame() %>%
      tibble::rownames_to_column("group1") %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(
        cols = -group1,
        names_to = "group2",
        values_to = "p.value"
      ) %>%
      dplyr::mutate(
        factor = names(al_attitudinal_mwu)[i],
        alternative = alternative,
        method = test$method,
        p.adjust.method = test$p.adjust.method
      ) %>%
      dplyr::filter(!is.na(p.value))

    phase0_mwu_attitudinal[[length(phase0_mwu_attitudinal) + 1]] <- tidy
  }
  print("--------------------------------------------")
}

source_counts <- al_attitudinal_mwu %>%
  dplyr::count(source, name = "n_source") %>%
  dplyr::mutate(source = as.character(source))

phase0_mwu_attitudinal <- dplyr::bind_rows(phase0_mwu_attitudinal) %>%
  dplyr::left_join(
    source_counts,
    by = c("group1" = "source")
  ) %>%
  dplyr::rename(n_group1 = n_source) %>%
  dplyr::left_join(
    source_counts,
    by = c("group2" = "source")
  ) %>%
  dplyr::rename(n_group2 = n_source) %>%
  dplyr::mutate(n_total = nrow(al_attitudinal_mwu))

phase0_write_csv(
  phase0_mwu_attitudinal,
  phase0_output_path("mwu_attitudinal_pairwise.csv"),
  "mwu_attitudinal",
  "Pairwise Mann-Whitney U tests on factor scores."
)

phase0_write_csv(
  source_counts,
  phase0_output_path("mwu_attitudinal_source_counts.csv"),
  "mwu_attitudinal",
  "Analysis sample size by recruitment channel for Mann-Whitney tests."
)

al_attitudinal_mwu$source %>%
  table()
