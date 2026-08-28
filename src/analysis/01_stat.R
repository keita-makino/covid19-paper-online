test_socio <- function(target, title) {
  data <- al %>%
    filter(!is.na(.data[[target]])) %>%
    phase0_track(
      paste0("stat_", target, "_filter_not_missing"),
      paste0("stat_", target)
    ) %>%
    filter(.data[[target]] != -99999) %>%
    phase0_track(
      paste0("stat_", target, "_filter_missing_code"),
      paste0("stat_", target)
    ) %>%
    factorize_socio() %>%
    select(all_of(c(target, "source"))) %>%
    rename(value = all_of(target)) %>%
    filter(!is.na(value)) %>%
    phase0_track(
      paste0("stat_", target, "_filter_invalid_transformed"),
      paste0("stat_", target)
    ) %>%
    mutate(
      value = value %>% as_factor(),
      source = source %>%
        factor(levels = c("lg", "op", "cs", "mm", "mo"))
    ) %>%
    group_by(value, source)

  count_data <- data %>%
    count() %>%
    group_by(source) %>%
    mutate(relative_frequency = n / sum(n)) %>%
    ungroup() %>%
    mutate(
      variable = target,
      variable_label = title
    )

  phase0_write_csv(
    count_data,
    phase0_output_path("test", paste0(target, "_distribution.csv")),
    "stat",
    "Sociodemographic distribution counts and relative frequencies by recruitment channel."
  )

  plot <- count_data %>%
    ggplot() +
    geom_bar(
      aes(
        x = value,
        y = relative_frequency,
        fill = source
      ),
      stat = "identity",
      position = "dodge"
    ) +
    labs(
      x = "Response",
      y = "Relative Frequency"
    ) +
    ylim(
      c(0, 1)
    ) +
    scale_fill_discrete(
      "Data Source",
      labels =
        c(
          paste0(
            "Longitudinal: N=",
            data %>%
              filter(source == "lg") %>%
              dim() %>%
              .[1]
          ),
          paste0(
            "Opinion panel: N=",
            data %>%
              filter(source == "op") %>%
              dim() %>%
              .[1]
          ),
          paste0(
            "Convenience sampling: N=",
            data %>%
              filter(source == "cs") %>%
              dim() %>%
              .[1]
          ),
          paste0(
            "Mail-back: N=",
            data %>%
              filter(source == "mm") %>%
              dim() %>%
              .[1]
          ),
          paste0(
            "Mail-online: N=",
            data %>%
              filter(source == "mo") %>%
              dim() %>%
              .[1]
          )
        )
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.5, "lines"),
      legend.margin = margin(-6, 0, 0, 0),
    ) +
    guides(
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    ) +
    xlab(
      title
    )

  output_path <- phase0_output_path("test", paste0(target, ".png"))
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    output_path,
    plot = plot,
    width = 6,
    height = 3.6,
    units = "in"
  )
}

test_socio("b01_yearborn", "Age")
test_socio("b05_gender", "Gender")
test_socio("b06_age_driver_license", "Driver's license")
test_socio("b07_educational_background", "Educational background")
test_socio("b08_hh_income", "Household income")
test_socio("d03_neighborhood_type", "Neighborhood type")
