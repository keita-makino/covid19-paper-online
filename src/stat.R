

test_socio <- function(target, title) {
  data <- al %>%
    factorize_socio() %>%
    select(target, source) %>%
    rename(value = target) %>%
    filter(!is.na(value)) %>%
    filter(value != -99999) %>%
    mutate(
      value = value %>% as_factor()
    ) %>%
    group_by(value, source)

  data %>%
    count() %>%
    group_by(source) %>%
    mutate(n = n / sum(n)) %>%
    ggplot() +
    geom_bar(
      aes(
        x = value,
        y = n,
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
            "Opinion panel: N=",
            data %>%
              filter(source == "op") %>%
              dim() %>%
              .[1]
          ),
          paste0(
            "Longitudinal: N=",
            data %>%
              filter(source == "lg") %>%
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
  ggsave(
    paste0("../dist/test/", target, ".png"),
    width = 6,
    height = 4,
    units = "in"
  )
}

test_socio("b01_yearborn", "Age")
test_socio("b05_gender", "Gender")
test_socio("b06_age_driver_license", "Driver's license")
test_socio("b07_educational_background", "Educational background")
test_socio("b08_hh_income", "Household income")
test_socio("d03_neighborhood_type", "Neighborhood type")
