do_lr_remote <- function(target) {
  column <- paste0("work_status_", target)
  column_employment <- paste0("c02_work_full_", target, "_dv")
  lr_data_temp <- lr_data_remote %>%
    filter(!!sym(column_employment) == 1) %>%
    phase0_track(
      paste0("lr_remote_", target, "_filter_full_time"),
      "lr_data_remote"
    ) %>%
    filter(!!sym(column) != "Non-worker") %>%
    phase0_track(
      paste0("lr_remote_", target, "_filter_non_worker"),
      "lr_data_remote"
    ) %>%
    # Exclude only source labels that were not mapped during data import.
    filter(!is.na(source)) %>%
    phase0_track(
      paste0("lr_remote_", target, "_filter_missing_source"),
      "lr_data_remote"
    ) %>%
    mutate(
      !!sym(column) := factor(
        !!sym(column),
        levels = c(
          "Commuter",
          "Hybrid-worker",
          "Teleworker"
        )
      )
    ) %>%
    phase0_track(
      paste0("lr_remote_", target, "_factor_outcome"),
      "lr_data_remote"
    )

  assign(
    "lr_data_temp",
    lr_data_temp,
    envir = globalenv()
  )

  model_lr <-
    multinom(
      paste0(
        column,
        " ~ ",
        "b01_yearborn +",
        "b05_gender +",
        "b06_age_driver_license +",
        "b07_educational_background +",
        "b08_hh_income +",
        "d03_neighborhood_type +",
        "source"
      ) %>%
        as.formula(),
      get("lr_data_temp", envir = globalenv()),
      Hess = T,
      trace = 0
    )

  assign(
    "model_lr",
    model_lr,
    envir = globalenv()
  )

  # Enforce retention of the channel dummy: the lower scope keeps `source`
  # in every candidate model, so backward selection cannot drop it.
  stepped <- step(
    get("model_lr", envir = globalenv()),
    scope = list(
      lower = ~ source,
      upper = get("model_lr", envir = globalenv()) %>% formula()
    ),
    direction = "backward",
    trace = 0
  )

  phase0_assert_source_retained(
    stepped,
    paste0("lr_remote_", target),
    "multinom"
  )
  phase0_assert_source_level_retained(
    stepped,
    lr_data_temp,
    paste0("lr_remote_", target),
    "multinom"
  )

  null <- multinom(
    paste0(
      column,
      " ~ ",
      "1"
    ) %>%
      as.formula(),
    get("lr_data_temp", envir = globalenv()),
    Hess = T,
    trace = 0
  )

  model_tidy <- phase0_tidy_multinom(
    stepped,
    target,
    null
  )
  phase0_append_model_tidy(model_tidy)

  phase0_write_csv(
    model_tidy,
    phase0_output_path("lr", paste0(target, "_multinom_tidy.csv")),
    "lr_remote",
    "Tidy multinomial logit coefficients after backward stepwise selection."
  )

  summary(stepped) %>% print()

  dim(lr_data_temp) %>%
    .[1] %>%
    print()

  lr_data_temp$source %>%
    table() %>%
    print()

  null %>%
    logLik() %>%
    print()

  stepped %>%
    logLik() %>%
    print()



  (
    1 -
      stepped %>% logLik() /
      null %>% logLik()
  ) %>%
    round(3) %>%
    format(nsmall = 3) %>%
    print()
  (
    1 -
      (stepped %>% logLik() - stepped$edf + null$edf - 1) /
        null %>% logLik()
  ) %>%
    round(3) %>%
    format(nsmall = 3) %>%
    print()



# Map each surviving coefficient to its plot metadata by coefficient NAME,
# never by position (AGENTS.md known fragility #2). Stepwise selection drops
# different terms at different time points, so any positional splice produces
# correctly formatted, wrong values.
coef_long <- stepped %>%
  coef() %>%
  t() %>%
  round(3) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  tibble::as_tibble() %>%
  tidyr::pivot_longer(
    -term,
    names_to = "outcome",
    values_to = "beta"
  )

sigma_long <- summary(stepped)$standard.errors %>%
  t() %>%
  round(3) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("term") %>%
  tibble::as_tibble() %>%
  tidyr::pivot_longer(
    -term,
    names_to = "outcome",
    values_to = "sigma"
  )

metadata <- lr_attitudinal_term_metadata(unique(coef_long$term))

plot <- coef_long %>%
  dplyr::left_join(sigma_long, by = c("term", "outcome")) %>%
  dplyr::left_join(metadata, by = "term") %>%
  mutate(
    type = factor(
      type,
      levels = c(
        "ASC",
        "Age",
        "Gender",
        "DL",
        "Education",
        "Income",
        "Neighborhood",
        "Source"
      )
    )
  ) %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_errorbar(
    aes(
      x = type,
      ymin = beta - 1.96 * sigma,
      ymax = beta + 1.96 * sigma,
      shape = classId,
      color = classId
    ),
    position = position_dodge(width = 0.5)
  ) +
  geom_point(
    aes(
      x = type,
      y = beta,
      shape = classId,
      color = classId
    ),
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  ggtext::geom_richtext(
    aes(
      label = class,
      x = type,
      y = beta + 1.96 * sigma,
      vjust = -(classId %>% as.numeric()) / 3.6 + 0.8,
      hjust = 0.2
    ),
    size = 3.6,
    alpha = 0.5,
    angle = 12,
    fill = NA,
    label.color = NA,
    position = position_dodge(width = 0.5)
  ) +
  scale_size_continuous(range = c(1, 10)) +
  # Clip at the panel boundary instead of dropping out-of-range rows:
  # ylim() would silently remove points outside +/-2.8.
  coord_flip(ylim = c(-2.8, 2.8)) +
  scale_x_discrete(limits = rev) +
  scale_color_discrete(guide = "none") +
  scale_shape_discrete(guide = "none") +
  labs(
    x = "Variable",
    y = "Coefficient"
  ) +
  facet_grid(
    cols = vars(outcome)
  )

ggsave(
  phase0_output_path("lr", paste0("plot_", target, ".png")),
  plot,
  width = 9,
  height = 3.3,
)
}

do_lr_remote("precovid")
do_lr_remote("2021")
do_lr_remote("2022")


phase0_write_csv(
  phase0_model_tidy %>%
    filter(target %in% c("precovid", "2021", "2022")) %>%
    arrange(target, outcome, term),
  phase0_output_path("lr", "remote_models_tidy.csv"),
  "lr_remote",
  "Combined tidy multinomial coefficients for all remote-work time points."
)

lr_data_remote %>%
  select(c06c_work_location_home_precovid, source) %>%
  filter(!is.na(c06c_work_location_home_precovid)) %>%
  phase0_track(
    "lr_remote_precovid_home_location_filter_not_missing",
    "lr_remote_precovid_home_location"
  ) %>%
  filter(c06c_work_location_home_precovid > -1) %>%
  phase0_track(
    "lr_remote_precovid_home_location_filter_missing_code",
    "lr_remote_precovid_home_location"
  ) %>%
  group_by_all() %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = c06c_work_location_home_precovid,
      y = perc,
      fill = source
    ),
    stat = "identity", position = "dodge"
  )


lr_data_remote %>%
  select(c07c_work_location_home_2021, source) %>%
  filter(!is.na(c07c_work_location_home_2021)) %>%
  phase0_track(
    "lr_remote_2021_home_location_filter_not_missing",
    "lr_remote_2021_home_location"
  ) %>%
  filter(c07c_work_location_home_2021 > -1) %>%
  phase0_track(
    "lr_remote_2021_home_location_filter_missing_code",
    "lr_remote_2021_home_location"
  ) %>%
  group_by_all() %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = c07c_work_location_home_2021,
      y = perc,
      fill = source
    ),
    stat = "identity", position = "dodge"
  )


lr_data_remote %>%
  select(c08c_work_location_home_2022, source) %>%
  filter(!is.na(c08c_work_location_home_2022)) %>%
  phase0_track(
    "lr_remote_2022_home_location_filter_not_missing",
    "lr_remote_2022_home_location"
  ) %>%
  filter(c08c_work_location_home_2022 > -1) %>%
  phase0_track(
    "lr_remote_2022_home_location_filter_missing_code",
    "lr_remote_2022_home_location"
  ) %>%
  group_by_all() %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  group_by(source) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = c08c_work_location_home_2022,
      y = perc,
      fill = source
    ),
    stat = "identity", position = "dodge"
  )
