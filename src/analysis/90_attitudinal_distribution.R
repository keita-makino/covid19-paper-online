attitudinal_distribution <- al %>%
  select(source, starts_with("a")) %>%
  pivot_longer(
    cols = starts_with("a"),
    names_to = "attitudinal",
    values_to = "value"
  ) %>%
  phase0_track(
    "attitudinal_distribution_initial",
    "attitudinal_distribution",
    unit = "item_responses"
  ) %>%
  filter(!is.na(value)) %>%
  phase0_track(
    "attitudinal_distribution_filter_not_missing",
    "attitudinal_distribution",
    unit = "item_responses"
  ) %>%
  filter(value != -99999) %>%
  phase0_track(
    "attitudinal_distribution_filter_missing_code",
    "attitudinal_distribution",
    unit = "item_responses"
  ) %>%
  group_by(source, attitudinal, value) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    value = case_when(
      value < 6 ~ value,
      T ~ value - 3
    )
  ) %>%
  group_by(source, attitudinal) %>%
  mutate(
    relative_frequency = n / sum(n)
  ) %>%
  ungroup()

phase0_write_csv(
  attitudinal_distribution,
  phase0_output_path("attitudinal_distribution.csv"),
  "attitudinal_distribution",
  "Attitudinal item response distributions by recruitment channel."
)

attitudinal_distribution %>%
  ggplot() +
  geom_bar(
    aes(x = value, y = relative_frequency, fill = source),
    stat = "identity",
    position = "dodge"
  ) +
  facet_grid(
    attitudinal ~ source,
  )

ggsave(phase0_output_path("attitudinal.png"), width = 10, height = 30, units = "in")
