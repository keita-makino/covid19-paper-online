al %>%
  select(source, starts_with("a")) %>%
  pivot_longer(
    cols = starts_with("a"),
    names_to = "attitudinal",
    values_to = "value"
  ) %>%
  filter(value != -99999) %>%
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
    n = n / sum(n)
  ) %>%
  ggplot() +
  geom_bar(
    aes(x = value, y = n, fill = source),
    stat = "identity",
    position = "dodge"
  ) +
  facet_grid(
    attitudinal ~ source,
  )

ggsave("../output/attitudinal.png", width = 10, height = 30, units = "in")
