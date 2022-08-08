mm <- read_sav("./data/mm.sav") %>%
  select(all_of(tag_list)) %>%
  rename_all(~tag_list_long) %>%
  mutate(source = "mm", state = "CA")
re <- read_sav("./data/altogether_cleaned.sav") %>%
  select(
    all_of(tag_list_long),
    meta_distributionchannel,
    state
  ) %>%
  rename(source = meta_distributionchannel)

al <-
  rbind(
    re %>% filter(
      source != "mail" &
        source != "-88888"
    ),
    mm
  ) %>%
  mutate(
    source = case_when(
      source == "convenience" ~ "cs",
      source == "longitudinal" ~ "lg",
      source == "mailed_online" ~ "mo",
      source == "opinion_panel" ~ "op",
      T ~ "mm",
    )
  ) %>%
  filter(source != "cs") %>%
  mutate(
    source = source %>%
      factor(levels = c(
        "op",
        "lg",
        "mm",
        "mo"
      ))
  ) %>%
  filter(
    state == "CA" | source == "mm" | source == "mo"
  )
