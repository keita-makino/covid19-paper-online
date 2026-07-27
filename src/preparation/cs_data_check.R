data <- read_sav("./data/altogether_cleaned.sav")

filtered <- data %>%
    filter(meta_distributionchannel == "convenience") %>%
    select(-ends_with("zips")) %>%
    mutate_at(
        vars(starts_with("A04")),
        ~ case_when(
            .x > 5 ~ .x - 3,
            T ~ .x
        )
    )

data %>%
    filter(meta_distributionchannel == "convenience") %>%
    mutate(if_ca_resident = case_when(
        grepl("ca", d07_residence_state %>% str_to_lower()) ~ 1,
        T ~ 0
    )) %>%
    filter(if_ca_resident == 1)
