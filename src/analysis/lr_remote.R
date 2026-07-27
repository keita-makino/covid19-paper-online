do_lr_remote <- function(target) {
  column <- paste0("work_status_", target)
  column_employment <- paste0("c02_work_full_", target, "_dv")
  lr_data_temp <- lr_data_remote %>%
    filter(!!sym(column_employment) == 1) %>%
    filter(!!sym(column) != "Non-worker") %>%
    mutate(
      !!sym(column) := factor(
        !!sym(column),
        levels = c(
          "Commuter",
          "Hybrid-worker",
          "Teleworker"
        )
      )
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

  stepped <- step(
    get("model_lr", envir = globalenv()),
    direction = "backward",
    trace = 0
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



  z <- summary(stepped)$coefficients /
    summary(stepped)$standard.errors
  p <- ((1 - pnorm(abs(z), 0, 1)) * 2) %>%
    as.data.frame() %>%
    round(3) %>%
    format(nsmall = 3) %>%
    t()
  coef <- stepped %>%
    coef() %>%
    as.data.frame() %>%
    round(3) %>%
    format(nsmall = 3) %>%
    t()
  for (i in 1:dim(p)[1]) {
    if (coef[i, 1] > 0) {
      coef[i, 1] <- paste0("  ", coef[i, 1])
    } else {
      coef[i, 1] <- paste0(" ", coef[i, 1])
    }
    if (p[i, 1] < 0.10 & p[i, 1] >= 0.05) {
      coef[i, 1] <- paste0("/", coef[i, 1])
    }
    if (p[i, 1] < 0.05 & p[i, 1] >= 0.01) {
      coef[i, 1] <- paste0("*", coef[i, 1])
    }
    if (p[i, 1] < 0.01) {
      coef[i, 1] <- paste0("**", coef[i, 1])
    }
    if (coef[i, 2] > 0) {
      coef[i, 2] <- paste0("  ", coef[i, 2])
    } else {
      coef[i, 2] <- paste0(" ", coef[i, 2])
    }
    if (p[i, 2] < 0.10 & p[i, 2] >= 0.05) {
      coef[i, 2] <- paste0("/", coef[i, 2])
    }
    if (p[i, 2] < 0.05 & p[i, 2] >= 0.01) {
      coef[i, 2] <- paste0("*", coef[i, 2])
    }
    if (p[i, 2] < 0.01) {
      coef[i, 2] <- paste0("**", coef[i, 2])
    }
  }
  write.csv(
    coef,
    file.path("..", "output", "lr", paste0(target, ".csv")),
    quote = F
  )
type <- c(
  "ASC",
  "Age", "Age", "Age",
  "Gender", "Gender",
  "DL",
  "Education", "Education",
  "Income", "Income", "Income",
  "Neighborhood", "Neighborhood",
  "Source", "Source", "Source"
)
class <- c(
  "ASC",
  "35-49", "50-64", "65+",
  "Male", "Non-binary",
  "Yes",
  "College", "Graduate",
  "$50K-$99K", "$100K-$149K", "$150K+",
  "Suburban", "Town/rural",
  "Longitudinal", "Convenience", "Mail-back", "Mail-online"
)
classId <- c(
  "1",
  "1", "2", "3",
  "1", "2",
  "1",
  "1", "2",
  "1", "2", "3",
  "1", "2",
  "1", "2", "3", "4"
)

beta <- stepped %>%
  coef() %>%
  as.data.frame() %>%
  round(3) %>%
  t() %>%
  as.data.frame() %>%
  tibble()
sigma <- summary(stepped)$standard.errors %>%
  as.data.frame() %>%
  round(3) %>%
  t() %>%
  as.data.frame() %>%
  tibble()

if (target == "precovid") {
  beta <- rbind(beta[1:4, ], c(NA, NA), c(NA, NA), beta[5:15, ])
  sigma <- rbind(sigma[1:4, ], c(NA, NA), c(NA, NA), sigma[5:15, ])
} else if (target == "2021") {
  beta <- rbind(beta[1:4, ], c(NA, NA), c(NA, NA), c(NA, NA), beta[5:14, ])
  sigma <- rbind(sigma[1:4, ], c(NA, NA), c(NA, NA), c(NA, NA), sigma[5:14, ])
} else {
  beta <- rbind(
    beta[1:4, ], c(NA, NA), c(NA, NA), c(NA, NA), beta[5:11, ],
    c(NA, NA), c(NA, NA), c(NA, NA)
  )
  sigma <- rbind(
    sigma[1:4, ], c(NA, NA), c(NA, NA), c(NA, NA), sigma[5:11, ],
    c(NA, NA), c(NA, NA), c(NA, NA)
  )
}

plot <- beta %>%
  mutate(
    type = type,
    class = class,
    classId = classId
  ) %>%
  gather("key", "value", -"type", -"class", -"classId") %>%
  mutate(
    sigma = sigma %>%
      gather() %>% .[, 2] %>% .$value
  ) %>%
  rename(beta = value) %>%
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
  coord_flip() +
  scale_x_discrete(limits = rev) +
  ylim(c(-2.8, 2.8)) +
  scale_color_discrete(guide = "none") +
  scale_shape_discrete(guide = "none") +
  labs(
    x = "Variable",
    y = "Coefficient"
  ) +
  facet_grid(
    cols = vars(key)
  )

ggsave(
  file.path("..", "output", "lr", paste0("plot_", target, ".png")),
  plot,
  width = 9,
  height = 3.3,
)
}

do_lr_remote("precovid")
do_lr_remote("2021")
do_lr_remote("2022")


read.csv(file.path("..", "output", "lr", "precovid.csv")) %>%
  tibble() %>%
  full_join(
    read.csv(file.path("..", "output", "lr", "2021.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "output", "lr", "2022.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  write.csv(
    file.path("..", "output", "lr", "remote.csv"),
    row.names = F,
    quote = F
  )

lr_data_remote %>%
  select(c06c_work_location_home_precovid, source) %>%
  filter(!is.na(c06c_work_location_home_precovid)) %>%
  filter(c06c_work_location_home_precovid > -1) %>%
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
  filter(c07c_work_location_home_2021 > -1) %>%
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
  filter(c08c_work_location_home_2022 > -1) %>%
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
