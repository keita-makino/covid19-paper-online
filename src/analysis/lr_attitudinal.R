attach_significance <- function(coef, p) {
  for (i in 1:length(p)) {
    if (coef[i] > 0) {
      coef[i] <- paste0("  ", coef[i])
    } else {
      coef[i] <- paste0(" ", coef[i])
    }
    if (p[i] < 0.10 & p[i] >= 0.05) {
      coef[i] <- paste0("/", coef[i])
    }
    if (p[i] < 0.05 & p[i] >= 0.01) {
      coef[i] <- paste0("*", coef[i])
    }
    if (p[i] < 0.01) {
      coef[i] <- paste0("**", coef[i])
    }
  }
  return(coef)
}

do_lr_attitudinal <- function(target, source_included = T) {
  eq <- paste0(
    target,
    " ~ ",
    "b01_yearborn +",
    "b05_gender +",
    "b06_age_driver_license +",
    "b07_educational_background +",
    "b08_hh_income +",
    "d03_neighborhood_type"
  )

  if (source_included) {
    eq <- paste0(eq, " + source")
  }

  model_lr <- lm(
    eq %>%
      as.formula(),
    data = lr_data_attitudinal
  )

  vif(model_lr) %>% print()
  stepped <- step(
    model_lr, model_lr %>% formula(),
    direction = "backward",
    trace = 0
  )

  beta <- stepped %>%
    summary() %>%
    coefficients() %>%
    .[, 1] %>%
    round(3) %>%
    format(nsmall = 3)

  sigma <- stepped %>%
    summary() %>%
    coefficients() %>%
    .[, 2]

  p <- stepped %>%
    summary() %>%
    coefficients() %>%
    .[, 4]

  coef <- attach_significance(
    beta,
    p
  ) %>%
    as.data.frame()

  lr_data_attitudinal$source %>%
    table() %>%
    print()

  summary(stepped)$r.squared %>%
    round(3) %>%
    format(nsmall = 3) %>%
    print()
  summary(stepped)$adj.r.squared %>%
    round(3) %>%
    format(nsmall = 3) %>%
    print()

  if (source_included) {
    write.csv(
      coef,
      file.path("..", "output", "lr", paste0(target, "_source.csv")),
      quote = F
    )
  } else {
    write.csv(
      coef,
      file.path("..", "output", "lr", paste0(target, ".csv")),
      quote = F
    )
  }

  if (target == "PA2" && source_included) {
    beta <- c(beta[1:7], NA, NA, beta[10:14], NA, NA, NA, NA)
    sigma <- c(sigma[1:7], NA, NA, sigma[10:14], NA, NA, NA, NA)
  }

  if (target == "PA3") {
    beta <- c(beta[1:4], NA, NA, beta[5:16])
    sigma <- c(sigma[1:4], NA, NA, sigma[5:16])
  }

  type <- c(
    "Age", "Age", "Age",
    "Gender", "Gender",
    "DL",
    "Education", "Education",
    "Income", "Income", "Income",
    "Neighborhood", "Neighborhood"
  )

  class <- c(
    "35-49", "50-64", "65+",
    "Male", "Non-binary",
    "Yes",
    "College", "Graduate",
    "$50K-$99K", "$100K-$149K", "$150K+",
    "Suburban", "Town/rural"
  )

  classId <- c(
    "1", "2", "3",
    "1", "2",
    "1",
    "1", "2",
    "1", "2", "3",
    "1", "2"
  )

  levels <- c(
    "Age",
    "Gender",
    "DL",
    "Education",
    "Income",
    "Neighborhood"
  )

  if (source_included) {
    type <- c(type, "Source", "Source", "Source", "Source")
    class <- c(class, "Longitudinal", "Convenience", "Mail-back", "Mail-online")
    classId <- c(classId, "1", "2", "3", "4")
    levels <- c(levels, "Source")
  }

  plot <- tibble(
    beta = beta[-1] %>% as.numeric(),
    sigma = sigma[-1] %>% as.numeric(),
    type = type,
    class = class,
    classId = classId
  ) %>%
    mutate(
      type = factor(
        type,
        levels = levels
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
    coord_flip() +
    scale_x_discrete(limits = rev) +
    ylim(c(-1.3, 1.3)) +
    scale_color_discrete(guide = "none") +
    scale_shape_discrete(guide = "none") +
    labs(
      x = "Variable",
      y = "Coefficient"
    )

  if (source_included) {
    ggsave(
      file.path("..", "output", "lr", paste0("plot_", target, "_source.png")),
      plot,
      width = 7,
      height = 3.3,
    )
  } else {
    ggsave(
      file.path("..", "output", "lr", paste0("plot_", target, "_source.png")),
      plot,
      width = 7,
      height = 3.3,
    )
  }
}

do_lr_attitudinal("PA4")
do_lr_attitudinal("PA1")
do_lr_attitudinal("PA2")
do_lr_attitudinal("PA6")
do_lr_attitudinal("PA3")
do_lr_attitudinal("PA5")

do_lr_attitudinal("PA4", F)
do_lr_attitudinal("PA1", F)
do_lr_attitudinal("PA2", F)
do_lr_attitudinal("PA6", F)
do_lr_attitudinal("PA3", F)
do_lr_attitudinal("PA5", F)

read.csv(file.path("..", "output", "lr", "PA4_source.csv")) %>%
  tibble() %>%
  full_join(
    read.csv(file.path("..", "output", "lr", "PA1_source.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "output", "lr", "PA2_source.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "output", "lr", "PA6_source.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "output", "lr", "PA3_source.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "output", "lr", "PA5_source.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  write.csv(
    file.path("..", "output", "lr", "attitudinal.csv"),
    row.names = F,
    quote = F
  )
