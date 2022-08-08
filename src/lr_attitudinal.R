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

do_lr_attitudinal <- function(target) {
  model_lr <- lm(
    paste0(
      target,
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
    data = lr_data_attitudinal
  )

  vif(model_lr) %>% print()
  stepped <- step(
    model_lr, model_lr %>% formula(),
    direction = "backward",
    trace = 0
  )

  coef <- attach_significance(
    stepped %>%
      summary() %>%
      coefficients() %>%
      .[, 1] %>%
      round(3) %>%
      format(nsmall = 3),
    stepped %>%
      summary() %>%
      coefficients() %>%
      .[, 4]
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

  write.csv(
    coef,
    file.path("..", "dist", "lr", paste0(target, ".csv")),
    quote = F
  )
}

do_lr_attitudinal("PA1")
do_lr_attitudinal("PA4")
do_lr_attitudinal("PA2")
do_lr_attitudinal("PA6")
do_lr_attitudinal("PA3")
do_lr_attitudinal("PA5")

read.csv(file.path("..", "dist", "lr", "PA1.csv")) %>%
  tibble() %>%
  full_join(
    read.csv(file.path("..", "dist", "lr", "PA4.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "dist", "lr", "PA2.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "dist", "lr", "PA6.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "dist", "lr", "PA3.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "dist", "lr", "PA5.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  write.csv(
    file.path("..", "dist", "lr", "attitudinal.csv"),
    row.names = F,
    quote = F
  )
