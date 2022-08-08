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
    file.path("..", "dist", "lr", paste0(target, ".csv")),
    quote = F
  )
}

do_lr_remote("precovid")
do_lr_remote("2021")
do_lr_remote("2022")


read.csv(file.path("..", "dist", "lr", "precovid.csv")) %>%
  tibble() %>%
  full_join(
    read.csv(file.path("..", "dist", "lr", "2021.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  full_join(
    read.csv(file.path("..", "dist", "lr", "2022.csv")) %>%
      tibble(),
    by = "X"
  ) %>%
  write.csv(
    file.path("..", "dist", "lr", "remote.csv"),
    row.names = F,
    quote = F
  )
