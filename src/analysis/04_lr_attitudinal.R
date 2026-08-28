# Map each surviving coefficient to its plot metadata by coefficient NAME,
# never by position (AGENTS.md known fragility #2). Stepwise selection drops
# different terms in different equations, so any positional splice produces
# correctly formatted, wrong values. Shared by 04_lr_attitudinal.R and
# 05_lr_remote.R; the multinomial logit plots pass the intercept through as
# "ASC", while the OLS plots exclude it before calling this function.
lr_attitudinal_term_metadata <- function(term) {
  type_by_variable <- c(
    "(Intercept)" = "ASC",
    "b01_yearborn" = "Age",
    "b05_gender" = "Gender",
    "b06_age_driver_license" = "DL",
    "b07_educational_background" = "Education",
    "b08_hh_income" = "Income",
    "d03_neighborhood_type" = "Neighborhood",
    "source" = "Source"
  )

  short_labels <- c(
    "Associate or college" = "College",
    "Graduate or professional" = "Graduate",
    "$50K - $99K" = "$50K-$99K",
    "$100K - $149K" = "$100K-$149K",
    "$150K or more" = "$150K+",
    "Town/Rural" = "Town/rural",
    "lg" = "Longitudinal",
    "cs" = "Convenience",
    "mm" = "Mail-back",
    "mo" = "Mail-online"
  )

  matched_variable <- character(length(term))
  matched_level <- character(length(term))

  for (i in seq_along(term)) {
    hits <- names(type_by_variable)[startsWith(term[i], names(type_by_variable))]
    if (length(hits) == 0) {
      stop(paste0("Unmapped coefficient term in lr_attitudinal plot: ", term[i]))
    }
    matched_variable[i] <- hits[1]
    # substring() is regex-safe; sub("^...", "") would break on "(Intercept)".
    matched_level[i] <- substring(term[i], nchar(hits[1]) + 1L)
  }

  tibble(
    term = term,
    type = unname(type_by_variable[matched_variable]),
    class = dplyr::case_when(
      # The intercept has no level suffix; label it after its type ("ASC").
      matched_level == "" ~ unname(type_by_variable[matched_variable]),
      matched_level %in% names(short_labels) ~
        unname(short_labels[matched_level]),
      TRUE ~ matched_level
    )
  ) %>%
    group_by(type) %>%
    mutate(classId = row_number() %>% as.character()) %>%
    ungroup()
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

  model_vif <- vif(model_lr)
  print(model_vif)
  # Enforce retention of the channel dummy: the lower scope keeps `source`
  # in every candidate model, so backward selection cannot drop it.
  step_scope <- list(upper = model_lr %>% formula())
  if (source_included) {
    step_scope$lower <- ~ source
  }

  stepped <- step(
    model_lr,
    scope = step_scope,
    direction = "backward",
    trace = 0
  )

  if (source_included) {
    phase0_assert_source_retained(
      stepped,
      paste0("lr_attitudinal_", target),
      "lm"
    )
    phase0_assert_source_level_retained(
      stepped,
      lr_data_attitudinal,
      paste0("lr_attitudinal_", target),
      "lm"
    )
  }

  output_suffix <- if (source_included) "_source" else ""

  phase0_write_csv(
    phase0_tidy_vif(model_vif, target, source_included),
    phase0_output_path("lr", paste0(target, output_suffix, "_vif_tidy.csv")),
    "lr_attitudinal",
    "Variance inflation factors for the initial full OLS model."
  )

  model_tidy <- phase0_tidy_lm(stepped, target, source_included)
  phase0_append_model_tidy(model_tidy)

  phase0_write_csv(
    model_tidy,
    phase0_output_path("lr", paste0(target, output_suffix, "_tidy.csv")),
    "lr_attitudinal",
    "Tidy OLS coefficients after backward stepwise selection."
  )

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

  coef_summary <- summary(stepped)$coefficients
  keep <- rownames(coef_summary) != "(Intercept)"

  metadata <- lr_attitudinal_term_metadata(rownames(coef_summary)[keep])

  type_levels <- c(
    "Age",
    "Gender",
    "DL",
    "Education",
    "Income",
    "Neighborhood"
  )

  if (source_included) {
    type_levels <- c(type_levels, "Source")
  }

  plot <- tibble(
    beta = coef_summary[keep, 1],
    sigma = coef_summary[keep, 2],
    type = metadata$type,
    class = metadata$class,
    classId = metadata$classId
  ) %>%
    mutate(
      type = factor(
        type,
        levels = type_levels
      )
    ) %>%
    ggplot() +
    geom_hline(yintercept = 0) +
    geom_errorbar(
      aes(
        x = type,
        ymin = beta - 1.96 * sigma,
        ymax = beta + 1.96 * sigma,
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
    # Clip at the panel boundary instead of dropping out-of-range rows:
    # scale limits would silently remove class labels outside +/-1.3.
    coord_flip(ylim = c(-1.3, 1.3)) +
    scale_x_discrete(limits = rev) +
    scale_color_discrete(guide = "none") +
    scale_shape_discrete(guide = "none") +
    labs(
      x = "Variable",
      y = "Coefficient"
    )

  if (source_included) {
    ggsave(
      phase0_output_path("lr", paste0("plot_", target, "_source.png")),
      plot,
      width = 7,
      height = 3.3,
    )
  } else {
    ggsave(
      phase0_output_path("lr", paste0("plot_", target, ".png")),
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

phase0_write_csv(
  phase0_model_tidy %>%
    filter(source_included) %>%
    arrange(target, term),
  phase0_output_path("lr", "attitudinal_source_models_tidy.csv"),
  "lr_attitudinal",
  "Combined tidy OLS coefficients for the source-included attitude models."
)
