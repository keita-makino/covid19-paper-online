phase0_seed <- 20260727
phase0_attitudinal_scenario <- "S2_conceptual_exclusions"
set.seed(phase0_seed)

if (!exists("phase0_project_root", envir = .GlobalEnv, inherits = FALSE)) {
  phase0_project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
}

phase0_attrition_log <- tibble::tibble(
  step = character(),
  dataset = character(),
  unit = character(),
  source = character(),
  n = integer()
)

phase0_manifest <- tibble::tibble(
  section = character(),
  path = character(),
  description = character()
)

phase0_model_tidy <- tibble::tibble()

phase0_factor_item_log <- tibble::tibble(
  analysis = character(),
  iteration = integer(),
  kept_items = character(),
  dropped_items = character()
)

phase0_condition_log <- tibble::tibble(
  dataset = character(),
  source = character(),
  condition = character(),
  n_fail_marginal = integer(),
  n_fail_first = integer(),
  n_enter = integer(),
  n_drop_any = integer(),
  n_pass = integer()
)

phase0_output_path <- function(...) {
  file.path(phase0_project_root, "output", ...)
}

phase0_output_context <- function(section) {
  contexts <- list(
    factor_analysis = list(
      scenario = phase0_attitudinal_scenario,
      specification = "exploratory_factor_analysis_6_factor_promax_pa"
    ),
    mwu_attitudinal = list(
      scenario = phase0_attitudinal_scenario,
      specification = "pairwise_wilcoxon_BH_current"
    ),
    lr_attitudinal = list(
      scenario = phase0_attitudinal_scenario,
      specification = "backward_stepwise_ols_current"
    ),
    attitudinal_distribution = list(
      scenario = "not_applicable_pre_factor_distribution",
      specification = "attitudinal_response_distribution_current"
    ),
    lr_remote = list(
      scenario = "not_applicable_factor_independent",
      specification = "backward_stepwise_multinomial_logit_current"
    ),
    stat = list(
      scenario = "not_applicable",
      specification = "sociodemographic_distribution_current"
    ),
    attrition = list(
      scenario = "not_applicable",
      specification = "phase0_tracking_current"
    )
  )

  context <- contexts[[section]]
  if (is.null(context)) {
    context <- list(
      scenario = "not_applicable",
      specification = "current"
    )
  }
  context
}

phase0_escape_latex <- function(values) {
  escaped <- ifelse(is.na(values), "", as.character(values))
  escaped <- gsub("\\", "PHASEZEROBACKSLASH", escaped, fixed = TRUE)

  replacements <- c(
    "&" = "\\&",
    "%" = "\\%",
    "$" = "\\$",
    "#" = "\\#",
    "_" = "\\_",
    "{" = "\\{",
    "}" = "\\}",
    "~" = "\\textasciitilde{}",
    "^" = "\\textasciicircum{}"
  )
  for (character in names(replacements)) {
    escaped <- gsub(character, replacements[[character]], escaped, fixed = TRUE)
  }

  gsub(
    "PHASEZEROBACKSLASH",
    "\\textbackslash{}",
    escaped,
    fixed = TRUE
  )
}

phase0_latex_table_lines <- function(data) {
  escaped_header <- phase0_escape_latex(names(data))
  escaped_data <- as.data.frame(
    lapply(data, phase0_escape_latex),
    stringsAsFactors = FALSE
  )
  body <- if (nrow(escaped_data) == 0) {
    character()
  } else {
    apply(escaped_data, 1, function(row) {
      paste0(paste(row, collapse = " & "), " \\\\")
    })
  }

  c(
    paste0("\\begin{tabular}{", paste(rep("l", ncol(data)), collapse = ""), "}"),
    "\\hline",
    paste0(paste(escaped_header, collapse = " & "), " \\\\") ,
    "\\hline",
    body,
    "\\hline",
    "\\end{tabular}"
  )
}

phase0_write_latex_table <- function(data, path) {
  writeLines(phase0_latex_table_lines(data), path)
  invisible(path)
}

phase0_source_column <- function(data) {
  if ("source" %in% names(data)) {
    return("source")
  }
  if ("meta_distribution_channel" %in% names(data)) {
    return("meta_distribution_channel")
  }
  if ("meta_distributionchannel" %in% names(data)) {
    return("meta_distributionchannel")
  }
  NA_character_
}

phase0_log_attrition <- function(data, step, dataset, unit = "respondents") {
  source_column <- phase0_source_column(data)
  attrition_log <- get("phase0_attrition_log", envir = .GlobalEnv)

  if (is.na(source_column)) {
    counts <- tibble::tibble(source = "ALL", n = nrow(data))
  } else {
    counts <- data %>%
      dplyr::count(.data[[source_column]], name = "n") %>%
      dplyr::mutate(
        source = as.character(.data[[source_column]]),
        source = dplyr::coalesce(source, "source_NA")
      ) %>%
      dplyr::select(source, n)
  }

  known_sources <- unique(
    attrition_log$source[
      attrition_log$dataset == dataset & attrition_log$unit == unit
    ]
  )
  counts <- tibble::tibble(source = union(known_sources, counts$source)) %>%
    dplyr::left_join(counts, by = "source") %>%
    dplyr::mutate(n = dplyr::coalesce(n, 0L))

  entry <- tibble::tibble(
    step = step,
    dataset = dataset,
    unit = unit,
    source = counts$source,
    n = counts$n
  )

  assign(
    "phase0_attrition_log",
    dplyr::bind_rows(
      attrition_log,
      entry
    ),
    envir = .GlobalEnv
  )

  invisible(data)
}

phase0_track <- function(data, step, dataset, unit = "respondents") {
  phase0_log_attrition(data, step, dataset, unit)
  data
}

# Maps input channel labels to the canonical labels used after the source
# recode in data_import.R. Unmapped online labels are retained until recoding,
# then appear as source_NA in the attrition log.
phase0_normalize_channel <- function(source_labels) {
  source_labels %>%
    dplyr::recode(
      "convenience" = "cs",
      "longitudinal" = "lg",
      "mail_online" = "mo",
      "mailed_online" = "mo",
      "opinion_panel" = "op",
      "mail_mail" = "mm",
      "mail" = "mail_unmapped_at_recode",
      "-88888" = "unknown_unmapped_at_recode"
    )
}

# Per-condition attrition breakdown for a multi-condition filter such as
# filter_by_socio(). For each channel and each condition, reports:
#   n_fail_marginal - rows failing this condition alone (NA counts as fail,
#                     matching dplyr::filter semantics); overlaps allowed.
#   n_fail_first    - sequential first-fail attribution in the order the
#                     conditions are listed; sums exactly to n_drop_any.
# The data are not modified; the function only logs and returns them.
phase0_log_condition_attrition <- function(data, dataset, conditions) {
  source_column <- phase0_source_column(data)
  sources <- if (is.na(source_column)) {
    rep("ALL", nrow(data))
  } else {
    as.character(data[[source_column]])
  }
  sources[is.na(sources)] <- "source_NA"

  evaluated <- lapply(conditions, function(condition) {
    rlang::eval_tidy(condition, data = data)
  })

  fails <- lapply(evaluated, function(x) is.na(x) | !x)

  alive <- rep(TRUE, nrow(data))
  first_fail <- list()
  for (condition_name in names(conditions)) {
    first_fail[[condition_name]] <- alive & fails[[condition_name]]
    alive <- alive & !fails[[condition_name]]
  }

  channels <- sort(unique(sources))
  count_by <- function(v) as.integer(tapply(v, sources, sum)[channels])
  n_enter <- as.integer(table(sources)[channels])
  n_pass <- count_by(alive)
  n_drop_any <- n_enter - n_pass

  entries <- lapply(names(conditions), function(condition_name) {
    tibble::tibble(
      dataset = dataset,
      source = channels,
      condition = condition_name,
      n_fail_marginal = count_by(fails[[condition_name]]),
      n_fail_first = count_by(first_fail[[condition_name]]),
      n_enter = n_enter,
      n_drop_any = n_drop_any,
      n_pass = n_pass
    )
  })

  total_entry <- tibble::tibble(
    dataset = dataset,
    source = channels,
    condition = "(total)",
    n_fail_marginal = NA_integer_,
    n_fail_first = NA_integer_,
    n_enter = n_enter,
    n_drop_any = n_drop_any,
    n_pass = n_pass
  )

  assign(
    "phase0_condition_log",
    dplyr::bind_rows(
      get("phase0_condition_log", envir = .GlobalEnv),
      dplyr::bind_rows(entries),
      total_entry
    ),
    envir = .GlobalEnv
  )

  data
}

# Canonical stage boundaries of the pipeline. `step` must match a tracked
# step name in data_import.R / attitudinal_data.R / lr_data_create.R /
# 05_lr_remote.R; phase0_build_attrition_matrix() fails loudly if a step
# is renamed without updating this map.
phase0_stage_map <- tibble::tribble(
  ~stage, ~step, ~dataset, ~label, ~applies_to,
  "0", "data_import_mm_raw", "mm",
  "Input rows (mm.sav / online .rds)",
  "mm after variable selection; online RDS source labels are normalized below",
  "0", "data_import_re_raw", "re",
  "Input rows (mm.sav / online .rds)",
  "mm after variable selection; online RDS source labels are normalized below",
  "1", "data_import_mm_filter_gc", "mm",
  "Mail-back gc filter",
  "mail-back only",
  "2", "data_import_mm_filter_attitudinal_complete", "mm",
  "Mail-back attitudinal completeness filter, including attention checks",
  "mail-back only; attention checks and conceptual exclusions are excluded from the 25-item analysis set",
  "3", "data_import_mm_filter_flat_line", "mm",
  "Straight-line screen",
  "mail-back and online",
  "3", "data_import_re_filter_flat_line", "re",
  "Straight-line screen",
  "mail-back and online",
  "4", "data_import_al_factor_source", "al",
  "rbind + source recode",
  "all channels",
  "5_na", "attitudinal_filter_missing", "al",
  "Attitudinal complete-case: NA removed",
  "all channels",
  "5_nonpositive", "attitudinal_filter_nonpositive", "al",
  "Attitudinal complete-case: values <= 0 removed",
  "all channels",
  "5_mwu_initial", "mwu_attitudinal_initial", "al_attitudinal_mwu",
  "Mann-Whitney sample before source filter",
  "all channels",
  "5_mwu_source", "mwu_attitudinal_filter_missing_source", "al_attitudinal_mwu",
  "Mann-Whitney missing-source filter",
  "unmapped source_NA rows only",
  "6_attitudinal_initial", "lr_data_attitudinal_cbind_factor_scores", "lr_data_attitudinal",
  "Attitude regression sample before source/socio filters",
  "all channels",
  "6_attitudinal_source", "lr_data_attitudinal_filter_missing_source", "lr_data_attitudinal",
  "Attitude regression missing-source filter",
  "unmapped source_NA rows only",
  "6_attitudinal", "lr_data_attitudinal_filter_by_socio", "lr_data_attitudinal",
  "Socio complete-case (attitude regression sample)",
  "all channels",
  "6_remote_initial", "lr_data_remote_initial", "lr_data_remote",
  "Remote MNL sample before socio complete-case",
  "all channels",
  "6_remote", "lr_data_remote_filter_by_socio", "lr_data_remote",
  "Socio complete-case (remote-work MNL base sample)",
  "all channels",
  "6_remote_source", "lr_data_remote_mutate_work_status", "lr_data_remote",
  "Remote outcome and source factorization",
  "remote-work MNL only; op/lg/cs/mm/mo are retained",
  "7_precovid_full_time", "lr_remote_precovid_filter_full_time", "lr_data_remote",
  "Full-time employment filter, precovid MNL sample",
  "remote-work MNL only",
  "7_precovid_non_worker", "lr_remote_precovid_filter_non_worker", "lr_data_remote",
  "Non-worker filter, precovid MNL sample",
  "remote-work MNL only",
  "7_precovid_source", "lr_remote_precovid_filter_missing_source", "lr_data_remote",
  "Missing-source filter, precovid MNL sample",
  "remote-work MNL only; unmapped source_NA records are excluded before multinom",
  "7_2021_full_time", "lr_remote_2021_filter_full_time", "lr_data_remote",
  "Full-time employment filter, 2021 MNL sample",
  "remote-work MNL only",
  "7_2021_non_worker", "lr_remote_2021_filter_non_worker", "lr_data_remote",
  "Non-worker filter, 2021 MNL sample",
  "remote-work MNL only",
  "7_2021_source", "lr_remote_2021_filter_missing_source", "lr_data_remote",
  "Missing-source filter, 2021 MNL sample",
  "remote-work MNL only; unmapped source_NA records are excluded before multinom",
  "7_2022_full_time", "lr_remote_2022_filter_full_time", "lr_data_remote",
  "Full-time employment filter, 2022 MNL sample",
  "remote-work MNL only",
  "7_2022_non_worker", "lr_remote_2022_filter_non_worker", "lr_data_remote",
  "Non-worker filter, 2022 MNL sample",
  "remote-work MNL only",
  "7_2022_source", "lr_remote_2022_filter_missing_source", "lr_data_remote",
  "Missing-source filter, 2022 MNL sample",
  "remote-work MNL only; unmapped source_NA records are excluded before multinom"
)

# Reshapes the attrition log into a stage-by-channel matrix. Empty cells
# mean the stage does not apply to that channel, never zero.
phase0_build_attrition_matrix <- function() {
  attrition_log <- get("phase0_attrition_log", envir = .GlobalEnv)

  missing_steps <- phase0_stage_map %>%
    dplyr::anti_join(attrition_log, by = c("step", "dataset"))

  if (nrow(missing_steps) > 0) {
    stop(paste0(
      "phase0_stage_map references steps missing from the attrition log: ",
      paste(missing_steps$step, collapse = ", ")
    ))
  }

  counts <- attrition_log %>%
    dplyr::inner_join(phase0_stage_map, by = c("step", "dataset")) %>%
    dplyr::mutate(
      channel = phase0_normalize_channel(source),
      channel = ifelse(is.na(channel), "source_NA", channel)
    )

  known_channels <- c(
    "op", "lg", "cs", "mm", "mo",
    "mail_unmapped_at_recode", "unknown_unmapped_at_recode", "source_NA"
  )
  unknown_channels <- setdiff(unique(counts$channel), known_channels)
  if (length(unknown_channels) > 0) {
    stop(paste0(
      "Unknown recruitment channel(s) in the attrition log: ",
      paste(unknown_channels, collapse = ", ")
    ))
  }
  channel_order <- intersect(known_channels, unique(counts$channel))

  counts %>%
    dplyr::group_by(stage, label, applies_to, channel) %>%
    dplyr::summarise(n = sum(n), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = channel, values_from = n) %>%
    dplyr::relocate(dplyr::all_of(channel_order), .after = applies_to) %>%
    dplyr::mutate(
      total = rowSums(
        dplyr::across(dplyr::all_of(channel_order)),
        na.rm = TRUE
      )
    ) %>%
    dplyr::arrange(match(stage, unique(phase0_stage_map$stage)))
}

phase0_write_csv <- function(data, path, section, description) {
  context <- phase0_output_context(section)
  if (!("scenario" %in% names(data))) {
    data <- dplyr::mutate(data, scenario = context$scenario, .before = 1)
  }
  if (!("specification" %in% names(data))) {
    data <- dplyr::mutate(
      data,
      specification = context$specification,
      .before = 2
    )
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(data, path, row.names = FALSE)
  latex_path <- sub("\\.csv$", ".tex", path, ignore.case = TRUE)
  phase0_write_latex_table(data, latex_path)

  manifest_entry <- tibble::tibble(
    section = c(section, section),
    path = c(path, latex_path),
    description = c(
      description,
      paste0("LaTeX table fragment: ", description)
    )
  )

  assign(
    "phase0_manifest",
    dplyr::bind_rows(
      get("phase0_manifest", envir = .GlobalEnv),
      manifest_entry
    ),
    envir = .GlobalEnv
  )

  invisible(data)
}

phase0_tidy_vif <- function(vif_object, target, source_included) {
  if (is.null(dim(vif_object))) {
    tidy <- tibble::tibble(
      term = names(vif_object),
      vif = as.numeric(vif_object)
    )
  } else {
    tidy <- vif_object %>%
      as.data.frame() %>%
      tibble::rownames_to_column("term") %>%
      tibble::as_tibble()
  }

  tidy %>%
    dplyr::mutate(
      target = target,
      source_included = source_included
    )
}

phase0_tidy_lm <- function(model, target, source_included) {
  model_summary <- summary(model)
  coefficients <- model_summary$coefficients

  tibble::tibble(
    scenario = phase0_attitudinal_scenario,
    specification = if (source_included) {
      "backward_stepwise_ols_source_included"
    } else {
      "backward_stepwise_ols_source_excluded"
    },
    target = target,
    source_included = source_included,
    term = rownames(coefficients),
    estimate = coefficients[, 1],
    std.error = coefficients[, 2],
    statistic = coefficients[, 3],
    p.value = coefficients[, 4],
    n = stats::nobs(model),
    r.squared = model_summary$r.squared,
    adj.r.squared = model_summary$adj.r.squared
  )
}

phase0_tidy_multinom <- function(model, target, null_model) {
  coefficients <- coef(model)
  standard_errors <- summary(model)$standard.errors
  statistics <- coefficients / standard_errors
  p_values <- (1 - stats::pnorm(abs(statistics), 0, 1)) * 2

  matrix_to_tidy <- function(values, value_name) {
    values %>%
      as.data.frame() %>%
      tibble::rownames_to_column("outcome") %>%
      tibble::as_tibble() %>%
      tidyr::pivot_longer(
        cols = -outcome,
        names_to = "term",
        values_to = value_name
      )
  }

  matrix_to_tidy(coefficients, "estimate") %>%
    dplyr::left_join(
      matrix_to_tidy(standard_errors, "std.error"),
      by = c("outcome", "term")
    ) %>%
    dplyr::left_join(
      matrix_to_tidy(statistics, "statistic"),
      by = c("outcome", "term")
    ) %>%
    dplyr::left_join(
      matrix_to_tidy(p_values, "p.value"),
      by = c("outcome", "term")
    ) %>%
    dplyr::mutate(
      scenario = "not_applicable_factor_independent",
      specification = "backward_stepwise_multinomial_logit_current",
      target = target,
      n = NROW(model$fitted.values),
      edf = model$edf,
      logLik = as.numeric(stats::logLik(model)),
      null_logLik = as.numeric(stats::logLik(null_model)),
      r2_mcfadden = 1 - as.numeric(stats::logLik(model)) / as.numeric(stats::logLik(null_model)),
      r2_mcfadden_adj = 1 -
        (as.numeric(stats::logLik(model)) - model$edf + null_model$edf - 1) /
        as.numeric(stats::logLik(null_model))
    ) %>%
    dplyr::select(
      scenario,
      specification,
      target,
      outcome,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      n,
      edf,
      logLik,
      null_logLik,
      r2_mcfadden,
      r2_mcfadden_adj
    )
}

phase0_append_model_tidy <- function(tidy) {
  assign(
    "phase0_model_tidy",
    dplyr::bind_rows(
      get("phase0_model_tidy", envir = .GlobalEnv),
      tidy
    ),
    envir = .GlobalEnv
  )

  invisible(tidy)
}

phase0_assert_source_retained <- function(model, context, model_type = c("lm", "multinom")) {
  model_type <- match.arg(model_type)

  terms <- if (model_type == "lm") {
    rownames(summary(model)$coefficients)
  } else {
    colnames(coef(model))
  }

  if (!any(grepl("^source", terms))) {
    stop(
      paste0(
        "source was not retained in ",
        context,
        "; stepwise selection must not drop the channel dummy."
      )
    )
  }

  invisible(model)
}

phase0_assert_source_level_retained <- function(
  model,
  data,
  context,
  model_type = c("lm", "multinom"),
  level = "cs"
) {
  model_type <- match.arg(model_type)
  source_levels <- levels(droplevels(data$source))

  if (!(level %in% source_levels) || identical(level, source_levels[1])) {
    return(invisible(model))
  }

  terms <- if (model_type == "lm") {
    rownames(summary(model)$coefficients)
  } else {
    colnames(coef(model))
  }
  required_term <- paste0("source", level)

  if (!(required_term %in% terms)) {
    stop(paste0(
      required_term,
      " was not retained in ",
      context,
      "; the observed convenience channel must remain in the model."
    ))
  }

  invisible(model)
}

phase0_log_factor_items <- function(analysis, iteration, kept_items, dropped_items) {
  entry <- tibble::tibble(
    analysis = analysis,
    iteration = iteration,
    kept_items = paste(kept_items, collapse = ";"),
    dropped_items = paste(dropped_items, collapse = ";")
  )

  assign(
    "phase0_factor_item_log",
    dplyr::bind_rows(
      get("phase0_factor_item_log", envir = .GlobalEnv),
      entry
    ),
    envir = .GlobalEnv
  )

  invisible(entry)
}

phase0_finalize <- function() {
  phase0_write_csv(
    get("phase0_attrition_log", envir = .GlobalEnv),
    phase0_output_path("attrition_by_step.csv"),
    "attrition",
    "Count by recruitment channel after each tracked filtering step; the unit column distinguishes respondents from item responses."
  )

  phase0_write_csv(
    get("phase0_condition_log", envir = .GlobalEnv),
    phase0_output_path("attrition_condition_breakdown.csv"),
    "attrition",
    "Per-condition breakdown of the socio complete-case filter: marginal failures and sequential first-fail attribution by channel."
  )

  attrition_matrix <- phase0_build_attrition_matrix()

  phase0_write_csv(
    attrition_matrix,
    phase0_output_path("attrition_matrix.csv"),
    "attrition",
    "Stage-by-channel sample-size matrix; empty cells mean the stage does not apply to that channel."
  )

  phase0_write_csv(
    get("phase0_factor_item_log", envir = .GlobalEnv),
    phase0_output_path("factor_item_iterations.csv"),
    "factor_analysis",
    "Items kept and dropped at each automatic loading-threshold iteration."
  )

  phase0_write_csv(
    get("phase0_model_tidy", envir = .GlobalEnv),
    phase0_output_path("model_tidy_all.csv"),
    "models",
    "Tidy model coefficients collected from instrumented model scripts."
  )

  manifest <- get("phase0_manifest", envir = .GlobalEnv) %>%
    dplyr::arrange(section, path)
  manifest_paths <- substring(
    manifest$path,
    nchar(phase0_output_path()) + 2L
  )

  session_lines <- capture.output(utils::sessionInfo())

  matrix_md <- {
    header <- paste0(
      "| ",
      paste(names(attrition_matrix), collapse = " | "),
      " |"
    )
    separator <- paste0(
      "|",
      paste(rep("---", ncol(attrition_matrix)), collapse = "|"),
      "|"
    )
    body <- apply(attrition_matrix, 1, function(row) {
      paste0("| ", paste(ifelse(is.na(row), "", row), collapse = " | "), " |")
    })
    c(header, separator, body)
  }

  results_lines <- c(
    "# Results",
    "",
    "Generated by the Phase 0 reproducibility scaffold. Numeric outputs are in the tidy CSV files listed below.",
    "Factor scores are standardized to mean zero within the analysis sample; only within-sample relative comparisons are valid.",
    "",
    "## Run metadata",
    "",
    paste0("- seed: ", get("phase0_seed", envir = .GlobalEnv)),
    paste0("- attitudinal scenario: ", get("phase0_attitudinal_scenario", envir = .GlobalEnv)),
    "- attitude model specification: backward_stepwise_ols_current",
    "- remote model scenario: not_applicable_factor_independent",
    "- remote model specification: backward_stepwise_multinomial_logit_current",
    paste0("- R: ", R.version.string),
    "",
    "## Attrition",
    "",
    "See `attrition_by_step.csv` (every tracked step; its unit column distinguishes respondents from item responses), `attrition_condition_breakdown.csv` (per-condition socio-filter breakdown), and the stage-by-channel matrix below (also `attrition_matrix.csv`). Empty cells mean the stage does not apply to that channel, never zero.",
    "",
    "Notes:",
    "- Stage 0 counts rows after input variable selection. The online input is an RDS file and mail-back input is mm.sav.",
    "- Stages 1 and 2 apply only to mail-back: gc, then attitudinal-completeness filtering. Stage 3 applies the straight-line screen to both inputs.",
    "- Stage 4 combines both inputs and recodes source. Online labels not mapped by the current recode become `source_NA`; no source/state filter is applied there.",
    "- Stage 5 is split into the NA-removal step and the <= 0-removal step, followed by the Mann-Whitney missing-source filter. Stage 6 reports the attitude regression and remote-work MNL samples before their source/socio filters, followed by remote outcome/source factorization.",
    "- Stage 7 is reported by time point for the full-time, non-worker, and missing-source filters. The missing-source filter makes the current multinomial complete-case exclusion explicit.",
    "",
    matrix_md,
    "",
    "## Output manifest",
    "",
    "| section | path | description |",
    "|---|---|---|",
    paste0(
      "| ",
      manifest$section,
      " | `",
      manifest_paths,
      "` | ",
      manifest$description,
      " |"
    ),
    "",
    "## Session info",
    "",
    "```",
    session_lines,
    "```"
  )

  writeLines(results_lines, phase0_output_path("results.md"))

  invisible(manifest)
}
