phase0_write_csv(
  attitudinal_conceptual_exclusions,
  phase0_output_path("factor_conceptual_exclusions.csv"),
  "factor_analysis",
  "Conceptual item exclusions applied before the EFA loading-threshold loop."
)

get_factor <- function(name, n) {
  data <- get(
    name,
    envir = globalenv()
  ) %>%
    select(all_of(attitudinal_analysis_tag_list_long))
  stopifnot(!any(attitudinal_conceptual_exclusions$item %in% names(data)))
  is_done <- F
  rows <- seq_along(data)
  iteration <- 0

  while (!is_done) {
    data <- data[rows]
    iteration <- iteration + 1
    factor <- psych::fa(
      data,
      nfactors = n,
      rotate = "promax",
      scores = "tenBerge",
      max.iter = 2000,
      SMC = TRUE,
      warnings = TRUE,
      fm = "pa"
    )

    print(
      factor$Vaccounted %>%
        round(3) %>%
        .[1, ]
    )

    check_significance <- factor$loadings[] %>%
      tibble() %>%
      rowwise() %>%
      summarize(
        n = any(abs(.) > 0.3)
      ) %>%
      deframe()

    current_items <- colnames(data)
    kept_items <- current_items[check_significance]
    dropped_items <- current_items[!check_significance]
    phase0_log_factor_items(
      paste0(name, "_factor_", n),
      iteration,
      kept_items,
      dropped_items
    )

    if (all(check_significance)) {
      is_done <- T
    }

    rows <- check_significance %>% which()
  }

  ggplot(
    data.frame(
      col = 1:length(factor$e.values),
      value = factor$e.values,
      sum = cumsum(factor$e.values) * max(factor$e.values) / sum(factor$e.values)
    )
  ) +
    geom_point(aes(x = col, y = value)) +
    geom_line(aes(x = col, y = sum)) +
    scale_y_continuous(
      "Eigenvalue",
      sec.axis = sec_axis(~ . / max(factor$e.values), name = "Cumulative Share")
    ) +
    scale_x_continuous(
      "Factor No."
    ) +
    theme_bw()

  ggsave(
    phase0_output_path(paste0("scree_", name, "_", n, ".png")),
    width = 10, height = 5, unit = "in"
  )

  loadings_tidy <- factor$loadings[] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("item") %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(
      cols = -item,
      names_to = "factor",
      values_to = "loading"
    ) %>%
    dplyr::mutate(
      analysis = paste0(name, "_factor_", n),
      nfactors = n
    )

  eigenvalues_tidy <- tibble::tibble(
    analysis = paste0(name, "_factor_", n),
    nfactors = n,
    factor_no = seq_along(factor$e.values),
    eigenvalue = factor$e.values
  )

  vaccounted_tidy <- factor$Vaccounted %>%
    as.data.frame() %>%
    tibble::rownames_to_column("metric") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      analysis = paste0(name, "_factor_", n),
      nfactors = n
    )

  phase0_write_csv(
    loadings_tidy,
    phase0_output_path(paste0("factor_", name, "_", n, "_loadings_tidy.csv")),
    "factor_analysis",
    "EFA loadings in long form."
  )

  phase0_write_csv(
    eigenvalues_tidy,
    phase0_output_path(paste0("factor_", name, "_", n, "_eigenvalues.csv")),
    "factor_analysis",
    "EFA eigenvalues."
  )

  phase0_write_csv(
    vaccounted_tidy,
    phase0_output_path(paste0("factor_", name, "_", n, "_vaccounted.csv")),
    "factor_analysis",
    "EFA variance accounted summary."
  )

  assign(
    paste(
      name, "factor", n,
      sep = "_"
    ),
    factor,
    envir = globalenv()
  )
}

n <- 6
get_factor("al_attitudinal", n)
