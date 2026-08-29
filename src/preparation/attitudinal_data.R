get_attitudinal <- function(name) {
  data <- get(
    name,
    envir = globalenv()
  )
  return(
    data %>%
      select(
        all_of(attitudinal_analysis_tag_list_long), source
      ) %>%
      phase0_track("attitudinal_select_items", name) %>%
      filter(if_all(starts_with("a"), ~ !is.na(.))) %>%
      phase0_track("attitudinal_filter_missing", name) %>%
      filter(if_all(starts_with("a"), ~ . > 0)) %>%
      phase0_track("attitudinal_filter_nonpositive", name)
  )
}

al_attitudinal <- "al" %>% get_attitudinal()
