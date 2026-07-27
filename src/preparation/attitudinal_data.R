get_attitudinal <- function(name) {
  data <- get(
    name,
    envir = globalenv()
  )
  return(
    data %>%
      select(
        attitudinal_tag_list_long[1:27], source
      ) %>%
      filter(if_all(starts_with("a"), ~ !is.na(.))) %>%
      filter(if_all(starts_with("a"), ~ . > 0)) %>%
      mutate_at(vars(
        "a1o_rightofway_cars",
        "a1p_prefer_being_driver",
        "a1q_no_alternative_to_driving",
        "a1r_like_idea_pt_as_transportation",
        "a1s_like_latest_technology"
      ), ~ {
        case_when(
          . == 1 ~ 1,
          . == 2 ~ 2,
          . == 6 ~ 3,
          . == 7 ~ 4,
          . == 8 ~ 5
        )
      })
  )
}

al_attitudinal <- "al" %>% get_attitudinal()
