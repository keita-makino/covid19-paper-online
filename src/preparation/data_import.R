mm <- read_sav("./data/mm.sav") %>%
  select(all_of(tag_list)) %>%
  rename_all(~tag_list_long) %>%
  mutate(source = "mm", state = "CA")

re <- read_sav("./data/altogether_cleaned.sav") %>%
  mutate(
    timer_too_short_attitudinal_1 = case_when(
      timer_08_page_submit < 30 ~ 1,
      T ~ 0
    ),
    timer_too_short_attitudinal_2 = case_when(
      timer_09_page_submit < 30 ~ 1,
      T ~ 0
    ),
    timer_too_short_attitudinal_3 = case_when(
      timer_04_page_submit < 30 ~ 1,
      T ~ 0
    ),
    timer_diff_too_short_attitudinal_1 = case_when(
      timer_08_page_submit - timer_08_first_click < 15 ~ 1,
      T ~ 0
    ),
    timer_diff_too_short_attitudinal_2 = case_when(
      timer_09_page_submit - timer_09_first_click < 15 ~ 1,
      T ~ 0
    ),
    timer_diff_too_short_attitudinal_3 = case_when(
      timer_04_page_submit - timer_04_first_click < 15 ~ 1,
      T ~ 0
    ),
    is_complete_flat_line_attitudinal_1 = case_when(
      a1a_like_new_things == a1b_schedule_impossible_pt &
        a1a_like_new_things == a1c_like_walking &
        a1a_like_new_things == a1d_want_own_car &
        a1a_like_new_things == a1e_exercise_important &
        a1a_like_new_things == a1f_raise_gas_price_for_environment &
        a1a_like_new_things == a1g_like_driving &
        a1a_like_new_things == a1h_reduce_impact_environment &
        a1a_like_new_things == a1i_congestion_problem ~ 1,
      T ~ 0
    ),
    is_complete_flat_line_attitudinal_2 = case_when(
      a1k_fine_rent_car == a1l_like_bike &
        a1k_fine_rent_car == a1m_prefer_spacious_home &
        a1k_fine_rent_car == a1n_car_as_just_a_way_to_move &
        a1k_fine_rent_car == a1o_rightofway_cars &
        a1k_fine_rent_car == a1p_prefer_being_driver &
        a1k_fine_rent_car == a1q_no_alternative_to_driving &
        a1k_fine_rent_car == a1r_like_idea_pt_as_transportation &
        a1k_fine_rent_car == a1s_like_latest_technology ~ 1,
      T ~ 0
    ),
    is_complete_flat_line_attitudinal_3 = case_when(
      a1t_reschedule_avoid_congestion == a1u_satified_transpo &
        a1t_reschedule_avoid_congestion == a1v_mixed_land_use_neighborhood &
        a1t_reschedule_avoid_congestion == a1x_wifi_essential &
        a1t_reschedule_avoid_congestion == a1y_pay_quickier_trip &
        a1t_reschedule_avoid_congestion == a1z_stretch_budget_for_new_things &
        a1t_reschedule_avoid_congestion == a1aa_increase_cost_pt &
        a1t_reschedule_avoid_congestion == a1ab_protected_bike &
        a1t_reschedule_avoid_congestion == a1ac_costly_parking &
        a1t_reschedule_avoid_congestion == a1ad_satisfied_life ~ 1,
      T ~ 0
    ),
  ) %>%
  mutate(if_ca_resident = case_when(
    grepl("ca", d07_residence_state %>% str_to_lower()) ~ 1,
    T ~ 0
  )) %>%
  filter(if_ca_resident == 1) %>%
  filter(!timer_too_short_attitudinal_1) %>%
  filter(!timer_too_short_attitudinal_2) %>%
  filter(!timer_too_short_attitudinal_3) %>%
  filter(!timer_diff_too_short_attitudinal_1) %>%
  filter(!timer_diff_too_short_attitudinal_2) %>%
  filter(!timer_diff_too_short_attitudinal_3) %>%
  filter(!is_complete_flat_line_attitudinal_1) %>%
  filter(!is_complete_flat_line_attitudinal_2) %>%
  filter(!is_complete_flat_line_attitudinal_3) %>%
  select(
    all_of(tag_list_long),
    meta_distributionchannel,
    state
  ) %>%
  rename(source = meta_distributionchannel)

al <-
  rbind(
    re %>% filter(
      source != "mail" &
        source != "-88888"
    ),
    mm
  ) %>%
  mutate(
    source = case_when(
      source == "convenience" ~ "cs",
      source == "longitudinal" ~ "lg",
      source == "convenience" ~ "cs",
      source == "mailed_online" ~ "mo",
      source == "opinion_panel" ~ "op",
      T ~ "mm",
    )
  ) %>%
  mutate(
    source = source %>%
      factor(levels = c(
        "op",
        "lg",
        "cs",
        "mm",
        "mo"
      ))
  ) %>%
  filter(
    state == "CA" | source == "mm" | source == "mo"
  )
