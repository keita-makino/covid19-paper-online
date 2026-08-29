# Data file locations come from environment variables loaded from .env in
# index.R: DATA_MM_PATH (mail-back .sav) and DATA_2021_PATH (2021 online .rds).

mm <- read_sav(Sys.getenv("DATA_MM_PATH")) %>%
  select(all_of(tag_list), gc) %>%
  rename_with(~tag_list_long, all_of(tag_list)) %>%
  mutate_at(
    vars(
      "a1o_rightofway_cars",
      "a1p_prefer_being_driver",
      "a1q_no_alternative_to_driving",
      "a1r_like_idea_pt_as_transportation",
      "a1s_like_latest_technology"
    ),
    ~ {
      case_when(
        . == 6 ~ 3,
        . == 7 ~ 4,
        . == 8 ~ 5,
        TRUE ~ as.numeric(.)
      )
    }
  ) %>%
  mutate(source = "mail_mail") %>%
  phase0_track("data_import_mm_raw", "mm") %>%
  filter(gc == 1) %>%
  phase0_track("data_import_mm_filter_gc", "mm") %>%
  filter(if_all(starts_with("a1"), ~ !is.na(.x) & .x != -99999)) %>%
  phase0_track("data_import_mm_filter_attitudinal_complete", "mm") %>%
  mutate(
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
        a1t_reschedule_avoid_congestion == a1ac_costly_parking ~ 1,
      T ~ 0
    )
  ) %>%
  phase0_track("data_import_mm_quality_flags", "mm") %>%
  filter(
    !is_complete_flat_line_attitudinal_1 &
      !is_complete_flat_line_attitudinal_2 &
      !is_complete_flat_line_attitudinal_3
  ) %>%
  phase0_track("data_import_mm_filter_flat_line", "mm") %>%
  select(
    all_of(analysis_tag_list_long),
    source
  )

re <- readRDS(Sys.getenv("DATA_2021_PATH")) %>%
  phase0_track("data_import_re_raw", "re") %>%
  mutate(
    # timer_too_short_attitudinal_1 = case_when(
    #   Timer_08_Page_Submit < 30 ~ 1,
    #   T ~ 0
    # ),
    # timer_too_short_attitudinal_2 = case_when(
    #   Timer_09_Page_Submit < 30 ~ 1,
    #   T ~ 0
    # ),
    # timer_too_short_attitudinal_3 = case_when(
    #   Timer_04_Page_Submit < 30 ~ 1,
    #   T ~ 0
    # ),
    # timer_diff_too_short_attitudinal_1 = case_when(
    #   Timer_08_Page_Submit - Timer_08_First_Click < 15 ~ 1,
    #   T ~ 0
    # ),
    # timer_diff_too_short_attitudinal_2 = case_when(
    #   Timer_09_Page_Submit - Timer_09_First_Click < 15 ~ 1,
    #   T ~ 0
    # ),
    # timer_diff_too_short_attitudinal_3 = case_when(
    #   Timer_04_Page_Submit - Timer_04_First_Click < 15 ~ 1,
    #   T ~ 0
    # ),
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
        a1t_reschedule_avoid_congestion == a1ac_costly_parking ~ 1,
      T ~ 0
    ),
  ) %>%
  phase0_track("data_import_re_quality_flags", "re") %>%
  filter(
    # !timer_too_short_attitudinal_1 &
    #   !timer_too_short_attitudinal_2 &
    #   !timer_too_short_attitudinal_3 &
    #   !timer_diff_too_short_attitudinal_1 &
    #   !timer_diff_too_short_attitudinal_2 &
    #   !timer_diff_too_short_attitudinal_3 &
    !is_complete_flat_line_attitudinal_1 &
      !is_complete_flat_line_attitudinal_2 &
      !is_complete_flat_line_attitudinal_3
  ) %>%
  phase0_track("data_import_re_filter_flat_line", "re") %>%
  select(
    all_of(analysis_tag_list_long),
    source = meta_distribution_channel
  )

al <-
  rbind(
    re,
    mm
  ) %>%
  phase0_track("data_import_al_rbind", "al") %>%
  mutate(
    source = case_when(
      source %in% c("convenience", "cs") ~ "cs",
      source %in% c("longitudinal", "lg") ~ "lg",
      source %in% c("mail_online", "mailed_online", "mo") ~ "mo",
      source %in% c("opinion_panel", "op") ~ "op",
      source %in% c("mail_mail", "mm") ~ "mm",
      T ~ NA_character_
    )
  ) %>%
  phase0_track("data_import_al_recode_source", "al") %>%
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
  phase0_track("data_import_al_factor_source", "al")

al %>%
  select(source) %>%
  table(useNA = "ifany")
