al_attitudinal_mwu <-
  al_attitudinal_factor_6$scores %>%
  as.data.frame() %>%
  tibble() %>%
  mutate(
    source = al_attitudinal$source
  )

for (i in 1:n) {
  pairwise.wilcox.test(
    al_attitudinal_mwu[, i] %>%
      deframe(),
    al_attitudinal_mwu[, n + 1] %>%
      deframe(),
    alternative = "l",
    p.adjust.method = "BH"
  ) %>%
    print()
  pairwise.wilcox.test(
    al_attitudinal_mwu[, i] %>%
      deframe(),
    al_attitudinal_mwu[, n + 1] %>%
      deframe(),
    alternative = "g",
    p.adjust.method = "BH"
  ) %>%
    print()
  print("--------------------------------------------")
}

al_attitudinal_mwu$source %>%
  table() 
