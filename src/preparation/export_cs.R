read_sav(file.path("src", "data", "cs.sav")) %>%
  write.csv(phase0_output_path("cs.csv"))
