rm(list = ls())

library(dotenv)
library(haven)
library(tidyverse)
library(psych)
library(GPArotation)
library(coin)
library(stats)
library(exactRankTests)
library(ggraph)
library(tidygraph)
library(sampling)
library(car)
library(nnet)
library(anomaly)
library(ggtext)

# Run this entry point from the repository root; all paths below are relative to it.
# Data file locations come from .env (gitignored; see .env.example).
dotenv_path <- ".env"
if (!file.exists(dotenv_path)) {
  stop(".env not found in the repository root; see .env.example.")
}
load_dot_env(file = dotenv_path)
phase0_project_root <- normalizePath(
  dirname(dotenv_path),
  winslash = "/",
  mustWork = TRUE
)

# Fail loudly if a required variable is missing, and resolve relative paths
# against the .env directory so they do not depend on the R working directory.
for (data_var in c("DATA_2021_PATH", "DATA_MM_PATH")) {
  data_value <- Sys.getenv(data_var, unset = "")
  if (!nzchar(data_value)) {
    stop(paste0(
      data_var,
      " is not set. Define it in .env at the project root; see .env.example."
    ))
  }
  if (!grepl("^([A-Za-z]:[\\\\/]|/|\\\\\\\\)", data_value)) {
    data_value <- file.path(dirname(dotenv_path), data_value)
  }
  assignment <- list(normalizePath(data_value, mustWork = TRUE))
  names(assignment) <- data_var
  do.call(Sys.setenv, assignment)
}

source("./src/preparation/phase0_utils.R")

dir.create(phase0_output_path("lr"), recursive = TRUE, showWarnings = FALSE)
dir.create(phase0_output_path("test"), recursive = TRUE, showWarnings = FALSE)

source("./src/preparation/columns.R")
source("./src/preparation/socio_functions.R")
source("./src/preparation/data_import.R")
# source("./src/preparation/export_cs.R")
source("./src/analysis/01_stat.R")
source("./src/preparation/attitudinal_data.R")

source("./src/analysis/02_attitudinal.R")
source("./src/analysis/03_mwu_attitudinal.R")

source("./src/preparation/lr_data_create.R")
source("./src/analysis/04_lr_attitudinal.R")
source("./src/analysis/05_lr_remote.R")

source("./src/analysis/90_attitudinal_distribution.R")

phase0_finalize()
