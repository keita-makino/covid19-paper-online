rm(list = ls())

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

setwd("./src")

dir.create(file.path("..", "output", "lr"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("..", "output", "test"), recursive = TRUE, showWarnings = FALSE)

source("./preparation/columns.R")
source("./preparation/data_import.R")
source("./preparation/export_cs.R")
source("./preparation/attitudinal_data.R")

source("./analysis/attitudinal.R")

source("./analysis/mwu_attitudinal.R")

source("./preparation/lr_data_create.R")
source("./analysis/lr_attitudinal.R")
source("./analysis/lr_remote.R")

source("./analysis/stat.R")
source("./analysis/attitudinal_distribution.R")
