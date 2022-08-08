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

setwd("./src")

source("./columns.R")
source("./data_import.R")

source("./attitudinal.R")

source("./mwu_attitudinal.R")

source("./lr_data_create.R")
source("./lr_attitudinal.R")
source("./lr_remote.R")

source("./stat.R")
