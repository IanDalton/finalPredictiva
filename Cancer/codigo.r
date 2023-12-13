setwd("~/development/Github/finalPredictiva/Cancer")
df <- read.csv(file = "metadata.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(tidyverse)