library(devtools)

library(tidyverse)
library(haven)
library(janitor)
library(openxlsx)
library(haven)
library(forcats)
library(data.table)

load_all(reset = T)

# Load Data ----
pp <- read_sav("../../data_band/PP_mar18_final_tna_n=12971.sav")
pp <- as_factor(pp)
pp2 <- data.table(pp)


test <- freq_univariate_helper2(pp2, var = c("q10KNOW_5"))
