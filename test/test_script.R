rm(list = ls())
library(devtools)
library(haven)
library(openxlsx)
library(surveydata)
library(sjlabelled)
data("membersurvey") ## Funktioniert nur schlecht, da eigene Klasse
load_all()

# Load Data ----
pp <- read_sav("../../data_band/PP_mar18_final_tna_n=12971.sav") %>% as_factor()
pp2 <- data.table(pp)

## Data Preparation ----
pp2[, gewichte := sample(1:9, nrow(pp2), replace = TRUE)/10]

## Show Labels of data
label_tab <- lookup_fast(pp2, attribute_name = "labels", item_text = c("[", "]"))
out <- tableband_mc(pp2, row_vars = c("q1GEND", "attribute_4"),
                    col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"),
                    count_factor = "Yes", item_labels = T)



## Test Functions ----
### tableband_uni ----
out <- tableband_uni(pp2, vars = c("q1GEND", "q10KNOW_5"))
out <- tableband_uni(pp2, vars = c("q1GEND", "q10KNOW_5"), var_labels = F)

### tableband_bi ----
out <- tableband_bi(pp, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"))
out <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"), var_labels = F)
out <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"), summary = T, weights = "age")

out <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("attribute_4", "q10KNOW_4"), summary = T, weights = "age")

### tableband_mc ----
out <- tableband_mc(pp2, row_vars = c("q1GEND", "attribute_4"), col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"), count_factor = "Yes")

### tableband_mean ----
var_old <- paste0("q10KNOW_", c(1:3))
var_new <- paste0("n_", var_old)
pp2[, (var_old) := lapply(.SD, as.numeric), .SDcols = c(var_old)]
pp2 <- copy_labels(pp2, pp)

test <- tableband_mean(pp2, row_vars = c("q1GEND", "attribute_4"), col_var = c(var_old), summary = T, item_labels = T, var_labels = T)
test <- mean_bi(pp2, row_vars = c("q1GEND", "attribute_4"), col_var = c(var_old), summary = T)

test <- mean_bi_helper(pp2, row_vars = c("q1GEND", "attribute_4"), col_var = var_new[1])

### Performance Tests ----
system.time(out <- tableband_uni(pp2, vars = c("q1GEND", "q10KNOW_5") ))
system.time(out <- lookup_fast(pp2))
system.time(out <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4", "q10KNOW_1"), summary = T))
system.time(t <- tableband_mc(pp2, row_vars = c("q1GEND", "attribute_4"), col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"), count_factor = "Yes"))

### Sonstiges ----
