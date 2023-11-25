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
pp <- data.table(pp)

## Data Preparation ----
pp[, gewichte := sample(1:9, nrow(pp), replace = TRUE)/10]
pp[, gend_num := as.numeric(q1GEND)]

## Show Labels of data
label_tab <- lookup_fast(pp, attribute_name = "labels", item_text = c("[", "]"))
out <- tableband_mc(pp, row_vars = c("q1GEND", "attribute_4"),
                    col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"),
                    count_factor = "Yes", item_labels = F)

## Test Functions ----
### tableband_uni ----
out <- tableband_uni(pp, vars = c("q1GEND", "q10KNOW_5"))
out <- tableband_uni(pp, vars = c("q1GEND", "q10KNOW_5"), var_labels = F)

### tableband_bi ----
out <- tableband_bi(pp, c("q1GEND", "gend_num", "q10KNOW_1", "attribute_4"), c("q10KNOW_5", "q10KNOW_4"))
out <- tableband_bi(pp, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"), var_labels = F)
out <- tableband_bi(pp, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"), summary = T, weights = "age")

out <- tableband_bi(pp, c("q1GEND", "q10KNOW_1"), c("attribute_4", "q10KNOW_4"), summary = T, weights = "age")

### tableband_mc ----
out <- tableband_mc(pp, row_vars = c("q1GEND", "attribute_4"), col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"), count_factor = "Yes")

### tableband_mean ----
var_old <- paste0("q10KNOW_", c(1:3))
pp2 <- copy(pp)
pp2[, (var_old) := lapply(.SD, as.numeric), .SDcols = c(var_old)]
pp2 <- copy_labels(pp2, pp)

test <- tableband_mean(pp2, row_vars = c("q1GEND", "attribute_4"), col_var = c(var_old), summary = T, item_labels = T, var_labels = T)
test <- mean_bi(pp2, row_vars = c("q1GEND", "attribute_4"), col_var = c(var_old), summary = T)

### Performance Tests ----
system.time(out <- tableband_uni(pp, vars = c("q1GEND", "q10KNOW_5") ))
system.time(out <- lookup_fast(pp))
system.time(out <- tableband_bi(pp, c("q1GEND"), c("q10KNOW_5", "q10KNOW_4", "q10KNOW_1"), summary = T))
system.time(t <- tableband_mc(pp, row_vars = c("q1GEND", "attribute_4"), col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"), count_factor = "Yes"))

### Sonstiges ----
