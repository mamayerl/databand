library(devtools)

library(haven)
library(openxlsx)
library(haven)

rm(list = ls())
load_all()

# Load Data ----
pp <- read_sav("../../data_band/PP_mar18_final_tna_n=12971.sav")
pp <- as_factor(pp)
pp2 <- data.table(pp)


## Test Functions
out <- tableband_uni(pp2, vars = c("q1GEND", "q10KNOW_5"))
out <- tableband_uni(pp2, vars = c("q1GEND", "q10KNOW_5"), var_labels = T)

## Mittelwert
var_old <- paste0("q10KNOW_", c(1:3))
var_new <- paste0("n_", var_old)

pp2[, (var_new) := lapply(.SD, as.numeric), .SDcols = var_old]
test <- mean_bi(pp2, row_vars = c("q1GEND", "attribute_4"), col_var = var_new)
mean_bi_helper(pp2, row_vars = c("q1GEND", "attribute_4"), col_var = c("n_q10KNOW_1"), digits = 3)



pp2[, gewichte := sample(1:9, nrow(pp2), replace = TRUE)/10]


system.time(out <- tableband_uni(pp2, vars = c("q1GEND", "q10KNOW_5") ))
system.time(out <- funct_univariate_helper(pp, var = q1GEND ))
system.time(out <- lookup_funct(pp))
system.time(out <- lookup_fast(pp2))

system.time(out <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"), summary = T))
system.time(out <- tabellenband_bivariat(pp, all_of(c("q1GEND", "q10KNOW_1")), all_of(c("q10KNOW_5", "q10KNOW_4")), version = "summary"))

test <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"))
test <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"), summary = T, weights = "age")

t <- tableband_mc(pp2, row_vars = c("q1GEND", "attribute_4"), col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"), count_factor = "Yes")

system.time(t <- tableband_mc(pp2, row_vars = c("q1GEND", "attribute_4"), col_vars = c("q10KNOW_5", "q10KNOW_2", "q10KNOW_4"), count_factor = "Yes"))

funct_univariate_helper(pp, q1GEND)
system.time(out <- tableband_bi(pp2, c("q1GEND", "q10KNOW_1"), c("q10KNOW_5", "q10KNOW_4"), var_labels = F))
out

#test <- tableband_mc(pp2, row_vars = c("q1GEND", "attribute_4"), col_vars = c("q10KNOW_5", "q10KNOW_4", "q10KNOW_1"), count_factor = "Yes")


function1 <- function(x, sq = NULL){

  function2(x = x, sq = sq)

}

function2 <- function(x, sq) {
  if(is.null(sq)){
    print(x)
  } else {
    x <- x*sq
    print(x)
  }

}


function1(2)
function1(2, 4)

