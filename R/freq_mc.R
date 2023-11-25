
#' Create Table for Multiple Choice Answer
#'
#' @param df Dataframe with variables
#' @param row_vars Variables for Breaks
#' @param col_vars Multiple Choice Variables to count for
#' @param count_factor Factor Level to count for
#' @param item_labels Boolean parameter for showing item labels (Default: True) or variable names (False). The item labels are generated from attributes ("label").
#' @param weight Weight
#'
#' @return Returns a single dataframe with frequency of multiple choice answers
#' @import data.table
#' @export
#'
#' @examples
#' tableband_mc(df, row_vars = c("GEND", "AGE_gr), col_vars = c("q10KNOW_1", "q10KNOW_2", "q10KNOW_2"), count_factor = "Yes")
#'
#'
## Idee col_vars als Liste eingegeben. Dann kann darüber iteriert werden.

tableband_mc <- function(df, row_vars, col_vars, count_factor, item_labels = T, weight = NULL){

  df_copy <- copy(df)

  if(!is.data.table(df_copy)){
    df_copy <- data.table(df_copy)
  }


  tab_bi <- tableband_bi(df = df_copy, row_vars = row_vars, col_vars = col_vars, summary = T,
                         var_labels = T, weights = weight)

  ### Lables als Parameter einf
  ###
  tab_bi <- mapply(rename_mc, df = tab_bi, col_var = col_vars, count_factor = count_factor, SIMPLIFY = F)
  tab_bi <- Reduce(function(df1, df2) merge(df1, df2, all.x = T, all.y = F, sort = F), tab_bi)
  tab_bi <- tab_bi[, !c("id")]

  ### Item names einfuegen
  if(isTRUE(item_labels)){
    label_tab <- lookup_fast(df)
    names(tab_bi)[names(tab_bi) %in% label_tab$variable] <-
      label_tab$item_name[match(names(tab_bi)[names(tab_bi) %in% label_tab$variable], label_tab$variable)]

    ### Add item variable name
    item_variable_name <- label_tab$item_variable_name[match(col_vars[1][col_vars[1] %in% label_tab$variable], label_tab$variable)]
    reps <- rep("-", length(tab_bi)-1)
    tab_bi[, item_variable_name := c(item_variable_name, rep("-", .N-1))]
  }

  return(tab_bi)

}

## Helper Function for rename

rename_mc <- function(df, col_var, count_factor){
  keep_names <- c("variable", "variable_label", "category", "stat", "n_valid", "n_missing", c(count_factor))
  new <- df[, ..keep_names]
  setnames(new, old = count_factor, new = col_var)
  new[, id := c(1:.N)]
  #print(new)
}


