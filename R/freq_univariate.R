## Helper Function Univariate

#' Helper Function for Univariate Tables
#' Create a simple frequency table. Is a helper function.
#'
#' @param df
#' @param var
#' @param weights
#'
#' @return data.table
#' @export
#'
#' @examples
freq_univariate_helper <- function(df, var, weights){

  if(is.null(weights)){
    #print("ungewichtet")
    tab_n <- df[, .(n = .N), by = var] # Calculate Frequency
  } else {
    #print("gewichtet")
    tab_n <- df[, .(n = sum(.SD)), by = var, .SDcols = weights] # Calculate Frequency
  }

  tab_n[, perc := round((n/sum(n))*100, 1)] # Add Percent
  tab_n[!is.na(get(var)), perc_valid := round((n/sum(n))*100, 1)] # Add valid Percent
  tab_n[, variable := var] # Create Variablen Name
  setnames(tab_n, old = var, new = "category") # Rename Variable to category
  tab_n[, category := fct_na_value_to_level(category, level = "(Missing)")] # Rename Missings
  tab_n[, id_group := seq.int(nrow(tab_n))] # Add id of groups
  return(tab_n)
}


#' Create a Univariate Frequency Table
#'
#' @param df
#' @param vars
#' @param weights
#'
#' @return
#' @export
#'
#' @examples tableband_uni(df, vars = c("var1", "var2))
tableband_uni <- function(df, vars, weights = NULL){
  label_lookup_map <- lookup_fast(df) # Create Lookup Table for adding Labels
  df_list <- lapply(vars, function (x) freq_univariate_helper(df, var = x, weights = weights)) # Apply Function on multiple Vars

  df_list <- rbindlist(df_list)
  df_list <- label_lookup_map[df_list, on = c("variable")] # Add Variable Labels from Lookup Table

  # Make Table pretty
  df_list[, variable := fifelse(id_group >= 2, "-", variable)]
  df_list[, variable_label := fifelse(id_group >= 2, "-", variable_label)]
  df_list[, id_group := NULL]

  return(df_list)
}



