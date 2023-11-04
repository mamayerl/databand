#' Create List of dataframes with bivarite frequency tables
#'
#' @param df Input as Dataframe
#' @param row_vars Vector of variable names for breaks
#' @param col_vars Vector of variable names
#' @param summary Output as summary (boolean variable)
#' @param var_labels Use of variable labels (set as attributes in Inputdataframe)
#' @param weights Variable name of weighting variable
#' @param col_per If False (Default): row percent, if True: column percent
#'
#' @return List of dataframes. Each col_var is a list element.
#'
#' @import data.table
#' @export
#'
#' @examples
#' tableband_bi(pp2, row_vars = c("q1GEND", "q10KNOW_1"), col_vars = c("q10KNOW_5", "q10KNOW_4"), summary = T)
#'

tableband_bi <- function(df, row_vars, col_vars, summary = F, var_labels = T, weights = NULL, col_per = F){
  df_list <- lapply(col_vars, function(x) freq_bivar(df, vars = row_vars, byvar = x,
                                                     summary = summary, var_labels = var_labels,
                                                     weights = weights, col_per = col_per))
  return(df_list)
}


# Helper functions ----

freq_bivar_helper <- function(df, var, byvar, summary, weights){

  # Ad Missing Values to Factor
  df[, c(var) := fct_na_value_to_level(get(var), level = "(Missing)")]
  df[, c(byvar) := fct_na_value_to_level(get(byvar), level = "(Missing)")]

  # create n, perc table
  if(is.null(weights)){
    df_n <- df[, .(n = .N), keyby = c(var, byvar)]
  } else {
    df_n <- df[, .(n = sum(.SD)), keyby = c(var, byvar), .SDcols = c(weights)]
  }

  # Add Sums for byvar

  if(is.null(weights)){
    df_byvar <- df[, .(n = .N), keyby = c(byvar)]
  } else {
    df_byvar <- df[, .(n = sum(.SD)), keyby = c(byvar), .SDcols = c(weights)]
  }

  df_byvar[, c(var) := "Total"]
  df_n <- rbind(df_n, df_byvar)

  # calculate valid percent (Without Missing and Totals)
  df_n <- df_n[get(byvar) != "(Missing)" & get(byvar) != "Total", perc_valid := round((n/sum(n))*100, 1), by = c(var)]

  # Spread freq table
  tab_n <- dcast(df_n, get(var) ~ get(byvar), value.var = "n", fill = 0) # Spread Format
  tab_n <- tab_n[, Total := rowSums(tab_n[, 2:ncol(tab_n)], na.rm = T)] # Add Totals in Column
  tab_n[, stat := "n"] # sign statistics variable

  # Spread perc Table
  tab_perc <- dcast(df_n, get(var) ~ get(byvar), value.var = "perc_valid", fill = 0)
  tab_perc <- tab_perc[, Total := rowSums(tab_perc[, 2:ncol(tab_perc)], na.rm = T)]
  tab_perc[, stat := "Valid Percent"]

  if(isFALSE(summary)){
    tab_tot <- rbind(tab_n, tab_perc) # Bind both tables
  }

  if(isTRUE(summary)){
    tab_perc[, "(Missing)" := NULL]
    tab_n[, Total := NULL]
    tab_n[, "n_valid" := rowSums(tab_n[, 2:(ncol(tab_perc)-2)], na.rm = T)]
    setnames(tab_n, old = "(Missing)", new = "n_missing")
    tab_n <- tab_n[, .(var, n_valid, n_missing)]

    tab_tot <- tab_n[tab_perc, on = "var"]
  }

  tab_tot[, id_group := seq.int(nrow(tab_tot))] # Add IDs for prettier tables
  setnames(tab_tot, old = "var", new = "category")
  tab_tot[, variable := var]
  return(tab_tot)
}

freq_bivar_col_helper <- function(df, var, byvar, summary, weights){

  # Ad Missing Values to Factor
  df[, c(var) := fct_na_value_to_level(get(var), level = "(Missing)")]
  df[, c(byvar) := fct_na_value_to_level(get(byvar), level = "(Missing)")]

  # create n
  if(is.null(weights)){
    df_n <- df[, .(n = .N), keyby = c(var, byvar)]
  } else {
    df_n <- df[, .(n = sum(.SD)), keyby = c(var, byvar), .SDcols = c(weights)]
  }

  # Add Sums for byvar

  if(is.null(weights)){
    df_byvar <- df[, .(n = .N), keyby = c(byvar)]
    df_var <- df[, .(n = .N), keyby = c(var)]
  } else {
    df_byvar <- df[, .(n = sum(.SD)), keyby = c(byvar), .SDcols = c(weights)]
    df_var <- df[, .(n = sum(.SD)), keyby = c(var), .SDcols = c(weights)]
  }

  df_byvar[, c(var) := "Total"]
  df_var[, c(byvar) := "Total"]
  df_n <- rbind(df_n, df_byvar, df_var)

  # calculate valid percent (Without Missing and Totals)
  df_n <- df_n[get(var) != "(Missing)" & get(var) != "Total", perc_valid := round((n/sum(n))*100, 1), by = c(byvar)]

  # Spread freq table
  tab_n <- dcast(df_n, get(var) ~ get(byvar), value.var = "n", fill = 0) # Spread Format
  tab_n <- tab_n[, Total := rowSums(tab_n[, 2:ncol(tab_n)], na.rm = T)] # Add Totals in Column
  tab_n[, stat := "n"] # sign statistics variable


  # Spread perc Table
  tab_perc <- dcast(df_n, get(var) ~ get(byvar), value.var = "perc_valid", fill = 0)
  tab_perc_sum <- tab_perc[, lapply(.SD, function(x) sum(x, na.rm = T)), .SDcols = 2:ncol(tab_perc)]
  tab_perc_sum[, var := "Total"]
  tab_perc <- rbind(tab_perc[var != "Total", ], tab_perc_sum)
  #tab_perc <- tab_perc[, Total := rowSums(tab_perc[, 2:ncol(tab_perc)], na.rm = T)]
  #tab_perc_sum <- tab_perc[, Total := sum()]
  #return(tab_sum)
  tab_perc[, stat := "Valid Percent"]


  if(isFALSE(summary)){
    tab_tot <- rbind(tab_n, tab_perc) # Bind both tables
  }

  if(isTRUE(summary)){
    tab_perc[, "(Missing)" := NULL]
    tab_n[, Total := NULL]
    tab_n[, "n_valid" := rowSums(tab_n[, 2:(ncol(tab_perc)-2)], na.rm = T)]
    setnames(tab_n, old = "(Missing)", new = "n_missing")
    tab_n <- tab_n[, .(var, n_valid, n_missing)]

    tab_tot <- tab_n[tab_perc, on = "var"]
  }

  tab_tot[, id_group := seq.int(nrow(tab_tot))] # Add IDs for prettier tables
  setnames(tab_tot, old = "var", new = "category")
  tab_tot[, variable := var]
  return(tab_tot)
}

# Function to iterate over vars
freq_bivar <- function(df, vars, byvar, summary, var_labels, weights, col_per){

  if(isTRUE(var_labels)){
    label_lookup_map <- lookup_fast(df) # Call lookup Funcation for Labels
  }

  if(isFALSE(col_per)){
    df_list <- lapply(vars, function (x) freq_bivar_helper(df, var = x, byvar = byvar, summary = summary,
                                                           weights = weights)) # iterate over vars
  } else {
    df_list <- lapply(vars, function (x) freq_bivar_col_helper(df, var = x, byvar = byvar, summary = summary,
                                                               weights = weights)) # iterate over vars
  }

  df_list <- rbindlist(df_list) # bind result of list to datatable

  df_list[, variable := fifelse(id_group > 1, "-", variable)] # Make labels prettier

  if(isTRUE(var_labels)){
    df_list <- label_lookup_map[df_list, on = c("variable")] # Add Labels
    df_list[, variable_label := fifelse(id_group > 1, "-", variable_label)] # make labels prettier
  }

  return(df_list)
}
