tableband_mean <- function(df, row_vars, col_vars, weight = NULL, var_labels = F, digits = 2, summary = F, item_labels = F){

  check_varnames(row_vars, col_vars) ## Check if row vars equals col_vars

  df_copy <- copy(df)

  if(!is.data.table(df_copy)){
    df_copy <- data.table(df_copy)
  }


  if(isTRUE(var_labels)){
    label_tab <- lookup_fast(df_copy) # Create Lookup Table for adding Labels
  }

  tab_mean <- mean_bi(df = df_copy, row_vars = row_vars, col_vars = col_vars,
                      weight = weight, var_labels = var_labels, digits = digits, summary = summary)

  if(isTRUE(item_labels) | isTRUE(var_labels)){
    label_tab <- lookup_fast(df)
  }

  if(isTRUE(item_labels)){
    ### Item names einfügen
    names(tab_mean)[names(tab_mean) %in% label_tab$variable] <-
      label_tab$item_name[match(names(tab_mean)[names(tab_mean) %in% label_tab$variable], label_tab$variable)]

    ### Add item variable name
    item_variable_name <- label_tab$item_variable_name[match(col_vars[1][col_vars[1] %in% label_tab$variable], label_tab$variable)]
    reps <- rep("-", length(tab_mean)-1)
    tab_mean[, item_variable_name := c(item_variable_name, rep("-", .N-1))]
  }

  if(isTRUE(var_labels)){
    tab_mean <- label_tab[tab_mean, on = c("variable")] # Add Labels
    tab_mean[, variable_label := fifelse(id > 1, "-", variable_label)] # make labels prettier
    tab_mean <- tab_mean[, !c("item_name", "item_variable_name")]
  }

  tab_mean <- tab_mean[, !c("id")]

  return(tab_mean)
}




mean_bi <- function(df, row_vars, col_vars, weight = NULL, var_labels = F, digits = 2, summary = F){


  mean_bi <- lapply(col_vars, function(x){
    mean_bi_helper(df, row_vars = row_vars, col_var = x, weight = weight, var_labels = var_labels)
  })

  mean_bi <- Reduce(function(df1, df2) merge(df1, df2, all.x = T, all.y = F, sort = F), mean_bi)


  ## Reduzierte Variante einfÃ¼hren mit n(min): funktioniert aber nooch nicht!!
  if(isTRUE(summary)){
    mean_tab <- mean_bi[stat == "mean", ]
    mean_n <- mean_bi[stat == "n", ]
    mean_n[, n_min := apply(.SD, 1, min, na.rm = T), .SDcols = col_vars]
    mean_n <- mean_n[, !c(..col_vars)]
    mean_bi <- cbind(mean_tab, n_min = mean_n$n_min)
  }

  #mean_bi <- mean_bi[, !c("id")]
}


mean_bi_helper <- function(df, row_vars, col_var, weight = NULL, var_labels = F, digits = 2){

  if (!is.numeric(df[[col_var]]))
    stop(paste0(col_var , " is not numeric!"))

  #df[, c(row_vars) := fct_na_value_to_level(get(row_vars), level = "(Missing)")]
  df[, (row_vars) := lapply(.SD, function (x){ fct_na_value_to_level(x, level = "(Missing)")}), .SDcols = row_vars]

  ## create mean table
  mean_bi <- lapply(row_vars, function(x) {
    tab <- cube(df, weighted.mean(.SD, na.rm = T), by = c(x), .SDcols = col_var)
    tab[, V1 := round(V1, digits)]
    tab[, variable := x]
    tab[, id := 1:.N]
    tab[, stat := "mean"]
    setnames(tab, old = x, new = "category")
    setnames(tab, old = "V1", new = col_var)
    tab <- tab[, c("variable", "category", "stat", ..col_var, "id")]
    tab[, category := fct_na_value_to_level(category, level = "Total")]

    return(tab)
  })
  mean_bi <- rbindlist(mean_bi)

  ## create n table
  mean_bi_n <- lapply(row_vars, function(x){
    #tab_n <- df[, .(n = .N), by = c(x, col_var)]
    tab_n <- df[, .(n = sum(!is.na(.SD))), by = c(x), .SDcols = c(col_var)]
    tab_n <- tab_n[!is.na(get(x)), ]
    tab_n <- cube(tab_n, .(n = sum(n)), by = c(x))
    tab_n[, (x) := fct_na_value_to_level(get(x), level = "Total")]
    tab_n[, variable := x]
    tab_n[, id := 1:.N]
    tab_n[, stat := "n"]
    setnames(tab_n, old = x, new = "category")
    setnames(tab_n, old = "n", new = col_var)

    return(tab_n)
  })

  #return(mean_bi_n)

  mean_bi_n <- rbindlist(mean_bi_n)

  mean_bi <- rbind(mean_bi, mean_bi_n)

  return(mean_bi)
}


