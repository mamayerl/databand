mean_bi <- function(df, row_vars, col_vars, weight = NULL, var_labels = F, digits = 2, summary = F){

  check_varnames(row_vars, col_vars)

  mean_bi <- lapply(col_vars, function(x){
    mean_bi_helper(df, row_vars = row_vars, col_var = x, weight = weight, var_labels = var_labels)
  })

  mean_bi <- Reduce(function(df1, df2) merge(df1, df2, all.x = T, all.y = F, sort = F), mean_bi)


  ## Reduzierte Variante einführen mit n(min): funktioniert aber nooch nicht!!
  if(isTRUE(summary)){
    mean_tab <- mean_bi[stat == "mean", ]
    mean_n <- mean_bi[stat == "n", ]
    mean_n[, n_min := apply(.SD, 1, min, na.rm = T), .SDcols = col_vars]
    #mean_n[, n_min := min(.SD, na.rm = T), .SDcols = col_vars]
    mean_n[, !c(..col_vars)]
    mean_bi <- merge(mean_tab, mean_n, all.x = T, sort = F)
  }

  mean_bi <- mean_bi[, !c("id")]

}


mean_bi_helper <- function(df, row_vars, col_var, weight = NULL, var_labels = F, digits = 2){

  if (!is.numeric(df[[col_var]]))
    stop(paste0(col_var , " is not numeric!"))

  if(isTRUE(var_labels)){
    label_lookup_map <- lookup_fast(df) # Create Lookup Table for adding Labels
  }

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

  ## create n table: noch fertig stellen, n funktioniert noch nicht ganz
  mean_bi_n <- lapply(row_vars, function(x){
    tab_n <- df[, .(n = .N), by = c(x, col_var)]
    tab_n <- tab_n[!is.na(get(x)), ]
    tab_n <- cube(tab_n, .(n = sum(n)), by = x)
    tab_n[, (x) := fct_na_value_to_level(get(x), level = "Total")]
    tab_n[, variable := x]
    tab_n[, id := 1:.N]
    tab_n[, stat := "n"]
    setnames(tab_n, old = x, new = "category")
    setnames(tab_n, old = "n", new = col_var)
    return(tab_n)
  })

  mean_bi_n <- rbindlist(mean_bi_n)

  mean_bi <- rbind(mean_bi, mean_bi_n)

  return(mean_bi)
}





# helper Function fuer Mittelwertberechnung - 1 x 1 Tabelle
funct_mean_helper <- function(df, row_var, col_var, weighted = F, weight = NULL){

  label_lookup_map <- lookup_funct(df)

  if(weighted == F){
    # print("no") # Debugging
    tab_mean <- df %>%
      group_by(Kategorie = {{row_var}}) %>%
      summarise(across({{col_var}}, ~round(mean(.x, na.rm = T), 2))) %>%
      pivot_longer(-Kategorie, names_to = "Variable", values_to = "mean") %>%
      #pivot_wider(names_from = A1, values_from = mean) %>%
      mutate(stat = "Mittelwert") %>%
      mutate(Kategorie = fct_explicit_na(Kategorie, na_level = "(Fehlend)"))

    tab_mean_overall <- df %>%
      summarise(across({{col_var}}, ~round(mean(.x, na.rm = T),2))) %>%
      pivot_longer(1, names_to = "Variable", values_to = "mean") %>%
      #pivot_wider(names_from = A1, values_from = mean) %>%
      mutate(stat = "Mittelwert") %>%
      mutate(Kategorie = "Gesamt")

    tab_mean <- bind_rows(tab_mean, tab_mean_overall)

  }

  if(weighted == T){
    # print("yes") # Debugging

    tab_mean <- df %>%
      group_by(Kategorie = {{row_var}}) %>%
      summarise(across({{col_var}}, ~round(weighted.mean(.x, na.rm = T, w = {{weight}}), 2))) %>%
      pivot_longer(-Kategorie, names_to = "Variable", values_to = "mean") %>%
      #pivot_wider(names_from = A1, values_from = mean) %>%
      mutate(stat = "Mittelwert") %>%
      mutate(Kategorie = fct_explicit_na(Kategorie, na_level = "(Fehlend)"))

    tab_mean_overall <- df %>%
      summarise(across({{col_var}}, ~round(weighted.mean(.x, na.rm = T, w = {{weight}}), 2))) %>%
      pivot_longer(1, names_to = "Variable", values_to = "mean") %>%
      #pivot_wider(names_from = A1, values_from = mean) %>%
      mutate(stat = "Mittelwert") %>%
      mutate(Kategorie = "Gesamt")

    tab_mean <- bind_rows(tab_mean, tab_mean_overall)
  }


  tab_mean_n <- df %>%
    group_by(Kategorie = {{row_var}}) %>%
    summarise(across({{col_var}}, ~sum(!is.na(.x)))) %>%
    pivot_longer(-Kategorie, names_to = "Variable", values_to = "n") %>%
    #pivot_wider( names_from = {{row_var}}, values_from = n) %>%
    mutate(stat = "n_gültig (ungewichtet)") %>%
    mutate(Kategorie = fct_explicit_na(Kategorie, na_level = "(Fehlend)"))


  tab_mean_n_overall <-   df %>%
    summarise(across({{col_var}}, ~sum(!is.na(.x)))) %>%
    pivot_longer(1, names_to = "Variable", values_to = "n") %>%
    mutate(stat = "n_gültig (ungewichtet)") %>%
    mutate(Kategorie = "Gesamt")


  tab_mean_n <- bind_rows(tab_mean_n, tab_mean_n_overall) %>% rename(mean = n)

  tab_mean <- tab_mean %>% bind_rows(tab_mean_n)

  label_lookup_map <- lookup_funct(df)

  tab_mean <- tab_mean %>%
    left_join(label_lookup_map, by = c("Variable" = "Variable")) %>%
    select(-Variable, -id, -Fragestellung_neu) %>%
    rename(Item = Fragestellung,
           Item_Kategorie = Fragekategorien) %>%
    mutate(id_group = row_number()) %>%
    mutate(mean = ifelse(is.nan(mean), NA, mean))

  print(paste(df %>% select({{col_var}}) %>% names(), "-", df %>% select({{row_var}}) %>% names()))

  return(tab_mean)
}


## Function: Zusammenfuegen von Breaks bei einer Spaltenvariable

funct_mean <- function(df, row_vars, col_var, weighted = F, weight = NULL){

  label_lookup_map <- lookup_funct(df)

  row_vars_names <- df %>%
    select({{row_vars}}) %>%
    names()

  col_var_name <- df %>%
    select({{col_var}}) %>%
    names()


  df_return <- map(row_vars_names, ~funct_mean_helper(df, row_var = .data[[.x]], col_var = .data[[{{col_var_name}}]], weighted = weighted, weight = {{weight}})) %>%
    set_names(row_vars_names) %>% # Benennung der Listen nach den Zeilenvariablen
    bind_rows(.id = "Variable")

  df_return <- df_return %>%
    left_join(label_lookup_map %>% select(-Fragekategorien, -Fragestellung_neu), by = c("Variable" = "Variable")) %>%
    relocate(Fragestellung, .after = Variable) %>%
    relocate(Kategorie, .after = Fragestellung) %>%
    relocate(stat, .after = Kategorie)

  return(df_return)
}

## Function: Breaks und mehrere Variablen einer Itembatterie

list_funct_mean <- function(df, row_vars, col_vars, weighted = F, weight = NULL){

  col_vars_names <- df %>% select({{col_vars}}) %>% names()

  res_list <- map(col_vars_names, ~funct_mean(df, row_vars = {{row_vars}}, col_var = .data[[.x]], weighted = weighted, weight = {{weight}}))

  res_list <- set_names(res_list, col_vars_names) # Bennenung der Liste nach Spaltenvariablen
  res_list <- map(res_list, ~mutate(.x, Variable = if_else(id_group >=2, "-", Variable))) # Formatiere Tabelle
  res_list <- map(res_list, ~mutate(.x, Fragestellung = if_else(id_group >=2, "-", Fragestellung)))
  res_list <- map(res_list, ~select(.x, -id_group, -id)) # Entferne Hilfsvariablen
  res_list <- map(res_list, ~mutate(.x, Item = if_else(row_number(Item) > 1, "-", Item)))

  return (res_list)
}

## Zusammenfuegen der Liste zu einer Tabelle
## Bug: Wenn Variablenlabels zu lange (wird bei SPSS abgeschnitten, dann stimmt die ITEM Formulierung nicht zusammen,
## folglich gibt es Probleme beim Left_join.

table_mean_bi <- function(df, row_vars, col_vars, weighted = F, weight = NULL, version = "long", pub_reduced = F){

  tab_bi <- list_funct_mean(df = df, row_vars = {{row_vars}}, col_vars = {{col_vars}}, weighted = weighted, weight = {{weight}})



  funct_rename <- function(df){
    new_name <- df$Item_Kategorie[1]
    names(df)[names(df)=="mean"] <- new_name
    return(df)
  }



  tab_bi <- map(tab_bi, ~funct_rename(.x))

  tab_bi <- map(tab_bi, ~select(.x, -Item_Kategorie))
  tab_bi <- map(tab_bi, ~mutate(.x, id = row_number()))

  tab_bi <- reduce(tab_bi, left_join, by = c("id", "Variable", "Fragestellung", "Kategorie", "stat", "Item")) %>% select(-id)
  tab_bi <- tab_bi %>% relocate(Item, .before = 1)

  if(version == "summary") {
    tab_mean_s <- tab_bi %>% filter(stat == "Mittelwert")
    tab_n_s <- tab_bi %>% filter(stat != "Mittelwert")

    tab_n_s <- tab_n_s %>% rowwise() %>%
      mutate("n (min)" = min(c_across(6:last_col()))) %>%
      select("n (min)") %>%
      ungroup()

    tab_bi <- bind_cols(tab_mean_s, tab_n_s)
  }


  if(pub_reduced == T){
    tab_bi <- tab_bi %>% select(-stat, -Variable)  %>%
      filter(Kategorie != "Gesamt") %>%
      filter(Kategorie != "(Fehlend)")
  }

  return(tab_bi)
}
