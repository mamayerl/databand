freq_bivar_helper <- function(df, var, byvar, summary){

  # Ad Missing Values to Factor
  df[, c(var) := fct_na_value_to_level(get(var), level = "(Missing)")]
  df[, c(byvar) := fct_na_value_to_level(get(byvar), level = "(Missing)")]

  # create n, perc table
  df_n <- df[, .(n = .N), keyby = c(var, byvar)]

  # Add Sums for byvar
  df_byvar <- df[, .(n = .N), keyby = c(byvar)]
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

# Function to iterate over vars
freq_bivar <- function(df, vars, byvar, summary){
  label_lookup_map <- lookup_fast(df) # Call lookup Funcation for Labels

  df_list <- lapply(vars, function (x) freq_bivar_helper(df, var = x, byvar = byvar, summary = summary)) # iterate over vars
  df_list <- rbindlist(df_list) # bind result of list to datatable
  df_list <- label_lookup_map[df_list, on = c("variable")] # Add Labels
  df_list[, variable := fifelse(id_group > 1, "-", variable)] # Make labels prettier
  df_list[, variable_label := fifelse(id_group > 1, "-", variable_label)] # make labels prettier
  return(df_list)

}


# Iterate over byvars --> Result in List of byvars for export in Excel-worksheets
tableband_bi <- function(df, row_vars, col_vars, summary = F){
  df_list <- lapply(col_vars, function(x) freq_bivar(df, vars = row_vars, byvar = x, summary = summary))
  return(df_list)
}


tabellenband_bivariat <- function(df, row_vars, col_vars, perc = "row", version = "long", weighted = F, weight = NULL, show_n_weighted = F, show_n = T, pub_reduced = F){

  col_vars_names <- df %>% select({{col_vars}}) %>% names()

  if(perc == "col") {
    res_list <- map(col_vars_names, ~funct_bivariate_col(df, var = {{row_vars}}, byvar = .data[[.x]], weighted = weighted, weight = {{weight}}, show_n_weighted = show_n_weighted, show_n = show_n, version = version))
  }
  else {
    res_list <- map(col_vars_names, ~funct_bivariate(df, var = {{row_vars}}, byvar = .data[[.x]], weighted = weighted, weight = {{weight}}, show_n_weighted = show_n_weighted, show_n = show_n, version = version))
  }

  res_list <- set_names(res_list, col_vars_names) # Bennenung der Liste nach Spaltenvariablen
  res_list <- map(res_list, ~mutate(.x, Variable = if_else(id_group >=2, "-", Variable))) # Formatiere Tabelle
  res_list <- map(res_list, ~mutate(.x, Fragestellung = if_else(id_group >=2, "-", Fragestellung)))
  res_list <- map(res_list, ~select(.x, -id_group, -id)) # Entferne Hilfsvariablen
  res_list <- map(res_list, ~mutate(.x, Item = if_else(row_number(Item) > 1, "-", Item)))
  res_list <- map(res_list, ~mutate(.x, Item_Kategorie = if_else(row_number(Item_Kategorie) > 1, "-", Item_Kategorie)))
  res_list <- map(res_list, ~relocate(.x, Item, Item_Kategorie, .before = 1))

  if(pub_reduced == T){
    res_list <- map(res_list, ~select(.x, -stat, -Item_Kategorie, -Variable))
    res_list <- map(res_list, ~filter(.x, Kategorie != "Gesamt"))
    res_list <- map(res_list, ~filter(.x, Kategorie != "(Fehlend)"))
  }

  return (res_list)
}



funct_bivariate <- function(df, var, byvar, weighted = T, weight = NULL, show_n_weighted = F, show_n = T, version = "long"){

  label_lookup_map <- lookup_funct(df)

  row_vars <- df %>%
    select({{var}}) %>%
    names()

  col_var <- df %>%
    select({{byvar}}) %>%
    names()

  if(version == "summary") {
    df_return <-  map(row_vars, ~funct_bivariate_helper_summary(df, var = .data[[.x]], byvar = .data[[{{col_var}}]], weighted = weighted, weight = {{weight}}, show_n_weighted = show_n_weighted, show_n = show_n)) %>%
      set_names(row_vars) %>% # Benennung der Listen nach den Zeilenvariablen
      bind_rows(.id = "Variable")
  }
  else {
    df_return <-  map(row_vars, ~funct_bivariate_helper(df, var = .data[[.x]], byvar = .data[[{{col_var}}]], weighted = weighted, weight = {{weight}}, show_n_weighted = show_n_weighted, show_n = show_n)) %>%
      set_names(row_vars) %>% # Benennung der Listen nach den Zeilenvariablen
      bind_rows(.id = "Variable")
  }

  df_return <- df_return %>%
    left_join(label_lookup_map %>% select(-Fragekategorien, -Fragestellung), by = c("Variable" = "Variable")) %>%
    relocate(Fragestellung_neu, .after = Variable) %>%
    rename(Fragestellung = Fragestellung_neu)

  return(df_return)
}



funct_bivariate_helper <- function(df, var, byvar, weighted = F, weight = NULL, show_n_weighted = F, show_n = T){

  label_lookup_map <- lookup_funct(df)

  # ## Names Labelling


  if (weighted == T){
    names_perc = "% gültig (gewichtet)"
  } else {
    names_perc = "% gültig (ungewichtet)"
  }

  # Einfache Kreuztabellierung - Haeufigkeit

  if(show_n_weighted == T) {
    df_n_weighted <- df %>%
      count({{var}}, {{byvar}}, wt = {{weight}}, name = "n") %>%
      mutate(stat = "n (gewichtet)") %>%
      mutate(n = round(n)) %>%
      mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
      spread({{byvar}}, n, fill = 0) %>%
      adorn_totals(where = c("col", "row"), name = "Gesamt", fill = "n (gewichtet)") %>%
      rename("Kategorie" = 1)
  }

  if(show_n == T) {
    df_n <- df %>%
      count({{var}}, {{byvar}}, name = "n") %>%
      mutate(stat = "n (ungewichtet)") %>%
      mutate(n = round(n)) %>%
      mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
      spread({{byvar}}, n, fill = 0) %>%
      adorn_totals(where = c("col", "row"), name = "Gesamt", fill = "n (ungewichtet)") %>%
      rename("Kategorie" = 1)
  }

  # Einfache Kreuztabellierung - Prozente
  df_perc <- df %>%
    count({{var}}, {{byvar}}, name = "Haeufigkeit", wt = {{weight}}) %>%
    group_by({{var}}) %>%
    mutate(gesamt = sum(Haeufigkeit),
           flag_na = if_else(gesamt != Haeufigkeit & is.na({{byvar}}), F, T)) %>%
    filter(flag_na == T) %>%
    #filter(!is.na({{byvar}})) %>%
    mutate(Prozent_gültig = round(Haeufigkeit/sum(Haeufigkeit)*100, 1),
           stat = names_perc) %>%
    mutate(Prozent_gültig = if_else(is.na({{byvar}}), 0, Prozent_gültig)) %>%
    select(-flag_na, -gesamt) %>%
    select(-Haeufigkeit) %>%
    spread({{byvar}}, Prozent_gültig, fill = 0) %>%
    adorn_totals(where = c("col"), name = "Gesamt", fill = names_perc) %>%
    rename("Kategorie" = 1)

  if("<NA>" %in% colnames(df_perc)){
    df_perc <- df_perc %>% select(-`<NA>`)
  }


  df_perc_gesamt <- df %>%
    count({{byvar}}, name = "Haeufigkeit", wt = {{weight}}) %>%
    filter(!is.na({{byvar}})) %>%
    mutate(Prozent_gültig = round(Haeufigkeit/sum(Haeufigkeit)*100, 1),
           stat = names_perc) %>%
    select(-Haeufigkeit) %>%
    spread({{byvar}}, Prozent_gültig, fill = 0) %>%
    rename("stat" = 1) %>%
    adorn_totals(where = c("col"), name = "Gesamt", fill = names_perc) %>%
    mutate(Kategorie = "Gesamt")

  df_perc <- bind_rows(df_perc, df_perc_gesamt)

  # Zusammenfuegen von Haeufigkeiten und Prozenten
  if(show_n == T & show_n_weighted == F){
    df_return = bind_rows(df_n, df_perc) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }

  if(show_n == F & show_n_weighted == T){
    df_return = bind_rows(df_n_weighted, df_perc) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }

  if(show_n == T & show_n_weighted == T){
    df_return = bind_rows(df_n, df_n_weighted, df_perc) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }


  #df_return = bind_rows(df_n, df_perc) %>%
  #  mutate(id_group = row_number()) %>%
  # ungroup() %>%
  #mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))

  # Hinzufuegen der Fragestellung
  df_return <- df_return %>%
    mutate(temp = df %>% select({{byvar}}) %>% names()) %>%
    left_join(label_lookup_map %>% select(Variable, Fragestellung, Fragekategorien), by = c("temp" = "Variable")) %>%
    select(-temp) %>%
    rename(Item = Fragestellung,
           Item_Kategorie = Fragekategorien)

  print(paste(df %>% select({{byvar}}) %>% names(), "-", df %>% select({{var}}) %>% names()))

  return(df_return)
}
