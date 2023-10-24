## Helper Function Univariate

freq_univariate_helper <- function(df, var){
  tab_n <- df[, .(n = .N), by = var]
  tab_n[, perc := round((n/sum(n))*100, 1)]
  tab_n[!is.na(get(var)), perc_valid := round((n/sum(n))*100, 1)]
  tab_n[, variable := var]
  setnames(tab_n, old = var, new = "category")
  tab_n[, category := fct_na_value_to_level(category, level = "(Missing)")]
  tab_n[, id_group := seq.int(nrow(tab_n))]
    #tab_n[ , c("id_group", names(tab_n)[names(tab_n) != "id_group"])]
  return(tab_n)
}


tableband_uni <- function(df, vars, weights = NULL){
  label_lookup_map <- lookup_fast(df)
  #label_lookup_map <- data.table(label_lookup_map) ## kann später entfernt werden...
  df_list <- lapply(vars, function (x) freq_univariate_helper(df, var = x))

  df_list <- rbindlist(df_list)
  df_list <- label_lookup_map[df_list, on = c("variable")]
  #df_list[, Fragestellung_neu := NULL]
  #df_list[, id := NULL]

  df_list[, variable := fifelse(id_group >= 2, "-", variable)]
  df_list[, variable_label := fifelse(id_group >= 2, "-", variable_label)]
  df_list[, id_group := NULL]

  return(df_list)
}





tabellenband_univariat <- function(df, vars, weighted = F, weight = NULL){
  label_lookup_map <- lookup_funct(df)

  row_vars <- df %>%
    select({{vars}}) %>%
    names()

  df_return <-  map(row_vars, ~funct_univariate_helper(df, var = .data[[.x]], weighted = weighted, weight = {{weight}})) %>%
    set_names(row_vars) %>% # Benennung der Listen nach den Zeilenvariablen
    bind_rows(.id = "Variable")

  df_return <- df_return %>%
    left_join(label_lookup_map %>% select(-Fragestellung_neu), by = c("Variable" = "Variable")) %>%
    relocate(Fragestellung, .after = Variable) %>%
    relocate(Fragekategorien, .after = Fragestellung)

  df_return <- df_return %>%
    mutate(Variable = if_else(id_group >=2, "-", Variable)) %>% # Formatiere Tabelle
    mutate(Fragestellung = if_else(id_group >=2, "-", Fragestellung)) %>%
    mutate(Fragekategorien = if_else(id_group >=2, "-", Fragekategorien)) %>%
    select(-id_group, -id) # Entferne Hilfsvariablen

  return(df_return)
}





## Univariate Haeufigkeiten
funct_univariate_helper <- function(df, var, weighted = F, weight = NULL){
  ## Names for Output
  perc = "Prozent"
  perc_valid = "Prozent gültig"
  name_n = "n"

  tab_n <- df %>% count({{var}}, wt = {{weight}})

  if(weighted == T){
    tab_n_unweight <-  df %>% count({{var}}) %>% rename("n (ungewichtet)" = n)
  }

  tab_perc1 <- df %>% count({{var}}, wt = {{weight}} )  %>%
    mutate({{perc}} := round((n/sum(n)*100), 1))

  tab_perc2 <- df %>% count({{var}}, wt = {{weight}} )  %>%
    na.omit() %>%
    mutate({{perc_valid}} :=  round((n/sum(n)*100), 1))

  tab_perc <- left_join(tab_n, tab_perc1) %>%
    left_join(tab_perc2) %>%
    mutate(Variable = names(tab_n)[1]) %>%
    rename(Kategorie = {{var}}) %>%
    mutate(n = round(n)) %>%
    rename({{name_n}} := n) %>%
    relocate(Variable, 1) %>%
    mutate(Kategorie = fct_explicit_na(Kategorie, na_level = "(Fehlend)")) %>%
    mutate(id_group = row_number())

  if(weighted == T){
    tab_perc <- left_join(tab_n, tab_n_unweight) %>%
      left_join(tab_perc1) %>%
      left_join(tab_perc2) %>%
      mutate(Variable = names(tab_n)[1]) %>%
      rename(Kategorie = {{var}}) %>%
      mutate(n = round(n)) %>%
      rename({{name_n}} := n) %>%
      relocate(Variable, 1) %>%
      mutate(Kategorie = fct_explicit_na(Kategorie, na_level = "(Fehlend)")) %>%
      mutate(id_group = row_number()) %>%
      rename("n (gewichtet)" = n) %>%
      rename("Prozent (g)" = Prozent) %>%
      rename("Prozent gültig (g)" = "Prozent gültig")
  }

  #print(df %>% select({{var}}) %>% names())

  return (tab_perc)
}


## Univariate Auswertung


