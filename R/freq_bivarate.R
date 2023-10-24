freq_bivar_helper <- function(df, var, byvar){
  label_lookup_map <- lookup_fast(df)

  # crate n, perc table
  df_n <- df[, .(n = .N), keyby = c(var, byvar)]
  df_n <- df_n[, perc := (n/sum(n))*100, by = c(var)]
  df_n <- df_n[!is.na(get(byvar)), perc_valid := (n/sum(n))*100, by = c(var)]
  df_n[, c(var) := fct_na_value_to_level(get(var), level = "(Missing)")]
  df_n[, c(byvar) := fct_na_value_to_level(get(byvar), level = "(Missing)")]


  tab_n <- dcast(df_n, get(var) ~ get(byvar), value.var = "n", fill = 0)
  tab_n[, stat := "n"]

  tab_perc <- dcast(df_n, get(var) ~ get(byvar), value.var = "perc_valid", fill = 0)
  tab_perc[, stat := "Valid Percent"]

  tab_tot <- rbind(tab_n, tab_perc)

#  setnames(df_n, old = "var", new = "category")

  #df_n[, stat := "n"]

   return(tab_tot)

}


df_n <- df %>%
  count({{var}}, {{byvar}}, name = "n") %>%
  mutate(stat = "n (ungewichtet)") %>%
  mutate(n = round(n)) %>%
  mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
  spread({{byvar}}, n, fill = 0) %>%
  adorn_totals(where = c("col", "row"), name = "Gesamt", fill = "n (ungewichtet)") %>%
  rename("Kategorie" = 1)




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
