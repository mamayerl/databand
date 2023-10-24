library(tidyverse)
library(haven)
library(janitor)
library(openxlsx)

# Hilfsfunktionen, die auch einzeln verwendet werden koennen

# Funktionen fuer die Erstellung eines Tabellenbandes
## Lookup Table fuer Beschriftung

lookup_funct <- function(df){
  label_lookup_map <- tibble(
    map_df(df, function(x){attr(x, which="label", exact=TRUE)}) %>% gather()) %>%
    #mutate(id = row_number()) %>%
    rename(Variable = key, Fragestellung = value)
  
  all_names <- names(df)
  
  return_df <- left_join(tibble(Variable = all_names), label_lookup_map, by = c("Variable")) %>% 
    mutate(Fragestellung = if_else(is.na(Fragestellung), Variable, Fragestellung))
  
  # Klammerausdruck extrahieren
  return_df <- return_df %>% 
    separate(Fragestellung, into = c("Fragestellung", "Fragestellung2"), sep = "] ", fill = "left")
  return_df$Fragestellung <- gsub("[", "", return_df$Fragestellung, fixed = TRUE) #
  
  ## Extract fuer langen Text
  #return_df <- return_df %>% 
  #  mutate(Fragestellung = if_else(is.na(Fragestellung), Fragestellung, paste0(" - ", Fragestellung))) %>%
  #  mutate(Fragestellung_neu = if_else(is.na(Fragestellung), Fragestellung2, paste0(Fragestellung2, Fragestellung)))
  
  ## Extrakt nur Klammerausdruck
  # return_df <- return_df %>% 
  #   mutate(Fragestellung_neu = if_else(is.na(Fragestellung), Fragestellung2, Fragestellung)) %>%
  #   select(-Fragestellung, -Fragestellung2) %>%
  #   rename(Fragestellung = Fragestellung_neu) %>%
  #   mutate(id = row_number())
  # 
  
  ## Teile Fragestellung und Kategorien
  return_df <- return_df %>% 
    mutate(Fragestellung_neu = if_else(is.na(Fragestellung), Fragestellung2, paste0(Fragestellung2, " - ", Fragestellung))) %>%
    #   select(-Fragestellung, -Fragestellung2) %>%
    rename(Fragekategorien = Fragestellung, 
           Fragestellung = Fragestellung2) %>%
    mutate(id = row_number()) %>%
    relocate(Fragestellung, .after = Variable)
  # 
  
  return(return_df)
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


## Bivariate Haeufigkeitsauswertung

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

funct_bivariate_helper_summary <- function(df, var, byvar, weighted = F, weight = NULL, show_n_weighted = F, show_n = T){
  
  label_lookup_map <- lookup_funct(df)
  
  if (weighted == T){
    names_perc = "% gültig (gewichtet)"
  } else {
    names_perc = "% gültig (ungewichtet)"
  }
  
  if(show_n == T){
  # Einfache Kreuztabellierung - Haeufigkeit
  df_n <- df %>% 
    count({{var}}, {{byvar}}, name = "Haeufigkeit") %>%
    mutate(stat = "Haeufigkeit") %>%
    mutate(Haeufigkeit = round(Haeufigkeit)) %>%
    #mutate("{{byvar}}" := fct_expand({{byvar}}, "(Fehlend)")) %>%
    mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
    spread({{byvar}}, Haeufigkeit, fill = 0) %>%
    adorn_totals(where = c("col", "row"), name = "Gesamt", fill = "Haeufigkeit") %>%
    rename("Kategorie" = 1)
  
  if(!("(Fehlend)" %in% colnames(df_n))) {
    df_n <- df_n %>% mutate("(Fehlend)" = 0)
  }
  
  df_n_valid <- df_n %>%
    select(Kategorie, stat, "(Fehlend)", Gesamt) %>%
    mutate("n (gültig)" = Gesamt - `(Fehlend)`) %>%
    select(-stat, -Gesamt) %>%
    rename(`n (fehlend)` = `(Fehlend)`) %>%
    relocate(`n (fehlend)`, .after = `n (gültig)`)
  }
  
  if(show_n_weighted == T){
    # Einfache Kreuztabellierung - Haeufigkeit
    df_n_w <- df %>% 
      count({{var}}, {{byvar}}, name = "Haeufigkeit", wt = {{weight}}) %>%
      mutate(stat = "Haeufigkeit") %>%
      mutate(Haeufigkeit = round(Haeufigkeit)) %>%
      #mutate("{{byvar}}" := fct_expand({{byvar}}, "(Fehlend)")) %>%
      mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
      spread({{byvar}}, Haeufigkeit, fill = 0) %>%
      adorn_totals(where = c("col", "row"), name = "Gesamt", fill = "Haeufigkeit") %>%
      rename("Kategorie" = 1)
    
    if(!("(Fehlend)" %in% colnames(df_n_w))) {
      df_n_w <- df_n_w %>% mutate("(Fehlend)" = 0)
    }
    
    df_n_w_valid <- df_n_w %>%
      select(Kategorie, stat, "(Fehlend)", Gesamt) %>%
      mutate("n (gültig, gewichtet)" = Gesamt - `(Fehlend)`) %>%
      select(-stat, -Gesamt) %>%
      rename(`n (fehlend, gewichtet)` = `(Fehlend)`) %>%
      relocate(`n (fehlend, gewichtet)`, .after = `n (gültig, gewichtet)`)
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
    df_return <- df_perc %>% left_join(df_n_valid, by = c("Kategorie")) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }
  
  if(show_n == F & show_n_weighted == T){
    df_return <- df_perc %>% left_join(df_n_w_valid, by = c("Kategorie")) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }
  
  if(show_n == T & show_n_weighted == T){
    df_return <- df_perc %>% 
      left_join(df_n_w_valid, by = c("Kategorie")) %>%
      left_join(df_n_valid, by = c("Kategorie")) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }
  
  #df_return <- df_perc %>% left_join(df_n_valid, by = c("Kategorie")) %>%
  #  mutate(id_group = row_number()) %>%
  #  ungroup() %>%
  #  mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))

  # Hinzufuegen der Fragestellung
  df_return <- df_return %>%
    mutate(temp = df %>% select({{byvar}}) %>% names()) %>%
    left_join(label_lookup_map %>% select(Variable, Fragestellung, Fragekategorien), by = c("temp" = "Variable")) %>%
    select(-temp) %>%
    rename(Item = Fragestellung,
           Item_Kategorie = Fragekategorien)

  print(paste(df %>% select({{byvar}}) %>% names(), "-", df %>% select({{var}}) %>% names()))

  return(df_return)
  #return(df_n)
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

# Bivariate Colwise

funct_bivariate_col_helper <- function(df, var, byvar, weighted = T, weight = NULL, show_n_weighted = F, show_n = T){
  
  label_lookup_map <- lookup_funct(df)
  
  ## Names Labelling
  if (weighted == T){
    names_perc = "% gültig (gewichtet)"
  } else {
    names_perc = "% gültig (ungewichtet)"
  }
  
  # Einfache Kreuztabellierung - Haeufigkeit
  
  if(show_n_weighted == T) {
    df_n_weighted <- df %>%  
    count({{var}}, {{byvar}}, name = "Haeufigkeit", wt = {{weight}}) %>%
    mutate(stat = "n (gewichtet)") %>%
    mutate(Haeufigkeit = round(Haeufigkeit)) %>%
    mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
    spread({{byvar}}, Haeufigkeit, fill = 0) %>%
    adorn_totals(where = c("col", "row"), name = "Gesamt", fill = "n (gewichtet)") %>%
    rename("Kategorie" = 1)
  }
  
  if(show_n == T) {
    df_n <- df %>%
      count({{var}}, {{byvar}}, name = "Haeufigkeit") %>%
      mutate(stat = "n (ungewichtet)") %>%
      mutate(Haeufigkeit = round(Haeufigkeit)) %>%
      mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
      spread({{byvar}}, Haeufigkeit, fill = 0) %>%
      adorn_totals(where = c("col", "row"), name = "Gesamt", fill = "n (ungewichtet)") %>%
      rename("Kategorie" = 1)
  }
  
  
  
  # Einfache Kreuztabellierung - Prozente
  df_perc <- df %>% 
    count({{var}}, {{byvar}}, name = "Haeufigkeit", wt = {{weight}}) %>%
    group_by({{byvar}}) %>%
    filter(!is.na({{var}})) %>%
    mutate(Prozent_gültig = round(Haeufigkeit/sum(Haeufigkeit)*100, 1), 
           stat = names_perc) %>% 
    select(-Haeufigkeit) %>%
    mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
    spread({{byvar}}, Prozent_gültig, fill = 0) %>%
    adorn_totals(where = c("row"), name = "Gesamt", fill = names_perc) %>%
    rename("Kategorie" = 1)
  
  df_perc_gesamt <- df %>% 
    count({{var}}, name = "Haeufigkeit", wt = {{weight}}) %>%
    filter(!is.na({{var}})) %>%
    mutate(Prozent_gültig = round(Haeufigkeit/sum(Haeufigkeit)*100, 1),
           stat = names_perc) %>%
    select(-Haeufigkeit) %>%
    rename("Gesamt" = "Prozent_gültig") %>%
    adorn_totals(where = c("row"), name = "Gesamt", fill = names_perc) %>%
    rename("Kategorie" = {{var}})
  
  df_perc <- left_join(df_perc, df_perc_gesamt)
  
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
  #  ungroup() %>%
  #  mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  
  df_return <- df_return %>% 
    mutate(temp = df %>% select({{byvar}}) %>% names()) %>%
    left_join(label_lookup_map %>% select(Variable, Fragestellung, Fragekategorien), by = c("temp" = "Variable")) %>%
    select(-temp) %>%
    rename(Item = Fragestellung, 
           Item_Kategorie = Fragekategorien)
  print(paste(df %>% select({{byvar}}) %>% names(), "-", df %>% select({{var}}) %>% names()))
  return(df_return)
}

funct_bivariate_col_helper_summary <- function(df, var, byvar, weighted = F, weight = NULL, show_n_weighted = F, show_n = T){
  
  label_lookup_map <- lookup_funct(df)
  
  if (weighted == T){
    names_perc = "% gültig (gewichtet)"
  } else {
    names_perc = "% gültig (ungewichtet)"
  }
  
  if(show_n == T){
  # Einfache Kreuztabellierung - Haeufigkeit
  df_n_valid <- df %>% 
    count({{var}}, {{byvar}}, name = "Haeufigkeit") %>%
    mutate(stat = "Haeufigkeit") %>%
    mutate(Haeufigkeit = round(Haeufigkeit)) %>%
    mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
    spread({{byvar}}, Haeufigkeit, fill = 0) %>%
    rename("Kategorie" = 1) %>%
    filter(!is.na(Kategorie)) %>%
    adorn_totals(where = c("row", "col"), name = "Gesamt", fill = "n (gültig)") %>%
    filter(Kategorie == "Gesamt")
  
  if(!("(Fehlend)" %in% colnames(df_n_valid))) {
    df_n_valid <- df_n_valid %>% mutate("(Fehlend)" = 0)
  }
  }
  
  if(show_n_weighted == T){
    df_n_w_valid <- df %>% 
      count({{var}}, {{byvar}}, name = "Haeufigkeit", wt = {{weight}}) %>%
      mutate(stat = "Haeufigkeit") %>%
      mutate(Haeufigkeit = round(Haeufigkeit)) %>%
      mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
      spread({{byvar}}, Haeufigkeit, fill = 0) %>%
      rename("Kategorie" = 1) %>%
      filter(!is.na(Kategorie)) %>%
      adorn_totals(where = c("row", "col"), name = "Gesamt", fill = "n (gültig, gewichtet)") %>%
      filter(Kategorie == "Gesamt")
    
    if(!("(Fehlend)" %in% colnames(df_n_w_valid))) {
      df_n_w_valid <- df_n_w_valid %>% mutate("(Fehlend)" = 0)
    }
  }
  
  # Einfache Kreuztabellierung - Prozente
  df_perc <- df %>% 
    count({{var}}, {{byvar}}, name = "Haeufigkeit", wt = {{weight}}) %>%
    group_by({{byvar}}) %>%
    filter(!is.na({{var}})) %>%
    mutate(Prozent_gültig = round(Haeufigkeit/sum(Haeufigkeit)*100, 1), 
           stat = names_perc) %>% 
    select(-Haeufigkeit) %>%
    mutate("{{byvar}}" := fct_explicit_na({{byvar}}, na_level = "(Fehlend)")) %>%
    spread({{byvar}}, Prozent_gültig, fill = 0) %>%
    adorn_totals(where = c("row"), name = "Gesamt", fill = names_perc) %>%
    rename("Kategorie" = 1)
  
  df_perc_gesamt <- df %>% 
    count({{var}}, name = "Haeufigkeit", wt = {{weight}}) %>%
    filter(!is.na({{var}})) %>%
    mutate(Prozent_gültig = round(Haeufigkeit/sum(Haeufigkeit)*100, 1),
           stat = names_perc) %>%
    select(-Haeufigkeit) %>%
    rename("Gesamt" = "Prozent_gültig") %>%
    adorn_totals(where = c("row"), name = "Gesamt", fill = names_perc) %>%
    rename("Kategorie" = {{var}})
  
  df_perc <- left_join(df_perc, df_perc_gesamt, by = c("Kategorie", "stat"))
  
  # Zusammenfuegen von Haeufigkeiten und Prozenten
  if(show_n == T & show_n_weighted == F){
    df_return <- bind_rows(df_perc, df_n_valid) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }
  
  if(show_n == F & show_n_weighted == T){
    df_return <- bind_rows(df_perc, df_n_w_valid) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }
  
  if(show_n == T & show_n_weighted == T){
    df_return <- bind_rows(df_perc, df_n_valid, df_n_w_valid) %>%
      mutate(id_group = row_number()) %>%
      ungroup() %>%
      mutate(Kategorie = if_else(is.na(Kategorie), "(Fehlend)", Kategorie))
  }
  
  df_return <- df_return %>% 
    mutate(temp = df %>% select({{byvar}}) %>% names()) %>%
    left_join(label_lookup_map %>% select(Variable, Fragestellung, Fragekategorien), by = c("temp" = "Variable")) %>%
    select(-temp) %>%
    rename(Item = Fragestellung, 
           Item_Kategorie = Fragekategorien)
  
  print(paste(df %>% select({{byvar}}) %>% names(), "-", df %>% select({{var}}) %>% names()))
  
  return(df_return)
}


funct_bivariate_col <- function(df, var, byvar, weighted = T, weight = NULL, show_n_weighted = F, show_n = T, version = "long"){
  
  label_lookup_map <- lookup_funct(df)
  
  row_vars <- df %>% 
    select({{var}}) %>% 
    names()
  
  col_var <- df %>%
    select({{byvar}}) %>%
    names()
  if(version == "summary") {
    df_return <-  map(row_vars, ~funct_bivariate_col_helper_summary(df, var = .data[[.x]], byvar = .data[[{{col_var}}]], weighted = weighted, weight = {{weight}}, show_n_weighted = show_n_weighted, show_n = show_n)) %>%
      set_names(row_vars) %>% # Benennung der Listen nach den Zeilenvariablen
      bind_rows(.id = "Variable")
  }
  else{
    df_return <-  map(row_vars, ~funct_bivariate_col_helper(df, var = .data[[.x]], byvar = .data[[{{col_var}}]], weighted = weighted, weight = {{weight}}, show_n_weighted = show_n_weighted, show_n = show_n)) %>%
      set_names(row_vars) %>% # Benennung der Listen nach den Zeilenvariablen
      bind_rows(.id = "Variable")
  }
  
  df_return <- df_return %>%
    left_join(label_lookup_map %>% select(-Fragekategorien, -Fragestellung), by = c("Variable" = "Variable")) %>%
    relocate(Fragestellung_neu, .after = Variable) %>%
    rename(Fragestellung = Fragestellung_neu)
  
  return(df_return)
}


## Function Mean univariate: VErbesserungsmoeglichkeit, weighted - Parameter weg zu bekommen.
funct_mean_uni <- function(df, list_of_vars, weighted = F, weight) {
  
  label_lookup_map <- lookup_funct(df)
  
  if (weighted == F) {
    
    print("No weight")
    uni_mean <-  df %>% 
      mutate(across({{list_of_vars}}, ~as.numeric(.x))) %>%
      summarise(across({{list_of_vars}}, ~mean(.x, na.rm = T))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Mittelwert") 
  } else {
    print("Weight")
    
    uni_mean <-  df %>% 
      mutate(across({{list_of_vars}}, ~as.numeric(.x))) %>%
      summarise(across({{list_of_vars}}, ~weighted.mean(.x, w = {{weight}}, na.rm = T))) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Mittelwert")
  }
  
  uni_mean_n <-  df %>%
    mutate(across({{list_of_vars}}, ~as.numeric(.x))) %>%
    summarise(across({{list_of_vars}}, ~sum(!is.na(.x)))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "n_gültig")
  
  uni_mean_tab <- left_join(uni_mean, uni_mean_n) %>%
    left_join(label_lookup_map) %>%
    relocate(Label, .after = Variable)
  
  
  
  return(uni_mean_tab) 
}

#funct_mean_uni(lm4, list_of_vars = c(la07_1, q34_4, q38_1, q54_1, q54_2, qualitaet_scal), weighted = F, weight = gewicht)


## Export Listein Excel: Bennenung der Tabellenblaetter nach Namen der Liste
export_xlsx <- function(list_of_df, filename){
  
  # create workbook
  wb <- createWorkbook()
  
  #creating an anonymous function inside Map())
  Map(function(data, nameofsheet){     
    addWorksheet(wb, nameofsheet)
    writeData(wb, nameofsheet, data)}, list_of_df, names(list_of_df))
  
  ## Save workbook to excel file 
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}

## Rating Univariate
mean_uni_func <- function(df, list_of_vars, weight = NULL){
  
  label_lookup_map <- lookup_funct(df)
  
  twofun <- list(mean = ~weighted.mean(., na.rm = T), 
                 valid = ~sum(!is.na(.)))
  
  df %>%
    select(all_of(list_of_vars), {{weight}}) %>%
    summarise(across(all_of(list_of_vars), twofun)) %>%
    gather(vars, num) %>% 
    separate(vars, into = c("var", "var_num", "stat")) %>%
    mutate(var_num = as.numeric(var_num)) %>%
    unite(var, var, var_num, sep = "_", remove = F) %>%
    spread(stat, num) %>%
    select(-var_num) %>%
    left_join(label_lookup_map, by = c("var" = "Variable")) %>%
    relocate(Label, .after = var) %>%
    arrange(id, var) 
}




## Rating Skala univariat
rating_uni_funct <- function(df, vars, weight = NULL){
  
  label_lookup_map <- lookup_funct(df)
  names_vars <- df %>% select({{vars}}) %>% names()
  
  df_n <- names_vars %>%
    map(~ count(df, .data[[.x]], wt = {{weight}})) %>%
    map(~ pivot_wider(data = .x, names_from = 1, values_from = n, values_fill = 0)) %>%
    reduce(bind_rows) %>%
    mutate(Variable = names_vars, stat = "n") %>%
    replace(is.na(.), 0)
  
  
  df_perc <- df_n %>%
    select(-stat) %>%
    pivot_longer(cols = !Variable, names_to = "cat", values_to = "n") %>% 
    filter(cat != "NA") %>%
    group_by(Variable) %>%
    mutate(perc = n / sum(n)) %>%
    separate(Variable, into = c("var_temp", "var_num"), sep = "_", remove = F) %>%
    select(-n, -var_temp) %>%
    pivot_wider(names_from = cat, values_from = perc, values_fill = 0) %>%
    arrange(as.numeric(var_num)) %>%
    select(-var_num) %>%
    mutate(stat = "Prozent gültig")
  
  df_return <- bind_rows(df_n, df_perc) %>%
    select(Variable, stat, everything())
  df_return <- left_join(df_return, label_lookup_map, by = c("Variable"="Variable"))
  df_return <- df_return %>% relocate(Label, .after = "Variable")
  
  return(df_return)
}




## Univariate Auswertung


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

## Tabellenband Bivariat (zu uebergeben ist ein Datensatz, Liste der Zeilenvariablen und Liste der Spaltenvariablen)


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

# tabellenband_bivariat_col <- function(df, row_vars, col_vars, weight = NULL){
#   
#   col_vars_names <- df %>% select({{col_vars}}) %>% names()
#   
#   res_list <- map(col_vars_names, ~funct_bivariate_col(df, var = {{row_vars}}, byvar = .data[[.x]], weight = {{weight}}))
#   
#   res_list <- set_names(res_list, col_vars_names) # Bennenung der Liste nach Spaltenvariablen
#   res_list <- map(res_list, ~mutate(.x, Variable = if_else(id_group >=2, "-", Variable))) # Formatiere Tabelle
#   res_list <- map(res_list, ~mutate(.x, Label = if_else(id_group >=2, "-", Label)))
#   res_list <- map(res_list, ~select(.x, -id_group, -id)) # Entferne Hilfsvariablen
#   
#   return (res_list)
# }



## Funktion Tabellenband: Kombination Univariat als auch Bivariate

tabellenband_export <- function(df, list_row_vars, list_col_vars){
  tab_uni <- tabellenband_univariat(df, list_row_vars) %>% list() %>% set_names("univariat gesamt") # Rufe univariate Tabellenfunktion auf
  tab_bi <- tabellenband_bivariat(df, list_row_vars, list_col_vars) # Rufe bivariate Tabellenfunktion auf
  tab_final <- c(tab_uni, tab_bi) # Fuege Listen zusammen
  
  return(tab_final)
}

## Tabelle fuer Multiple-Choice: Funktionsweise: 1) Bivariate Tabllen errechnen und dann 2) Ja Spalten zusammenfuegen.

table_mc_bi <- function(df, row_vars, col_vars, weighted = F, weight = NULL,  show_n_weighted = F, show_n = T, pub_reduced = F){
  
  if(show_n == T & show_n_weighted == T){
    stop("Nur entweder n = gewichtet oder nicht gewichtet")
  }
  
  if(show_n == T & show_n_weighted == F){
  tab_bi <- tabellenband_bivariat(df = df, row_vars = {{row_vars}}, col_vars = {{col_vars}}, perc = "row", version = "summary", weighted = weighted, weight = {{weight}}, show_n_weighted = F, show_n = T)
  }
  
  if(show_n == F & show_n_weighted == T){
    tab_bi <- tabellenband_bivariat(df = df, row_vars = {{row_vars}}, col_vars = {{col_vars}}, perc = "row", version = "summary", weighted = weighted, weight = {{weight}}, show_n_weighted = T, show_n = F)
  }
  
  
  funct_rename <- function(df){
    new_name <- df$Item_Kategorie[1]
    names(df)[names(df)=="Ja"] <- new_name
    return(df)
  }
  
  #tab_bi <- map(tab_bi, ~select(.x, -Item))
  #tab_bi <- map(tab_bi, ~rename(.x, Item = Item_Kategorie))
  tab_bi <- map(tab_bi, ~funct_rename(.x)) # Rename Ja Spalte
  tab_bi <- map(tab_bi, ~select(.x, -`Nicht gewählt`, -Item_Kategorie, -Gesamt))
  tab_bi <- map(tab_bi, ~mutate(.x, id = row_number()))
  
  if(show_n == T & show_n_weighted == F){
  tab_bi <- reduce(tab_bi, left_join, by = c("id", "Variable", "Fragestellung", "Kategorie", "stat", "n (gültig)", "n (fehlend)", "Item")) %>% 
    select(-id, -`n (gültig)`, -`n (fehlend)`, `n (gültig)`, `n (fehlend)`)
  }
  
  if(show_n == F & show_n_weighted == T){
    tab_bi <- reduce(tab_bi, left_join, by = c("id", "Variable", "Fragestellung", "Kategorie", "stat", "n (gültig, gewichtet)", "n (fehlend, gewichtet)", "Item")) %>% 
      select(-id, -`n (gültig, gewichtet)`, -`n (fehlend, gewichtet)`, `n (gültig, gewichtet)`, `n (fehlend, gewichtet)`)
  }
  
  tab_bi <- tab_bi %>% relocate(Item, .before = 1)
  
  if(pub_reduced == T){
    tab_bi <- tab_bi %>% select(-stat, -Variable) %>% 
      filter(Kategorie != "Gesamt") %>% 
      filter(Kategorie != "(Fehlend)")
  }
  
  return(tab_bi)


  # #tab_bi <- map(tab_bi, ~select(.x, -Item))
  # #tab_bi <- map(tab_bi, ~rename(.x, Item = Item_Kategorie))
  # tab_bi <- map(tab_bi, ~funct_rename(.x)) # Rename Ja Spalte
  # tab_bi <- map(tab_bi, ~select(.x, -`Nicht Gewählt`, -Item_Kategorie, -Gesamt))
  # tab_bi <- map(tab_bi, ~mutate(.x, id = row_number()))
  # tab_bi <- reduce(tab_bi, left_join, by = c("id", "Variable", "Fragestellung", "Kategorie", "stat", "n (gültig)", "n (fehlend)", "Item")) %>% select(-id, -`n (gültig)`, -`n (fehlend)`, `n (gültig)`, `n (fehlend)`)
  # 
  # tab_bi <- tab_bi %>% relocate(Item, .before = 1)
  # 
  # 
  # 
  # return(tab_bi)
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
