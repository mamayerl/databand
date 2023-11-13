
## Lookup Table fuer Beschriftung

#' Title
#'
#' @param df
#'
#' @return
#' @export
#' @import data.table
#' @import forcats
#'
#' @examples
lookup_fast <- function(df, attribute_name = "label", item_text = c("[", "]")){
  lookup <- lapply(df, function(x){attr(x, which = "label", exact = T)})
  lookup <- do.call("rbind", lookup)
  lookup <- data.table(lookup, keep.rownames = T)
  setnames(lookup, old = "rn", new = "variable")
  setnames(lookup, old = "V1", new = "variable_label")

  ## Extract Item - Values
  lookup[, c("item_name", "item_variable_name") := tstrsplit(variable_label, split = item_text[2])]
  lookup[, item_name := gsub(item_text[1], "", item_name, fixed = T)]

  ## Setting right columns
  lookup[, item_name := fifelse(is.na(item_variable_name), NA_character_, item_name)]

  return(lookup)
}


## Extract Text outside the brackes
#label_tab[, c("variable_left", "variable_right") := tstrsplit(variable_label, split = "\\[([^]]+)\\]", perl = T)]



lookup_funct2 <- function(df){
  label_lookup_map <- tibble(
    map_df(df, function(x){attr(x, which="label", exact=TRUE)}) %>% gather()) %>%
    #mutate(id = row_number()) %>%
    rename(variable = key, Fragestellung = value)

  all_names <- names(df)

  return_df <- left_join(tibble(variable = all_names), label_lookup_map, by = c("variable")) %>%
    mutate(Fragestellung = if_else(is.na(Fragestellung), variable, Fragestellung))

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
    relocate(Fragestellung, .after = variable)
  #

  return(return_df)
}


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
