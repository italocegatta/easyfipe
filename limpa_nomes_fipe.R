library(tidyverse)
library(tidytext)

base_raw <- list.files("dados", ".RDS", full.names = TRUE) %>% 
  map_dfr(read_rds)

base <- base_raw %>% 
  select(marca, modelo) %>% 
  unique()

base %>% 
  View()

base %>% 
  mutate(
    nome = pega_nome(modelo),
    versao = pega_versao(modelo),
    motor = str_extract(modelo, "\\d\\.\\d"),
    potencia = str_extract(modelo, "\\d+cv"),
    valvulas = str_extract(modelo, "\\d+[Vv]"),
    tracao = str_extract(modelo, " \\d[Xx]\\d"),
    combustivel = pega_combustivel(modelo),
    cambio = pega_cambio(modelo),
    portas = str_extract(modelo, "\\dp ")
  ) %>% 
  View()

x <- c(
  "155", "145 Elegant", "145 Elegante 1.4", 
  "Legend 3.2/3.5", "CIELO 1.6 16V 119cv 5p", 
  "MINI STAR CS 1.0 8V 53cv (Pick-Up)", "164 Super V6 24V",
  "525i/iA", "164 3.0 V6"
)

pega_versao(x)
str_count(x, "[:graph:]+")

pega_versao <- function(x) {
  str_remove(x, pega_nome(x)) %>% 
    str_remove("\\d\\.\\d") %>% # motor
    str_remove("\\d\\.\\d") %>% # motor
    str_remove("\\d+cv") %>% # potencia
    str_remove("(\\d+V)") %>% # valvulas
    str_remove("\\d[Xx]\\d") %>% # tracao
    str_remove("Aut.|Mec.|Mec") %>% # direcao
    str_remove("Diesel|Dies.|Die.") %>%  # combustivel
    str_replace(" Flex ", " ") %>% # combustivel
    str_remove("\\dp") %>%  # portas
    str_remove("^\\s+") %>% 
    str_remove("\\s+$") %>% 
    str_remove("^\\/") %>% 
    str_remove("\\/$") #%>% 
    #replace("^$", NA_character_)
}

pega_cambio <- function(x) {
  case_when(
    str_detect(x, "Aut.") ~ "Automático",
    str_detect(x, "Mec.|Mec") ~ "Manual",
    TRUE ~ NA_character_
  )
}

pega_combustivel <- function(x) {
  case_when(
    str_detect(x, " Flex ") ~ "Flex",
    str_detect(x, "Dies") ~ "Diesel",
    TRUE ~ NA_character_
  )
}

pega_nome <- function(x) {
  vec_n <- map_dbl(x, ~str_count(.x, "[:graph:]+"))
  
  case_when(
    vec_n == 1 ~ x,
    TRUE ~ str_extract(x,  "^(.*?)(?=\\s)")
  ) %>% 
  str_to_upper()
}

pega_versao <- function(x) {
  vec_n <- map_dbl(x, ~str_count(.x, "\\w+"))
  
  case_when(
    vec_n == 1 ~ NA_character_,
    vec_n == 2 ~ str_extract(x, "(?<=\\s).+"),
    TRUE ~ str_extract(x, "(?<=\\s)(.*?)(?=\\s)")
  ) %>% 
  str_to_upper()
}
