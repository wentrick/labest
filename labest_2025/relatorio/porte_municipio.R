# Carregando pacotes necessários
pacman::p_load(tidyverse, readxl, stringr)

# Lendo e limpando os dados
municipio_porte <- read_excel("labest_2025/data/POP2022_Municipios.xls", skip = 1) %>%
  slice(-1) %>%  # Remove a primeira linha
  rename(population = POPULAÇÃO) %>%
  mutate(
    population = as.numeric(population),
    porte_ibge = case_when(
      population <= 5000 ~ "Pequeno I (até 5.000 hab.)",
      population <= 10000 ~ "Pequeno II (5.001–10.000 hab.)",
      population <= 20000 ~ "Médio I (10.001–20.000 hab.)",
      population <= 50000 ~ "Médio II (20.001–50.000 hab.)",
      population <= 100000 ~ "Grande I (50.001–100.000 hab.)",
      population > 100000 ~ "Grande II (> 100.000 hab.)",
      TRUE ~ NA_character_
    ),
    # Extraindo o rótulo do porte diretamente
    Porte = str_extract(porte_ibge, "^[^()]+I{1,2}")
  )


