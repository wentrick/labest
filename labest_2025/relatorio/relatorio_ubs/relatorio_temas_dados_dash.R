# 1. Carregar pacotes
pacman::p_load(tidyverse, stringr, readxl, scales, janitor, openxlsx)

# Esse codigo calcula o porte direto a partir da coluna populacao da pop_cadastrada_2023

# 3. Lê os dados (somand todos so temas dessa vez)
temas <- read_excel("labest_2025/data/UBS - Atencao Primaria/temas_municipio_2023.xlsx") %>%
  clean_names() %>%
  mutate(
    total_temas = rowSums(
      across(4:last_col()),    # todas as colunas da 4ª em diante
      na.rm = TRUE
    )
  )

pop <- read_excel("labest_2025/data/UBS - Atencao Primaria/pop_cadastrada_2023_denominador_temas_e_praticas.xls") %>%
  clean_names() %>%
  rename(
    pop_total = qt_total_de_cadastros_limitado_pela_populacao_ibge,
    population = populacao  # <- ajustar o nome conforme está no seu arquivo
  ) %>%
  mutate(across(c(pop_total, population), as.numeric)) %>%
  drop_na(pop_total, population) %>%
  select(-c(municipio,uf,competencia_cnes))

# 4. Junta os dados e calcula indicadores
mun <- temas %>%
  left_join(pop, by = "ibge") %>%
  relocate(
    regiao, uf, municipio, ibge,
    population, pop_total, cobertura_aps, total_temas,
    .before = 1  # garante que essas colunas vão para o início
  ) %>%
  mutate(
    Porte = case_when(
      population <= 5000   ~ "Pequeno I (até 5.000 hab.)",
      population <= 10000  ~ "Pequeno II (5.001–10.000 hab.)",
      population <= 20000  ~ "Médio I (10.001–20.000 hab.)",
      population <= 50000  ~ "Médio II (20.001–50.000 hab.)",
      population <= 100000 ~ "Grande I (50.001–100.000 hab.)",
      TRUE                 ~ "Grande II (> 100.000 hab.)"
    )
  ) %>%
  mutate(Porte_simplificado = str_extract(Porte, "^[^()]+I{1,2}")) %>%
  select(regiao, uf, municipio, ibge,
         population, pop_total, cobertura_aps, total_temas,Porte,Porte_simplificado)


# Usando openxlsx para salvar em excel
write.xlsx(mun, file = "labest_2025/relatorio/relatorio_ubs/taxas_temas_municipio_dash.xlsx", sheetName = "mun", rowNames = FALSE)


