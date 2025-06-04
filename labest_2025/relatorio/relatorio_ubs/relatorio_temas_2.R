# 1. Carregar pacotes
pacman::p_load(tidyverse, stringr, readxl, scales, janitor, openxlsx)

# Esse codigo calcula o porte direto a partir da coluna populacao da pop_cadastrada_2023

# 2. Define regiões
regioes <- list(
  Norte          = c("AC","AP","AM","PA","RO","RR","TO"),
  Nordeste       = c("MA","PI","CE","RN","PB","PE","AL","SE","BA"),
  Sudeste        = c("SP","RJ","ES","MG"),
  Sul            = c("PR","RS","SC"),
  `Centro-Oeste` = c("MT","MS","GO","DF")
)

# 3. Lê os dados
temas <- read_excel("labest_2025/data/UBS - Atencao Primaria/temas_municipio_2023.xlsx") %>%
  clean_names() %>%
  select(uf, ibge, municipio,
         saude_bucal, alimentacao_saudavel,
         saude_sexual_e_reprodutiva, saude_mental,
         autocuidado_de_pessoas_com_doe)

pop <- read_excel("labest_2025/data/UBS - Atencao Primaria/pop_cadastrada_2023_denominador_temas_e_praticas.xls") %>%
  clean_names() %>%
  rename(
    pop_total = qt_total_de_cadastros_limitado_pela_populacao_ibge,
    population = populacao  # <- ajustar o nome conforme está no seu arquivo
  ) %>%
  mutate(across(c(pop_total, population), as.numeric)) %>%
  drop_na(pop_total, population)

# 4. Junta os dados e calcula indicadores
mun <- temas %>%
  left_join(pop, by = "ibge") %>%
  rename(uf = uf.x, municipio = municipio.x) %>%
  drop_na(saude_bucal, alimentacao_saudavel,
          saude_sexual_e_reprodutiva, saude_mental,
          autocuidado_de_pessoas_com_doe) %>%
  mutate(
    total_temas = saude_bucal +
      alimentacao_saudavel +
      saude_sexual_e_reprodutiva +
      saude_mental +
      autocuidado_de_pessoas_com_doe,
    across(
      c(saude_bucal, alimentacao_saudavel,
        saude_sexual_e_reprodutiva, saude_mental,
        autocuidado_de_pessoas_com_doe, total_temas),
      ~ round(.x / pop_total * 100000, 3),
      .names = "ind_{.col}"
    ),
    regiao = case_when(
      uf %in% regioes$Norte          ~ "Norte",
      uf %in% regioes$Nordeste       ~ "Nordeste",
      uf %in% regioes$Sudeste        ~ "Sudeste",
      uf %in% regioes$Sul            ~ "Sul",
      uf %in% regioes$`Centro-Oeste` ~ "Centro-Oeste",
      TRUE                            ~ NA_character_
    ),
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
  select(
    regiao, uf, municipio, ibge, population, pop_total, Porte,Porte_simplificado,
    saude_bucal, alimentacao_saudavel,
    saude_sexual_e_reprodutiva, saude_mental,
    autocuidado_de_pessoas_com_doe, total_temas,
    starts_with("ind_")
  )


# Usando openxlsx para salvar em excel
write.xlsx(mun, file = "labest_2025/relatorio/relatorio_ubs/taxas_temas_municipio.xlsx", sheetName = "mun", rowNames = FALSE)


