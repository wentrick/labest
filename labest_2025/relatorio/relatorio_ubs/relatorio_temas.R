# 1. Carregar pacotes
pacman::p_load(tidyverse, stringr, readxl, scales, janitor, openxlsx)

# 1. Lê e trata o arquivo de porte
municipio_porte <- read_excel("labest_2025/data/POP2022_Municipios.xls", skip = 1) %>%
  slice(-1) %>%                    
  rename(population = POPULAÇÃO) %>%
  mutate(
    population = as.numeric(population),
    `NOME DO MUNICÍPIO` = str_to_upper(`NOME DO MUNICÍPIO`),
    porte_ibge = case_when(
      population <= 5000   ~ "Pequeno I (até 5.000 hab.)",
      population <= 10000  ~ "Pequeno II (5.001–10.000 hab.)",
      population <= 20000  ~ "Médio I (10.001–20.000 hab.)",
      population <= 50000  ~ "Médio II (20.001–50.000 hab.)",
      population <= 100000 ~ "Grande I (50.001–100.000 hab.)",
      TRUE                 ~ "Grande II (> 100.000 hab.)"
    ),
    Porte = str_extract(porte_ibge, "^[^()]+I{1,2}")
  ) %>%
  select(uf = UF,
    municipio = `NOME DO MUNICÍPIO`,  # ajusta para casar exatamente com o nome em 'mun'
    Porte
  )

# 2. Gera 'mun' conforme você já vinha fazendo
regioes <- list(
  Norte          = c("AC","AP","AM","PA","RO","RR","TO"),
  Nordeste       = c("MA","PI","CE","RN","PB","PE","AL","SE","BA"),
  Sudeste        = c("SP","RJ","ES","MG"),
  Sul            = c("PR","RS","SC"),
  `Centro-Oeste` = c("MT","MS","GO","DF")
)

temas <- read_excel("labest_2025/data/UBS - Atencao Primaria/temas_municipio_2023.xlsx") %>%
  clean_names() %>%
  select(uf, ibge, municipio,
         saude_bucal, alimentacao_saudavel,
         saude_sexual_e_reprodutiva, saude_mental,
         autocuidado_de_pessoas_com_doe)

pop <- read_excel("labest_2025/data/UBS - Atencao Primaria/pop_cadastrada_2023_denominador_temas_e_praticas.xls") %>%
  clean_names() %>%
  rename(pop_total = qt_total_de_cadastros_limitado_pela_populacao_ibge) %>%
  mutate(pop_total = as.numeric(pop_total)) %>%
  drop_na(pop_total)

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
    )
  ) %>%
  # 3. JOIN final pelo nome do município para trazer a coluna Porte
  left_join(municipio_porte, by = c("municipio","uf")) %>%
  select(
    regiao, uf, municipio, ibge, pop_total, Porte,saude_bucal, alimentacao_saudavel,
    saude_sexual_e_reprodutiva, saude_mental, autocuidado_de_pessoas_com_doe,
    starts_with("ind_")
  )

# Agora 'mun' contém também a coluna 'Porte', associada por nome de município.


# Usando openxlsx para salvar em excel
write.xlsx(mun, file = "labest_2025/relatorio/relatorio_ubs/taxas_temas_municipio.xlsx", sheetName = "mun", rowNames = FALSE)



