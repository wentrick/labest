pacman::p_load(tidyverse,readxl,ggplot2,scales,janitor,openxlsx)

# 0) Carregar pacotes
library(readxl)
library(dplyr)
library(stringr)
library(janitor)

# 1) Importar dados
atividade_municipio <- read_excel(
  "labest_2025/data/Escolas/atividades_pse_2023_numerador.xlsx"
)

escolas_adesao <- read_excel(
  "labest_2025/data/Escolas/escolas_pse_adesao_2023_2024_denominador.xlsx"
)

pop <- read_excel(
  "labest_2025/data/UBS - Atencao Primaria/pop_cadastrada_2023_denominador_temas_e_praticas.xls"
)

# 2) Padronizar nomes em snake_case
pop_clean <- pop               %>% clean_names()
ativ_clean <- atividade_municipio %>% clean_names()
esc_clean <- escolas_adesao     %>% clean_names()

# 3) Construir df_final
df_final <- pop_clean %>%
  
  # 3.1) Selecionar e renomear colunas principais do pop
  transmute(
    ibge       = ibge,
    regiao     = str_to_title(regiao),  # “norte” → “Norte”
    municipio  = municipio,
    population = populacao
  ) %>%
  
  # 3.2) Juntar as atividades (e já trazer o uf do atividade_municipio)
  left_join(
    ativ_clean %>%
      transmute(
        ibge,
        uf                 = uf,          # sigla já no arquivo de atividades
        municipio,
        atividade_educacao = educacao,
        atividade_saude    = saude
      ) %>%
      group_by(ibge, uf, municipio) %>%
      summarise(
        atividade_educacao = sum(atividade_educacao, na.rm = TRUE),
        atividade_saude    = sum(atividade_saude,    na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(total_atividade = atividade_educacao + atividade_saude),
    by = c("ibge", "municipio")
  ) %>%
  
  # 3.3) Juntar escolas e educandos
  left_join(
    esc_clean %>%
      filter(situacao_adesao == "Aderido") %>%
      transmute(
        ibge,
        municipio,
        escolas   = 1,                     # contar escolas
        educandos = quantidade_educandos
      ) %>%
      group_by(ibge, municipio) %>%
      summarise(
        escolas   = sum(escolas,   na.rm = TRUE),
        educandos = sum(educandos, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("ibge", "municipio")
  ) %>%
  
  # 4) Classificação de porte
  mutate(
    Porte = case_when(
      population <=  5000   ~ "Pequeno I (até 5.000 hab.)",
      population <= 10000   ~ "Pequeno II (5.001–10.000 hab.)",
      population <= 20000   ~ "Médio I (10.001–20.000 hab.)",
      population <= 50000   ~ "Médio II (20.001–50.000 hab.)",
      population <= 100000  ~ "Grande I (50.001–100.000 hab.)",
      TRUE                  ~ "Grande II (> 100.000 hab.)"
    ),
    Porte_simplificado = str_extract(Porte, "^[^()]+I{1,2}")
  ) %>%
  
  # 5) Cálculo das razões
  mutate(
    razao_atividades_por_escola = round(total_atividade / escolas,   8),
    razao_educando_atividade    = round(total_atividade / educandos, 8)
  ) %>%
  
  # 6) Seleção e ordenação final de colunas
  select(
    regiao, uf, municipio, ibge,
    Porte, Porte_simplificado, population,
    escolas, educandos,
    atividade_educacao, atividade_saude, total_atividade,
    razao_atividades_por_escola, razao_educando_atividade
  )

# E voilà — agora a coluna `uf` vem preenchida a partir do seu data frame de atividades.

# Usando openxlsx para salvar em excel
write.xlsx(mun, file = "labest_2025/relatorio/relatorio_ubs/taxas_atividades_municipio_dash.xlsx", sheetName = "mun", rowNames = FALSE)

