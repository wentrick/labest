pacman::p_load(tidyverse,readxl,ggplot2,scales)


escolas_adesao <- read_excel("labest_2025/data/escolas_adesao_2023_2024.xlsx") %>%
  rename(municipio = MUNICÍPIO)


pop_cadastrada <- read_excel("labest_2025/data/pop_cadastrada_2023.xls", 
                                  col_types = c("text", "text", "text", 
                                                "numeric", "text", "numeric", 
                                                "numeric", "numeric"))

palestras_municipio <- read_excel("labest_2025/data/palestras_municipio_2023.xlsx") %>%
  select(1,2,3,5,6,14,16,17) %>%
  mutate(total = `Alimentação saudável`+ `Autocuidado de pessoas com doe`+`Saúde bucal`+`Saúde mental` + `Saúde sexual e reprodutiva`) %>%
  rename(municipio = Municipio)



# Juntando pelo campo "ibge"
df <- left_join(pop_cadastrada, escolas_adesao, by = "IBGE") %>%
  select(-1,-9) %>%
  drop_na() 

df_escola_municipio <- df %>%
  select(Município, `QUANTIDADE EDUCANDOS`) %>%
  group_by(Município) %>%
  summarise(
    escolas = n(),
    educandos = sum(`QUANTIDADE EDUCANDOS`, na.rm = TRUE)
  ) %>%
  rename(municipio = Município)

df_escola_municipio <- df_escola_municipio %>%
  left_join(df %>% select(Município, Região) %>% distinct(), 
            by = c("municipio" = "Município"))


#numero de palestras por numero de escolar no municipio

df_final = left_join(df_escola_municipio,palestras_municipio, by = 'municipio') %>%
  rename(total_palestras = total,regiao = Região) %>%
  select(regiao,Uf,municipio,Ibge,escolas,educandos,total_palestras)  %>%
  mutate(taxa_palestras_escola = total_palestras/escolas,
         taxa_educandos_palestras = educandos/total_palestras) 
  


#numero de palestras por numero de educandos

#### Analise Geral ----

# Tabela de frequência
tabela_freq <- df %>%
  group_by(Região) %>%
  summarize(frequencia = n()) %>%
  mutate(
    frequencia_relativa = round(frequencia / sum(frequencia) * 100, 1),
    Região = fct_reorder(Região, -frequencia)  # reordenar do menor para o maior
  )

# Mostrar a tabela
print(tabela_freq)

# Gráfico de barras ordenado
ggplot(tabela_freq, aes(x = Região, y = frequencia, fill = Região)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = frequencia), vjust = -0.5) +
  labs(title = "Escolas por Região", x = "Região", y = "Quantidade") +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################

# Tabela de frequência
tabela_freq <- df %>%
  group_by(`PARTE DO GRUPO PRIORITÁRIO`) %>%
  summarize(frequencia = n()) %>%
  mutate(
    frequencia_relativa = round(frequencia / sum(frequencia) * 100, 1),
    `PARTE DO GRUPO PRIORITÁRIO` = fct_reorder(`PARTE DO GRUPO PRIORITÁRIO`, -frequencia)  # reordenar do menor para o maior
  )

# Mostrar a tabela
print(tabela_freq)

# Gráfico de barras ordenado
ggplot(tabela_freq, aes(x = `PARTE DO GRUPO PRIORITÁRIO`, y = frequencia, fill = `PARTE DO GRUPO PRIORITÁRIO`)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = frequencia), vjust = -0.5) +
  labs(title = "Faz parte do grupo prioritario?", x = " ", y = "Quantidade") +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################

# Tabela de frequência
tabela_freq <- df %>%
  group_by(`POSSUI MAIS DE 50% DOS EDUCANDOS NO BFA`) %>%
  summarize(frequencia = n()) %>%
  mutate(
    frequencia_relativa = round(frequencia / sum(frequencia) * 100, 1),
    `POSSUI MAIS DE 50% DOS EDUCANDOS NO BFA` = fct_reorder(`POSSUI MAIS DE 50% DOS EDUCANDOS NO BFA`, -frequencia)  # reordenar do menor para o maior
  )

# Mostrar a tabela
print(tabela_freq)

# Gráfico de barras ordenado
ggplot(tabela_freq, aes(x = `POSSUI MAIS DE 50% DOS EDUCANDOS NO BFA`, y = frequencia, fill = `POSSUI MAIS DE 50% DOS EDUCANDOS NO BFA`)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = frequencia), vjust = -0.5) +
  labs(title = "Possui mais de 50% dos educandos no BFA?", x = " ", y = "Quantidade") +
  theme_minimal() +
  theme(legend.position = "none")


################################################################################

# Tabela de frequência
tabela_freq <- df %>%
  group_by(`PARTICIPA DO SISTEMA NACIONAL DE ACOMPANHAMENTO DE MEDIDAS SOCIOEDUCATIVAS`) %>%
  summarize(frequencia = n()) %>%
  mutate(
    frequencia_relativa = round(frequencia / sum(frequencia) * 100, 1),
    `PARTICIPA DO SISTEMA NACIONAL DE ACOMPANHAMENTO DE MEDIDAS SOCIOEDUCATIVAS` = fct_reorder(`PARTICIPA DO SISTEMA NACIONAL DE ACOMPANHAMENTO DE MEDIDAS SOCIOEDUCATIVAS`, -frequencia)  # reordenar do menor para o maior
  )

# Mostrar a tabela
print(tabela_freq)

# Gráfico de barras ordenado
ggplot(tabela_freq, aes(x = `PARTICIPA DO SISTEMA NACIONAL DE ACOMPANHAMENTO DE MEDIDAS SOCIOEDUCATIVAS`, y = frequencia, fill = `PARTICIPA DO SISTEMA NACIONAL DE ACOMPANHAMENTO DE MEDIDAS SOCIOEDUCATIVAS`)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = frequencia), vjust = -0.5) +
  labs(title = "Participa do sistema nacional de medidas socioeducativas?", x = " ", y = "Quantidade") +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################


# Adicionar total e calcular percentual
df_soma <- df %>%
  summarize(across(c(14:20), ~sum(.x, na.rm = TRUE))) %>% 
  pivot_longer(cols = c(1:7), names_to = 'Tipo', values_to = "Quantidade") %>%
  mutate(
    Tipo = gsub("^QUANTIDADE ", "", Tipo),
    Percentual = Quantidade / sum(Quantidade),
    Label = paste0(format(Quantidade, big.mark = ".", decimal.mark = ","), 
                   "\n(", percent(Percentual, accuracy = 0.1), ")")
  )

# Gráfico com rótulos no topo
ggplot(df_soma, aes(x = fct_reorder(Tipo, -Quantidade), y = Quantidade)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Label), vjust = -0.3, size = 3.5) +  # <-- Rótulos acima das barras
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.1))) +  # Expansão para não cortar o texto
  labs(title = "Quantidade por Tipo",
       x = "Tipo",
       y = "Quantidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Analise Regional ----



















