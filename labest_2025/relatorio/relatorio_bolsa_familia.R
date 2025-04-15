pacman::p_load(readxl)

bolsa_familia <- read_excel("labest_2025/data/bolsa_familia_consolidado_geral_2023.xlsx")

pop_cadastrada <- read_excel("labest_2025/data/pop_cadastrada_2023.xls", 
                             col_types = c("text", "text", "text", 
                                           "numeric", "text", "numeric", 
                                           "numeric",  "numeric"))
# Juntando pelo campo "ibge"
df <- left_join(pop_cadastrada, bolsa_familia, by = "IBGE") %>%
  select(-1,-9) %>%
  drop_na()




#### Analise Geral ----

#BENEFICIARIOS X REGIAO

# Agrupando os dados e criando os rótulos
df_soma <- df %>%
  select(c(1, 2, 5, 6, 10, 11)) %>%
  group_by(Região) %>%
  summarize(across(c(2:5), ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentual = `Qtd. beneficiários acompanhados` / `Qtd. beneficiários a serem acompanhados`,
    Label = paste0(
      format(`Qtd. beneficiários acompanhados`, big.mark = ".", decimal.mark = ","), 
      "\n(", percent(percentual, accuracy = 0.1), ")"
    )
  )

# Gráfico com ordenação do maior para o menor
ggplot(df_soma, aes(x = fct_reorder(Região, -percentual), 
                    y = percentual)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = Label), vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Beneficiários Acompanhados por Região",
       x = "Região",
       y = "Qtd. Beneficiários Acompanhados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###############################################################################

#BENEFICIARIOS CRIANCAS X REGIAO

# Agrupando e preparando dados
df_soma <- df %>%
  select(c(1, 2, 5, 6, 13, 14)) %>%
  group_by(Região) %>%
  summarize(across(c(2:5), ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentual = `Qtd. criança acompanhada`/ `Qtd. criança a ser acompanhada`  ,
    Label = paste0(
      format(`Qtd. criança a ser acompanhada`, big.mark = ".", decimal.mark = ","), 
      "\n(", percent(percentual, accuracy = 0.1), ")"
    )
  )


# Gráfico com ordenação do maior para o menor
ggplot(df_soma, aes(x = fct_reorder(Região, -`Qtd. criança a ser acompanhada`), 
                    y = `Qtd. criança a ser acompanhada`)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = Label), vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Beneficiários crianças acompanhados por Região",
       x = "Região",
       y = "Qtd. Beneficiários Acompanhados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


################################################################################
# ANALISE POR UF

#BENEFICIARIOS X UF

# Agrupando e preparando dados
df_soma <- df %>%
  select(c(1, 2, 5, 6, 10, 11)) %>%
  group_by(UF) %>%
  summarize(across(c(2:5), ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentual = `Qtd. beneficiários acompanhados` / `Qtd. beneficiários a serem acompanhados`,
    Label = paste0(
      format(`Qtd. beneficiários acompanhados`, big.mark = ".", decimal.mark = ","), 
      "(", percent(percentual, accuracy = 0.1), ")"
    )
  )

# Gráfico de barras horizontal
ggplot(df_soma, aes(x = fct_reorder(UF, `Qtd. beneficiários acompanhados`), 
                    y = `Qtd. beneficiários acompanhados`)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = Label), hjust = -0.05, size = 3.5) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Beneficiários Acompanhados por UF",
       x = "Região",
       y = "Qtd. Beneficiários Acompanhados") +
  coord_flip() +  # <- muda para horizontal
  theme_minimal()

###############################################################################

#BENEFICIARIOS CRIANCAS X UF

# Agrupando e preparando dados
df_soma <- df %>%
  select(c(1, 2, 5, 6, 13, 14)) %>%
  group_by(UF) %>%
  summarize(across(c(2:5), ~sum(.x, na.rm = TRUE))) %>%
  mutate(
    percentual = `Qtd. criança acompanhada`/ `Qtd. criança a ser acompanhada`  ,
    Label = paste0(
      format(`Qtd. criança a ser acompanhada`, big.mark = ".", decimal.mark = ","), 
      "(", percent(percentual, accuracy = 0.1), ")"
    )
  )

# Gráfico de barras horizontal
ggplot(df_soma, aes(x = fct_reorder(UF, `Qtd. criança a ser acompanhada`), 
                    y = `Qtd. criança a ser acompanhada`)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = Label), hjust = -0.05, size = 3.5) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Beneficiários crianças acompanhados por UF",
       x = "Região",
       y = "Qtd. Beneficiários Acompanhados") +
  coord_flip() +  # <- muda para horizontal
  theme_minimal()




