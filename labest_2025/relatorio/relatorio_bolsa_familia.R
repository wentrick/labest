pacman::p_load(readxl)

bolsa_familia <- read_excel("labest_2025/data/bolsa_familia_consolidado_geral_2023.xlsx")

pop_cadastrada <- read_excel("labest_2025/data/pop_cadastrada_2023.xls", 
                             col_types = c("text", "text", "text", 
                                           "numeric", "text", "numeric", 
                                           "numeric",  "numeric"))














