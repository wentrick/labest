pacman::p_load(readxl)


escolas_adesao <- read_excel("labest_2025/data/escolas_adesao_2023_2024.xlsx")


pop_cadastrada <- read_excel("labest_2025/data/pop_cadastrada_2023.xls", 
                                  col_types = c("text", "text", "text", 
                                                "numeric", "text", "numeric", 
                                                "numeric", "numeric"))

