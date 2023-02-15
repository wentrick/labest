#pacotes
pacman::p_load(tidyverse, gganimate)

# bases
data <- readxl::read_excel("~/R Documents/R Data/data_prop.xlsx") 

bacias <- readxl::read_excel("~/R Documents/R Data/81bacias.xlsx") %>% 
  mutate(ordem = row_number(),
         ordem = as.factor(ordem))


# juntando os bancos
join <- left_join(data, bacias, by = c("bacia" = "Estação")) %>%
  mutate(ordem = as.double(ordem))



## complemento de dados (fechar o buraco)
next_pluv <- join %>%
  filter(ordem == 1) %>%
  mutate(ano = ano - 1,
         ordem = 0)

teste <- c(1:81)

t_data <- bind_rows(join, next_pluv)%>%
  mutate(ordem = factor(ordem, levels = c(teste, "0")),
         pluv_number = as.numeric(ordem)) 

# Spiral
dt <- t_data %>% 
  select(ano, ordem, prop, pluv_number)

## bacia labels
bacia <- tibble(
  x = 1:81,
  labels = 1:81,
  y = 3
)

##
nomes <- c(1:81)

## Maximos e Minimos da série inteira
annotation_max <- dt %>%
  group_by(pluv_number)  %>%
  slice_max(prop) %>%
  mutate(max = prop)

annotation_min <- dt %>%
  group_by(pluv_number)  %>%
  slice_min(prop) %>%
  mutate(min = prop)

##
pluv_lines <- tibble(
  x = 81,
  y = c(0,1, 2, 3 ),
  labels = c("Sem vazão", "1x", "2x", "3x")
)

## bacias ordenadas do maior maximo
bacias_max = left_join(bacias,annotation_max, by = "ordem") %>%
  select(1,4) %>%
  arrange(desc(prop)) %>%
  mutate(ordem_max = row_number()) %>%
  select(-2)

## bacias ordenadas do menor minimo
bacias_min = left_join(bacias,annotation_min, by = "ordem") %>%
  select(1,4) %>%
  arrange(-desc(prop)) %>%
  unique() %>%
  mutate(ordem_min = row_number()) %>%
  select(-2)

## Ajustando o banco para o plot dos maximos e minimos na animacao 
dt = left_join(dt,annotation_max,by = c("ano", "ordem", "pluv_number"))

dt = left_join(dt,annotation_min,by = c("ano", "ordem", "pluv_number"))

colnames(dt) = c("ano", "ordem", "prop", "pluv_number","prop_max","max", "prop_min", "min")
#### Gráfico 2D ---- 

dt %>% 
  ggplot(aes(x=pluv_number, y=prop, group=ano, color=ano)) +
  geom_col(data = bacia, aes(x=x + 0.5, y=3), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  geom_col(data = bacia, aes(x=x + 0.5, y=-2), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  #geom_hline(yintercept = c(0, 1, 2), color="red") +
  geom_segment(aes(x = 5, y = 0, xend = 75, yend = 0), color = "red") +
  geom_segment(aes(x = 1, y = 1, xend = 80, yend = 1), color = "red") +
  geom_segment(aes(x = 1, y = 2, xend = 80, yend = 2), color = "red") +
  geom_segment(aes(x = 1, y = 3, xend = 80, yend = 3), color = "red") +
  geom_line() +
  geom_point(data=dt, aes(x=pluv_number, y=max, color=ano, fill = ano),
             size = 3,
             shape = 24,
             inherit.aes = FALSE) +
  geom_point(data=dt, aes(x=pluv_number, y=min, color=ano),
             size = 3,
             inherit.aes = FALSE) +
  geom_label(data = pluv_lines, aes(x=x, y=y, label=labels),
             color = "red", fill = "transparent", label.size = 0,
             inherit.aes=FALSE) +
  # geom_text(data = bacia, aes(x=x, y=y, label = labels),
  #           inherit.aes = FALSE, color="white",
  #           angle = seq(360 - 360/81, 0, length.out = 81)) +
  geom_text(aes(x = 1, y=-1.3, label = "2050", color = ano), size=6) +
  scale_x_continuous(breaks=1:81,
                     labels=nomes, expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 3.1), expand = c(0, -0.7), 
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_color_viridis_c(breaks = seq(1980, 2050, 10),
                        guide = "none") +
  scale_fill_viridis_c(breaks = seq(1980, 2050, 10),
                       guide = "none") +
  scale_size(breaks = seq(1980, 2050, 10),
             guide = "none")+
  # coord_cartesian(xlim=c(1,12)) +
  coord_polar(start = 2*pi/81) +
  labs(x = NULL,
       y = NULL,
       title = "Proporção de Vazão (1985-2050)") +
  theme(
    panel.background = element_rect(fill="#444444", size=1),
    plot.background = element_rect(fill = "#444444", color="#444444"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color="white", size=13),
    plot.title = element_text(color="white", hjust = 0.5,size = 15)
  )

#### Animacao ---- 

anim = dt %>% 
  ggplot(aes(x=pluv_number, y=prop, group=ano, color=ano)) +
  geom_col(data = bacia, aes(x=x + 0.5, y=3), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  geom_col(data = bacia, aes(x=x + 0.5, y=-2), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  #geom_hline(yintercept = c(0, 1, 2), color="red") +
  geom_segment(aes(x = 5, y = 0, xend = 75, yend = 0), color = "red") +
  geom_segment(aes(x = 1, y = 1, xend = 80, yend = 1), color = "red") +
  geom_segment(aes(x = 1, y = 2, xend = 80, yend = 2), color = "red") +
  geom_segment(aes(x = 1, y = 3, xend = 80, yend = 3), color = "red") +
  geom_line() +
  geom_point(data=dt, aes(x=pluv_number, y=max, color=ano, fill = ano),
             size = 3,
             shape = 24,
             inherit.aes = FALSE) +
  geom_point(data=dt, aes(x=pluv_number, y=min, color=ano),
             size = 3,
             inherit.aes = FALSE) +
  geom_label(data = pluv_lines, aes(x=x, y=y, label=labels),
             color = "red", fill = "transparent", label.size = 0,
             inherit.aes=FALSE) +
  # geom_text(data = bacia, aes(x=x, y=y, label = labels),
  #           inherit.aes = FALSE, color="white",
  #           angle = seq(360 - 360/81, 0, length.out = 81)) +
  geom_label(aes(x = 1, y=-1.3, label = ano, color = ano), 
             fill = "black", 
             label.size = 0,
             inherit.aes=FALSE)+ #ano ou 2050
  scale_x_continuous(breaks=1:81,
                     labels=nomes, expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 3.1), expand = c(0, -0.7), 
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_color_viridis_c(breaks = seq(1980, 2050, 10),
                        guide = "none") +
  scale_fill_viridis_c(breaks = seq(1980, 2050, 10),
                       guide = "none") +
  scale_size(breaks = seq(1980, 2050, 10),
             guide = "none")+
  # coord_cartesian(xlim=c(1,12)) +
  coord_polar(start = 2*pi/81) +
  labs(x = NULL,
       y = NULL,
       title = "Proporção de Vazão (1985-2050)") +
  theme(
    panel.background = element_rect(fill="#444444", size=1),
    plot.background = element_rect(fill = "#444444", color="#444444"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color="white", size=13),
    plot.title = element_text(color="white", hjust = 0.5,size = 15)
  )+
  transition_manual(frames = ano, cumulative = TRUE)
  

animate(anim, width=1200, height=1080, res=300, duration = 20)
anim_save("vazao.gif", path = "figures/")


animate(anim, width=3840, height=2160, res=300, duration = 20,
        renderer = av_renderer("vazao_video.mp4")
)
anim_save("vazao_video.mp4", path = "figures/")




