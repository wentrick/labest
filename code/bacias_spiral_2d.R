## pacotes
pacman::p_load(tidyverse, gganimate)

## bases

bacias <- readxl::read_excel("~/R Documents/R Data/81bacias.xlsx") %>% 
  mutate(ordem = row_number())
bacias$Estação
dt = readxl::read_excel("~/R Documents/R Data/data_prop.xlsx")
#spiral
join <- left_join(dt, bacias, by = c("bacia" = "Estação"))

dt <- join %>% 
  select(ano, ordem, prop)

## bacia labels
bacia <- tibble(
  x = 1:81,
  labels = 1:81,
  y = 2.7
)

##
nomes <- c(1:81)

##
annotation <- dt %>%
  slice_max(ordem) %>%
  slice_max(ano)  

##
pluv_lines <- tibble(
  x = 81,
  y = c(0,1, 2),
  labels = c("0.0", "1.0", "2.0")
)


dt %>% 
  ggplot(aes(x=ordem, y=prop, group=ano, color=ano)) +
  geom_col(data = bacia, aes(x=x + 0.5, y=2.4), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  geom_col(data = bacia, aes(x=x + 0.5, y=-2), fill = "black",
           width  = 1,
           inherit.aes = FALSE) +
  geom_hline(yintercept = c(0, 1, 2), color="red") +
  geom_line() +
  # geom_point(data=annotation, aes(x=ordem, y=prop, color=ano),
  #            size = 2,
  #            inherit.aes = FALSE) +
  geom_label(data = pluv_lines, aes(x=x, y=y, label=labels),
             color = "red", fill = "black", label.size = 0,
             inherit.aes=FALSE) +
  # geom_text(data = bacia, aes(x=x, y=y, label = labels),
  #           inherit.aes = FALSE, color="white",
  #           angle = seq(360 - 360/81, 0, length.out = 81)) +
  geom_text(aes(x = 1, y=-1.3, label = "2050"), size=6) +
  scale_x_continuous(breaks=1:81,
                     labels=nomes, expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 2.7), expand = c(0, -0.7), 
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_color_viridis_c(breaks = seq(1980, 2050, 10),
                        guide = "none") +
  
  # coord_cartesian(xlim=c(1,12)) +
  coord_polar(start = 2*pi/81) +
  labs(x = NULL,
       y = NULL,
       title = "Propor??o de vaz?o") +
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

#animacao 

anim <- dt %>% 
  ggplot(aes(x=ordem, y=prop, group=ano, color=ano)) +
  geom_rect(aes(xmin=1, xmax=81, ymin=0, ymax=2),
            color="black", fill="black",
            inherit.aes = FALSE) +
  geom_hline(yintercept = c(0, 1, 2), color="red") +
  geom_label(data = pluv_lines, aes(x=x, y=y, label=labels),
             color = "red", fill = "black", label.size = 0,
             inherit.aes=FALSE) +
  geom_text(data =bacia, aes(x=x, y=y, label = labels),
            inherit.aes = FALSE, color="white",
            angle = seq(360 - 360/81, 0, length.out = 81)) +
  geom_label(aes(x = 1, y=-1.3, label = ano), #ano ou 2050
             color="white", fill="black",
             label.padding = unit(50, "pt"), label.size = 0,
             size=6) +
  geom_line() +
  scale_x_continuous(breaks=1:81,
                     labels=nomes, expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_y_continuous(breaks = seq(-2, 2, 0.2),
                     limits = c(-2, 2.7), expand = c(0, -0.7), 
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  scale_color_viridis_c(breaks = seq(1980, 2050, 10),
                        guide = "none") +
  coord_polar(start = 2*pi/81) +
  labs(x = NULL,
       y = NULL,
       title = " Proporção de Vazão (1985-2050)") +
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
  ) +
  transition_manual(frames = ano, cumulative = TRUE)

animate(anim, width=4.155, height=4.5, unit="in", res=300,fps = 60, duration = 20)
anim_save("vazao.gif", path = "figures/climate_spiral.pdf")


animate(anim, width=4.155, height=4.5, unit="in", res=300,
        renderer = av_renderer("vazao.mp4")
)



