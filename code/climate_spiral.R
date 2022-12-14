pacman::p_load(tidyverse, vroom)


temperature_data_og <- vroom("data/GLB.Ts+dSST.csv", 
                             skip = 1, na='***')
#teste

temp_data = temperature_data_og %>%
  select(year = Year, month.abb) %>%
  pivot_longer(-year,names_to = "month", values_to = "t_diff") %>%
  drop_na()

# last_dec = temp_data %>%
#   filter(month == "Dec") %>%
#   mutate(year = year + 1,
#          month = "last_dec")

next_jan = temp_data %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_jan")


temp_data_final = bind_rows(temp_data,next_jan)  %>%
  mutate(month = factor(month, levels = c(month.abb, "next_jan")),
         month_number = as.numeric(month))

annotation = temp_data_final %>%
  slice_max(year) %>%
  slice_max(month_number)

temp_lines = tibble(
  x = 12,
  y = c(1.5,2.0),
  labels = c("1.5\u00B0C","2.0\u00B0C")
)

month_labels = tibble(
  x = 1:12,
  y = 2.7,
  labels = month.abb
)

ggplot(temp_data_final,aes(x = month_number, y = t_diff, group = year, color = year)) +
  geom_col(data = month_labels, aes(x=x+0.5,y=2.4), 
           fill = "black", 
           width = 1,
           inherit.aes = FALSE) +
  geom_col(data = month_labels, aes(x=x+0.5,y=-2), 
           fill = "black", 
           width = 1,
           inherit.aes = FALSE) +
  geom_hline(yintercept = c(1.5,2), color = "red") +
  geom_line() +
  geom_point(data = annotation, aes(x=month_number, y=t_diff, color = year),size = 2.5, inherit.aes = FALSE)+
  geom_label(data = temp_lines, aes(x=x,y=y,label = labels),
             color = "red", fill = "black", label.size = 0,
             inherit.aes = FALSE)+
  geom_text(data = month_labels, aes(x=x,y=y,label = labels),
            color = "white",
            angle = seq(360 - 360/12,0, length.out = 12),
            inherit.aes = FALSE) +
  geom_text(aes(x=1,y=-1.3,label="2022"))+
  scale_x_continuous(breaks = 1:12, labels = month.abb,expand = c(0,0), sec.axis = dup_axis(name = NULL, label = NULL))+
  scale_y_continuous(breaks = seq(-2,2,0.2),limits = c(-2,2.7),expand = c(0,-0.7), sec.axis = dup_axis(name = NULL, label = NULL))+
  scale_color_viridis_c(breaks = seq(1880,2022,20),
                        guide = "none")+
  #coord_cartesian(xlim = c(1,12)) +
  coord_polar(start = 2*pi/12)+
  labs(x = NULL, 
       y = NULL, 
       title = "Global temperature change since (1880 - 2022)") +
  theme(
    panel.background = element_rect(fill  = "#444444", size = 1),
    plot.background = element_rect(fill = "#444444", color = "#444444"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(color = "white",hjust = 0.5, size = 15),
  )

ggsave("figures/temperature_lines.pdf", width = 8, height = 4.5)


