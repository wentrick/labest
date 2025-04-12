pacman::p_load(tidyverse,readr,vroom,scales,glue)

temperature_data_og <- vroom("data/GLB.Ts+dSST.csv", 
                             skip = 1, na='***')

temp_data = temperature_data_og %>%
  select(year = Year,temp_diff = `J-D`) %>%
  drop_na()

annotation = temp_data %>%
  arrange(year) %>%
  slice(1, n()) %>%
  mutate(temp_diff = 0,
         x = year + c(-5,5))

max_t_diff = max(temp_data$temp_diff)
min_year = min(temp_data$year)
ggplot(temp_data, aes(x = year,y = temp_diff, fill = temp_diff)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = annotation, aes(x=x, label = year), color = "white") +
  geom_text(x = 1880,y =1, hjust = 0, color = "white",
            label = glue("Global temperatures have increase by over {max_t_diff} since {min_year}")) +
  #scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", midpoint = 0)+
  # scale_fill_gradientn(colors = c("darkblue","white","darkred"),
  #                      values = rescale(c(min(temp_data$temp_diff),0,max(temp_data$temp_diff))),
  #                      limits = c(min(temp_data$temp_diff),max(temp_data$temp_diff))) + 
  scale_fill_stepsn(colors = c("darkblue","white","darkred"),
                    values = rescale(c(min(temp_data$temp_diff),0,max(temp_data$temp_diff))),
                    limits = c(min(temp_data$temp_diff),max(temp_data$temp_diff)),
                    n.breaks = 9) + 
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  )


ggsave("figures/temperature_index_col_plot.pdf",width = 7,height = 4)
