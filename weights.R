bandwidth <- 3000

plot_title <- glue::glue("Weights Produced by Gaussian Kernel with {scales::comma(bandwidth, 1)}km Bandwidth")

weights <- tibble(d = seq(0,10000, by = 10),
                  w = exp(-(d/bandwidth)^2),
                  lab = case_when(d == 300  ~ "Death Valley",
                                  d == 1350 ~ "Yellowstone",
                                  d == 2490 ~ "Mexico City",
                                  d == 3940 ~ "New York City",
                                  d == 8760 ~ "London",
                                  TRUE      ~ NA_character_))


weights %>% 
  ggplot(aes(d, w, label = lab)) +
  geom_line() + 
  geom_point(data = ~ filter(.x, !is.na(lab))) +
  geom_text(data = ~ filter(.x, !is.na(lab)), hjust = 0, vjust = 0, nudge_x = 50, nudge_y = 0.01) +
  theme_bw() +
  scale_x_continuous("Distance from Los Angeles, km", breaks = seq(0,10000, 2000), labels = scales::comma) +
  scale_y_continuous("Weight in Local Model for Los Angeles") +
  ggtitle(plot_title, "Model Centered on Los Angeles")

ggsave("Figures/LA_weights.png", height = 4, width = 6)
