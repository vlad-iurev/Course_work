

df_1 <- data %>% 
  filter(h33 == 1) %>% 
  select(id_w, o26_1, o26_2, o26_3) %>% 
  pivot_longer(cols = c(o26_1, o26_2, o26_3),
               names_to = "question",
               values_to = "answer") %>% 
  drop_na(answer)


df_1 <- df_1 %>% 
  group_by(id_w, answer) %>% 
  count(answer) %>% 
  print(n = 80) %>% 
  ungroup()


df_mini <- data %>% 
  filter(h33 == 1) %>%
  group_by(id_w) %>% 
  summarise(total_resp = n(), .groups = "drop")


share <- df_1 %>%
  left_join(df_mini, by = "id_w") %>%
  mutate(
    share = n / total_resp,          
    share_pct = round(100 * share,1)
  )

print(share, n = 80)

top <- share %>% 
  group_by(id_w) %>% 
  slice_max(order_by = share, n = 3) %>% 
  arrange(id_w, desc(share)) %>%
  ungroup()




p <- ggplot(top, aes(x = factor(id_w),
                y = share,
                fill = as_factor(answer))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            position = position_dodge(width = 0.8),
            vjust = -0.25, size = 3.6) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(
    values = c(
      "ДЛЯ НЕПРЕДВИДЕННЫХ РАСХОДОВ, НА ВСЯКИЙ СЛУЧАЙ" = "#fe4a49", # красный
      "НА СТАРОСТЬ"                                     = "#17bebb", # жёлтый
      "ДЛЯ ОПЛАТЫ ЛЕЧЕНИЯ"                              = "#ffc914"  # голубой
    ), 
    guide = guide_legend(ncol = 1)
  ) +
  labs(
    x = "Год",
    y = "Доля домохозяйств",
    fill = "Мотив сбережений"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  coord_cartesian(clip = "off")


ggsave("figures/top3_motives.png", p,
       width = 10, height = 8, dpi = 600)





