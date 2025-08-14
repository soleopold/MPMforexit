
data_dim <- data_dim %>%
  mutate(ach_index = (1/3) * DIM1 +
                      (1/3) * DIM2.1 +
                      #(1/6) * DIM2.2 +
                      (1/9) * DIM3.1 +
                      (1/9) * DIM3.2 +
                      (1/9) * DIM3.3
  )


#average index for host_local and host_national
avg_index_local <- data_dim %>%
  filter(popgroup2 == "host_local") %>%
  summarise(
    avg_local = weighted.mean(
      ach_index,
      weights,
      na.rm = TRUE
    )
  ) %>%
  pull(avg_local)


avg_index_national <- data_dim %>%
  filter(popgroup2 %in% c("host_local", "host_far")) %>%
  summarise(
    avg_national = weighted.mean(
      ach_index,
      weights,
      na.rm = TRUE
    )
  ) %>%
  pull(avg_national)

data_dim <- data_dim %>%
  mutate(ach_index_avg_local = avg_index_local,
         ach_index_avg_national = avg_index_national)


#table 
dim_table_data <- data_dim %>%
  select(hh_popgroup, popgroup2, hhweight, weights,starts_with("DIM"))
  
dim_table <- dim_table_data %>%
   select(-DIM1b) %>%
  group_by(hh_popgroup) %>%
  summarise(across(starts_with("DIM"), ~ weighted.mean(.x, hhweight, na.rm = TRUE) * 100, .names = "avg_{.col}")) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("avg_"), 
               names_to = "Dimension", 
               values_to = "Percentage") %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  mutate(Dimension = case_when(
    Dimension == "avg_DIM1" ~ "% non-poor (national pov line)",
    Dimension == "avg_DIM2.1" ~ "% all kids in school",
   # Dimension == "avg_DIM2.2" ~ "% no adults completed primary",
    Dimension == "avg_DIM3.1" ~ "% access to limited standard drinking water",
    Dimension == "avg_DIM3.2" ~ "% access to limited standard sanitation",
    Dimension == "avg_DIM3.3" ~ "% access to electricity",
    TRUE ~ Dimension
  )) %>%
  pivot_wider(names_from = hh_popgroup, values_from = Percentage) %>%
  select(Dimension, everything())


dim_table_popgroup2 <- dim_table_data %>%
  group_by(popgroup2) %>%
  summarise(across(starts_with("DIM"), 
                   ~ weighted.mean(.x, hhweight, na.rm = TRUE) * 100, 
                   .names = "avg_{.col}")) %>%
  ungroup() %>%
  pivot_longer(cols = starts_with("avg_"), 
               names_to = "Dimension", 
               values_to = "Percentage") %>%
  mutate(Percentage = round(Percentage, 2)) %>%
  mutate(Dimension = case_when(
    Dimension == "avg_DIM1" ~ "% non-poor (national pov line)",
    Dimension == "avg_DIM2.1" ~ "% all kids in school",
  #  Dimension == "avg_DIM2.2" ~ "% no adults completed primary",
    Dimension == "avg_DIM3.1" ~ "% access to limited standard drinking water",
    Dimension == "avg_DIM3.2" ~ "% access to limited standard sanitation",
    Dimension == "avg_DIM3.3" ~ "% access to electricity",
    TRUE ~ Dimension
  )) %>%
  pivot_wider(names_from = popgroup2, values_from = Percentage) %>%
  select(Dimension, everything())

host_local_column <- dim_table_popgroup2 %>%
  select(Dimension, host_local)

# Join the host_local column to dim_table by Dimension
dim_table <- dim_table %>%
  left_join(host_local_column, by = "Dimension")


dim_long <- dim_table %>%
  pivot_longer(
    cols = c("Host", "host_local", "IDP"),
    names_to = "group",
    values_to = "pct_ach"
  )

dim_long <- dim_long %>%
  mutate(group = recode(group,
                        "host_local" = "NDP (local)",
                        "Host"       = "NDP (all)",
                        "IDP"        = "IDP"))

dim_long$group <- factor(dim_long$group, levels = c("IDP", "NDP (local)", "NDP (all)"))

# Labels for dimensions
dimension_labels <- c(
  "% non-poor (national pov line)" = "Monetary self-reliance",
  "% all kids in school" = "Educational enrollment",
  "% access to limited standard drinking water" = "Drinking water",
  "% access to limited standard sanitation" = "Sanitation",
  "% access to electricity" = "Electricity"
)

dim_long$Dimension <- factor(
  dim_long$Dimension,
  levels = rev(names(dimension_labels)),
  labels = rev(dimension_labels)
)



ggplot(dim_long, aes(x = pct_ach / 100, y = Dimension)) +
  # Bars for IDP
  geom_col(data = subset(dim_long, group == "IDP"),
           aes(fill = group),
           width = 0.8) +
  # Dots for NDP groups
  geom_point(data = subset(dim_long, group != "IDP"),
             aes(color = group),
             size = 3,
             position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("IDP" = "#0072BC")) +
  scale_color_manual(values = c("NDP (all)" = "#B41C37", "NDP (local)" = "#F1998E")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "%",
    y = "Dimension",
    fill = "Group",
    color = "Group"
  ) +
  guides(
    fill = guide_legend(reverse = TRUE, order = 1),
    color = guide_legend(reverse = TRUE, order = 2)
  ) +
  theme_unhcr() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank(),
    axis.title.x = element_blank()
  )


ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/EHCVM Mali/ach_figure_Mali.png", width = 6, height = 3.5, dpi = 300)





#histogram with achievement index
data_idp <- data_dim %>% filter(hh_popgroup == "IDP")

# Create the histogram
ggplot(data_idp, aes(x = ach_index)) +
  geom_histogram(binwidth = 0.05, fill = "#0072BC", color = "white", boundary = 0) +
  geom_vline(aes(xintercept = ach_index_avg_local), 
             data = data_dim, 
             color = "#F1998E", linetype = "dashed", size = 1) +
    # Add text label for national average
  geom_text(
    data = data.frame(x = avg_index_local, y = 100),  # adjust y for placement
    aes(x = x, y = y),
    label = scales::number(avg_index_local, accuracy = 0.01),
    inherit.aes = FALSE,
    hjust = 1.2, color = "#F1998E", fontface = "bold"
  ) +
  geom_vline(aes(xintercept = ach_index_avg_national), 
             data = data_dim, 
             color = "#B41C37", linetype = "dashed", size = 1) +
   geom_text(
    data = data.frame(x = avg_index_national, y = 100),  # adjust y for placement
    aes(x = x, y = y),
    label = scales::number(avg_index_national, accuracy = 0.01),
    inherit.aes = FALSE,
    hjust = -0.2, color = "#B41C37", fontface = "bold"
  ) +
  
  labs(
    x = "MPM score",
    y = "Number of Households"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_unhcr()

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/EHCVM Mali/histogram_ach_Mali.png", width = 6, height = 3, dpi = 300)
