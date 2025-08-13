library(haven)
library(forcats)
library(geodata)
library(sf)
library(tmap)
library(readxl)
library(writexl)
library(viridis)
library(dplyr)
library(tidyr)
library(stringi)
library(foreign)
library(ivreg)
library(modelsummary)
library(stargazer)
library(broom)
library(labelled)
library(psych)
library(factoextra)
library(mice)
library(fastDummies)
library(gridExtra)
library("unhcrthemes")
import_lato()
library(GPArotation)


knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(root.dir="C:/Users/LEOPOLD/OneDrive - UNHCR/Work/R")


dim_table <- tribble(
  ~Dimension, ~`Non-IDP household`, ~`IDP out-of-camp household`, ~`IDP in camp`,
  "Monetary self-reliance", 26.6, 26.6, 13.2,
  "Education enrolment",    63.5, 64.2, 58.2,
  "Drinking water",         59.7, 56.7, 73.4,
  "Sanitation",             30.5, 28.9, 33.3,
  "Electricity",            12.8, 12.2, 2.1
)

# Step 2: Pivot to long format
dim_long <- dim_table %>%
  pivot_longer(
    cols = -Dimension,
    names_to = "group",
    values_to = "pct_ach"
  )

# Step 3: Recode and factor group levels for plotting
dim_long <- dim_long %>%
  mutate(group = recode(group,
                        "Non-IDP household" = "Non-IDP",
                        "IDP out-of-camp household" = "IDP (out-of-camp)",
                        "IDP in camp" = "IDP (in-camp)"))

dim_long$group <- factor(dim_long$group, levels = c("IDP (in-camp)", "IDP (out-of-camp)", "Non-IDP"))

# Step 4: Factor and label dimensions (optional renaming)
dimension_labels <- c(
  "Monetary self-reliance" = "Monetary self-reliance",
  "Education enrolment" = "Education enrolment",
  "Drinking water" = "Drinking water",
  "Sanitation" = "Sanitation",
  "Electricity" = "Electricity"
)

dim_long$Dimension <- factor(
  dim_long$Dimension,
  levels = rev(names(dimension_labels)),
  labels = rev(dimension_labels)
)

# Step 5: Create plot
ggplot(dim_long, aes(x = pct_ach, y = Dimension)) +
  # Bars for both IDP groups
  geom_col(data = subset(dim_long, group %in% c("IDP (in-camp)", "IDP (out-of-camp)")),
           aes(fill = group),
           width = 0.7,
           position = position_dodge(width = 0.8)) +
  # Dot for non-IDP
  geom_point(data = subset(dim_long, group == "Non-IDP"),
             aes(color = group),
             size = 3,
             position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c(
    "IDP (in-camp)" = "#0072BC",        # Dark blue
    "IDP (out-of-camp)" = "#A7C6ED"     # Light blue
  )) +
  scale_color_manual(values = c(
    "Non-IDP" = "#B41C37"               # Red
  )) +
  labs(
    x = "%",
    y = "Dimension",
    fill = "Group",
    color = "Group",
    title = "Average Achievement"
  ) +
  guides(
    fill = guide_legend(reverse = TRUE, order = 1),
    color = guide_legend(reverse = TRUE, order = 2)
  ) +
  theme_unhcr() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_blank()
  )
ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/EHCVM Mali/ach_figure_CAR.png", width = 6, height = 3.5, dpi = 300)
