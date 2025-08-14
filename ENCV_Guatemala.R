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
library(stringr)
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
library(scales)

hogares <- read_sav("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/ENCV Guatemala/Hogares.sav")
consumo <- read_sav("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/ENCV Guatemala/Agregado.Consumo.sav")
personas <- read_sav("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/ENCV Guatemala/Personas.sav")
tierra2 <- read_sav("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/ENCV Guatemala/Tierra2.sav")

design <-
  hogares |> 
  select(DEPTO:FACTOR, TOTAL_PERS) |> 
  as_factor()

pov <-
  consumo |> 
  mutate(NO_HOGAR,
         non_poor = pobreza == 3,  # TRUE if above national poverty line| 3 = no-pobre
         .keep = "none")


# Education: achievement = attending school (7–15 years old)
edu <-
  personas |> 
  summarize(.by = NO_HOGAR,
            attendance = any(between(PPA03, 7, 15) & P06B05 != 2)) # != 2 means attending

# Housing: achievement = access to improved sources
viv <-
  hogares |> 
  mutate(NO_HOGAR,
         water = P01D06 != 5,        # flip so TRUE means improved
         sanitation = P01D17 != 3,   # flip so TRUE means improved
         electricity = P01D24 != 2,  # flip so TRUE means has electricity
         .keep = "none")

# Keep IDP logic the same
disp <- hogares |> 
  mutate(NO_HOGAR,
         violence = (P02B02A == 1 | P02B02B == 1 | P02B02C == 1 | P02B02D == 1 | P02B02E == 1) &
           (P02B03A == 1 | (P02B03A == 2 & P02B04A %in% 1:2)),
         disaster = (P02B02F == 1) &
           (P02B03A == 1 | (P02B03A == 2 & P02B04A %in% 1:2)),
         idp = violence | disaster,
         .keep = "none")

# Merge all datasets
data <- 
  list(design, disp, pov, edu, viv) |> 
  reduce(full_join)|> 
  mutate(achievement = 
           1/3*non_poor +
           1/3*attendance + #1/6*completed +
           1/9*water + 1/9*sanitation + 1/9*electricity,
         mpm = achievement > 0.67)




#POVERTY RATE TO COMPARE TO WORLD BANK NUMBER (to cross check whether poverty line is correct)
poverty_rate <- 1 - weighted.mean(data$non_poor, w = data$FACTOR * data$TOTAL_PERS, na.rm = TRUE)

#benchmark <- weighted.mean(filter(data, !idp)$achievement, 
#                           filter(data, !idp)$FACTOR, 
#                           na.rm = T)
data_host <- data %>%
  filter(idp==F) 
  
benchmark <- mean(data_host$achievement, na.rm = T)
  


plot_data <- data |>
  summarize(.by = idp,
            across(non_poor:mpm, \(x) weighted.mean(x, FACTOR, na.rm = TRUE))) |>
  pivot_longer(-idp, names_to = "ind", values_to = "p") |>
  # Keep only 5 indicators
  filter(ind %in% c("non_poor", "attendance", "water", "sanitation", "electricity")) |>
  mutate(
    ind = factor(ind, levels = c("electricity", "sanitation", "water","attendance" ,"non_poor"),
                 labels = c("Electricity", "Sanitation",
                            "Drinking water",  "Educational enrollment","Monetary self-reliance")),
    group = ifelse(idp, "NDP", "Host")
  )

# Plot in Colombia style
ggplot(plot_data, aes(x = p, y = ind)) +
  # Bars for IDP
  geom_col(data = subset(plot_data, group == "NDP"),
           aes(fill = group),
           width = 0.8) +
  # Dots for Host
  geom_point(data = subset(plot_data, group == "Host"),
             aes(color = group),
             size = 3,
             position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("NDP" = "#0072BC")) +
  scale_color_manual(values = c("Host" = "#B41C37")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "",
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
    title = element_blank()

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/ENCV Guatemala/ach_figure.png", width = 6, height = 3.5, dpi = 300)


# Filter IDP households for Guatemala
data_idp_gt <- data %>% 
  filter(idp)  # or filter(hh_popgroup == "IDP") if you have that column

national_avg <- unique(data$benchmark) # should be 1 value

# Create the histogram
ggplot(data_idp_gt, aes(x = achievement)) +
  geom_histogram(binwidth = 0.05, fill = "#0072BC", color = "white", boundary = 0) +
  
  # Vertical line for national average
  geom_vline(xintercept = benchmark, 
             color = "#B41C37", linetype = "dashed", size = 1) +
  
  # Add text label for national average
  geom_text(
    data = data.frame(x = benchmark, y = 250),  # adjust y for placement
    aes(x = x, y = y),
    label = scales::number(benchmark, accuracy = 0.01),
    inherit.aes = FALSE,
    hjust = 1.2, color = "#B41C37", fontface = "bold"
  ) +
  
  labs(
    y = "Number of Households"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  theme_unhcr() +
  theme(
    title = element_blank(), 
    axis.title.x = element_blank()
  )

ggsave("C:/Users/LEOPOLD/OneDrive - UNHCR/Work/2_EGRISS/ENCV Guatemala/histogram_ach.png", width = 6, height = 3, dpi = 300)






#EXIT NUMBERS
exit_prebounding <- mean(data_idp_gt$achievement > benchmark, na.rm = TRUE)
