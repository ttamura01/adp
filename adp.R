library(tidyverse) 
library(readxl) 
library(scales) 
library(sysfonts) 
library(showtextdb) 
library(showtext) 
library(ggtext)
library(fredr)

fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244") 

# Overall unemployment 
nfp_data <- fredr(series_id = "PAYEMS") %>% 
  select(date, nfp = value) 

adp <- fredr("ADPMNUSNERSA") %>% 
  select(date, ner = value) %>% 
  mutate(ner = ner/1000)

jobs <- nfp_data %>% 
  left_join(., adp, by = "date") %>% 
  filter(date >= "2010-01-01") %>% 
  mutate(nfp_mil = nfp/1000, ner_mil = ner/1000) %>% 
  select(date, nfp_mil, ner_mil) %>% 
  mutate("spread" = nfp_mil - ner_mil) 

jobs %>% 
  pivot_longer(-date, names_to = "data", values_to = "us_jobs") %>% 
  ggplot(aes(x = date, y = us_jobs, colour = data)) + 
  geom_line() 

model <- lm(ner_mil ~ nfp_mil, data = jobs) 

summary(model) 
coefficients <- coef(model) 
intercept <- coefficients[1] 
slope <- coefficients[2] 
r.squared <- summary(model)$r.squared 

jobs %>% 
  ggplot(aes(x = nfp_mil, y = ner_mil)) + 
  geom_point() + geom_smooth(method = lm) 

jobs %>% 
  ggplot(aes(x = spread)) + 
  geom_density() 

jobs %>% arrange(spread) %>% 
  slice_tail(n = 10) 

jobs %>% 
  ggplot(aes(x = nfp_mil, y = spread)) + 
  geom_boxplot() 

jobs$spread 

summary(jobs$spread) 

sd(jobs$spread) 

mean(jobs$spread) 

median(jobs$spread)

# Load and prepare the full data
adp <- adp %>%
  mutate(
         year = year(date),
         month = month(date, label = TRUE))

# Get December NFP of previous year for baseline
dec_adp_prev_year <- adp %>%
  filter(month(date) == 12) %>%
  mutate(year = year(date) + 1) %>%
  select(year, ner) %>%
  rename(adp_dec_prev = ner)

# Compute job creation
adp_cumulative <- adp %>%
  filter(year >= 2010 & year <= 2025) %>%
  left_join(., dec_adp_prev_year, by = "year") %>%
  mutate(job_creation = ner - adp_dec_prev) %>%
  filter(year != 2020 & !is.na(job_creation)) %>%
  mutate(line_type = case_when(
    year == 2025 ~ "highlight_2025",
    year == 2010 ~ "highlight_2010",
    # year == 2008 ~ "highlight_2008",
    # year == 2009 ~ "highlight_2009",
    TRUE ~ "normal"
  ))

# Label only years 2021–2025
label_points <- adp_cumulative %>%
  group_by(year) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  filter(year >= 2021|year == 2010)

# Plot
ggplot(adp_cumulative, aes(x = month, y = job_creation, group = year)) +
  geom_line(aes(color = line_type, size = line_type, linetype = line_type), show.legend = FALSE) +
  scale_color_manual(
    values = c(
      "highlight_2025" = "red",
      "highlight_2010" = "blue",
      "normal" = "gray70"
    ),
    labels = c("highlight_2010" = "2010", "highlight_2025" = "2025"),
    breaks = c("highlight_2010", "highlight_2025")
  ) +
  scale_size_manual(
    values = c(
      "highlight_2025" = 1.6,
      "highlight_2010" = 1.2,
      "normal" = 0.6
    ),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c(
      "highlight_2025" = "solid",
      "highlight_2010" = "solid",
      "normal" = "solid"
    ),
    guide = "none"
  ) +
  geom_text(
    data = label_points,
    aes(label = year, color = line_type),
    hjust = -0.2, vjust = 0.5,
    show.legend = FALSE, size = 6
  ) +
  scale_x_discrete(expand = expansion(add = c(0.1, 1.5))) +
  scale_y_continuous(
    limits = c(NA, NA),
    # breaks = seq(0, 8000, 2000),
    labels = scales::label_number(big.mark = ",")) +
  # labs(
  #   title = "US Job Creation by Year (2010–2025, excluding 2020)",
  #   subtitle = "Cumulative job gains from December of previous year\n2025 (red), 2010 (blue, dashed), others gray; labels shown for 2021–2025",
  #   x = "Month",
  #   y = "Cumulative Job Creation (Millions)",
  #   color = "Highlighted Years"
  # ) +
  labs(
    title = "US Job Creation by Year (2010–2025, excluding 2020)",
    subtitle = "Cumulative job creation in 2025 is the lowest since 2010",
    x = NULL,
    y = "Cumulative Job Creation (x1,000)",
    caption = "<i>FRED(Federal Reserve Economic Data)"
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold", size = 16),
    plot.caption.position = "panel",
    plot.caption = element_markdown(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )

ggsave("us_job_creation_1.png", height = 6, width = 6.5)
