library(tidyverse) 
library(readxl) 
library(scales) 
library(sysfonts) 
library(showtextdb) 
library(showtext) 
library(glue)
library(ggtext)
library(fredr)
library(patchwork)

fredr_set_key("0c5fd2514c7d98427fe3c931e2fcb244") 

# Assess if the ADP's job-data can be an alternative of NFP by BLD

# Overall unemployment 
nfp_data <- fredr(series_id = "PAYEMS") %>% 
  select(date, nfp = value) 

adp <- fredr("ADPMNUSNERSA") %>% 
  select(date, ner = value) %>% 
  mutate(ner = ner/1000)

jobs <- nfp_data %>% 
  left_join(., adp, by = "date") %>% 
  filter(date >= "2010-01-01") %>% 
  mutate("NFP" = nfp/1000, "ADP" = ner/1000) %>% 
  select(date, NFP, ADP) %>% 
  mutate("Spread" = NFP - ADP) 

a <- jobs %>% 
  pivot_longer(-date, names_to = "data", values_to = "us_jobs") %>% 
  ggplot(aes(x = date, y = us_jobs, colour = data)) + 
  geom_line() +
  labs(x = NULL,
       y = "US Non-farm Payroll(million)") +
  theme(
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.position = "top",
    panel.background = element_blank()
  )

model <- lm(ADP ~ NFP, data = jobs) 

summary(model) 
coefficients <- coef(model) 
intercept <- coefficients[1] 
slope <- round(coefficients[2],3 )
r.squared <- round(summary(model)$r.squared, 3 )
"p-value" <- "< 2.2e-16"

MIN <- summary(jobs$Spread)[1]
FIRST_Qu <- summary(jobs$Spread)[2]
Median <- round(summary(jobs$Spread)[3],3)
Mean <- round(summary(jobs$Spread)[4], 3)
THIRD_Qu <- summary(jobs$Spread)[5]
MAX <- summary(jobs$Spread)[6]
SD = round(sd(jobs$Spread), 3)

annotation1 <- glue("min: {MIN}")
annotation2 <- glue("1st Qu.: {FIRST_Qu}")
annotation3 <- glue("median: {Median}")
annotation4 <- glue("mean: {Mean}")
annotation5 <- glue("3rd Qu: {THIRD_Qu}")
annotation6 <- glue("max: {MAX}")
annotation7 <- glue("sd: {SD}")

# "+SD" = MEAN + SD

MEAN <- round(mean(jobs$Spread), 3) 

median(jobs$Spread)

b <- jobs %>% 
  ggplot(aes(x = NFP, y = ADP)) + 
  geom_point() + geom_smooth(method = lm) + 
  annotate("text", x = 135, y =130,
           label = glue("y = x * {slope} \nr-squared = {r.squared}\np-value: < 2.2e-16"), 
           colour = "red", 
           fontface = "italic",
           hjust = 0) +
  labs(x = "NFP(mill)",
       y = "ADP(mill)") +
  theme(
    panel.background = element_blank()
  )
  
c <- jobs %>% 
  ggplot(aes(x = Spread)) + 
  geom_histogram(alpha = 0.3) +
  geom_density(colour = "red") +
  geom_vline(xintercept = MEAN, colour = "green") +
  geom_vline(xintercept = MEAN + SD, colour = "blue", linetype = "dashed") +
  geom_vline(xintercept = MEAN - SD, colour = "blue", linetype = "dashed") +
  geom_vline(xintercept = MEAN + 2*SD, colour = "blue", linetype = "dotted") +
  geom_vline(xintercept = MEAN - 2*SD, colour = "blue", linetype = "dotted") +
  annotate("text",
           x = as.numeric(mean(jobs$Spread)), y = 75, label = glue("mean\n{MEAN}")) +
  annotate("text",
           x = 13, y = 70,
           label = glue("{annotation1}"), hjust = 0) +
  annotate("text",
           x = 13, y = 65,
           label = glue("{annotation2}"), hjust = 0) +
  annotate("text",
           x = 13, y = 60,
           label = glue("{annotation3}"), hjust = 0) +
  annotate("text",
           x = 13, y = 55,
           label = glue("{annotation4}"), hjust = 0) +
  annotate("text",
           x = 13, y = 50,
           label = glue("{annotation5}"), hjust = 0) +
  annotate("text",
           x = 13, y = 45,
           label = glue("{annotation6}"), hjust = 0) +
  annotate("text",
           x = 13, y = 40,
           label = glue("{annotation7}"), hjust = 0) +
  scale_x_log10() +
  labs(x = "Spread(million)",
       y = "Percentage") +
  theme(
    panel.background = element_blank()
  )

jobs %>% arrange(Spread) %>% 
  slice_tail(n = 10) 

d <- jobs %>% 
  ggplot(aes(x = NFP, y = Spread)) + 
  geom_boxplot() +
  theme(
    panel.background = element_blank()
  )

e <- jobs %>% 
  mutate(NFP = case_when(
    NFP < 140 ~ "1",
    NFP >= 140 & NFP <= 150 ~ "2",
    NFP > 150 ~ "3")
  ) %>% 
  ggplot(
    aes(x = NFP, y = Spread)
  ) +
  geom_boxplot() +
  scale_x_discrete (breaks = c(1, 2, 3),
                     labels = c("~140mil", "140mil\n~ 150mil", "150mil~")) +
  labs(y = "Spread(million)") + 
  theme(
    panel.background = element_blank(),
    axis.line = element_line(),
    axis.text = element_text(size = 14)
  ) 

(a+b)/(c+e)

ggsave("adp_nfp.png", width = 7.5, height = 6.3)

# Load and prepare the full data
adp <- adp %>%
  mutate(
         year = year(date),
         month = month(date, label = TRUE))

# monthly data
adp %>% 
  mutate(monthly_change = c(NA, diff(ner))) %>% 
  ggplot(aes(x = date, y = monthly_change)) +
  geom_col()

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
    show.legend = FALSE, size = 5
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
    caption = "<i>ADP, FRED(Federal Reserve Economic Data), by Takayuki Tamura"
  ) +
  theme_classic() +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold", size = 14, margin = margin(t= 10, b =10)),
    plot.caption.position = "panel",
    plot.caption = element_markdown(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )

ggsave("adp.us_job_creation_1.png", height = 6, width = 6.5)


