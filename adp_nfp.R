library(tidyverse)
library(broom)
library(patchwork)
library(sandwich); library(lmtest)

# 1) Fetch & scale once
nfp <- fredr("PAYEMS") %>% transmute(date, NFP = value/ 1e3)
adp <- fredr("ADPMNUSNERSA") %>% transmute(date, ADP = value/ 1e6)

jobs <- inner_join(nfp, adp, by = "date") %>%
  filter(date >= as.Date("2010-01-01")) %>%
  mutate(Spread = NFP - ADP,
         dNFP = NFP - lag(NFP),
         dADP = ADP - lag(ADP)) %>%
  drop_na()

summary(select(jobs, NFP, ADP))

# 2) Levels model (descriptive only)
m_levels <- lm(ADP ~ NFP, data = jobs)

# 3) Change model (for replacement/nowcast use-case)
m_change <- lm(dNFP ~ dADP, data = jobs)

# Robust SEs
lev_coefs <- coeftest(m_levels, vcov = NeweyWest(m_levels, lag = 6))
chg_coefs <- coeftest(m_change, vcov = NeweyWest(m_change, lag = 6))

# 4) Rolling out-of-sample test (simple)
split_date <- as.Date("2019-12-01")
train <- jobs %>% filter(date <= split_date)
test  <- jobs %>% filter(date >  split_date)

m_train <- lm(dNFP ~ dADP, data = train)
test <- test %>%
  mutate(pred = predict(m_train, newdata = test),
         err  = dNFP - pred)

mae <- mean(abs(test$err), na.rm = TRUE)
rmse <- sqrt(mean(test$err^2, na.rm = TRUE))

# 5) Plots (cleaned)
p1 <- jobs %>% 
  pivot_longer(c(NFP, ADP)) %>%
  ggplot(aes(date, value, color = name)) +
  geom_line() +
  labs(y = "Employment (millions)", x = NULL, color = NULL) +
  theme_minimal()

co <- coef(m_levels)
p2 <- ggplot(jobs, aes(NFP, ADP)) +
  geom_point(alpha = .6) +
  geom_abline(intercept = co[1], slope = co[2]) +
  annotate("label", x = min(jobs$NFP)+2, y = min(jobs$ADP)+20,
           hjust = 0,
           label = glue("ADP = {round(co[1],3)} + {round(co[2],3)}×NFP\n",
                        "R² = {round(glance(m_levels)$r.squared,3)}; ",
                        "p(slope) = {signif(tidy(m_levels)$p.value[2],3)}")) +
  labs(x = "NFP (millions)", y = "ADP (millions)") +
  theme_minimal()

p3 <- ggplot(jobs, aes(Spread)) +
  geom_histogram(aes(y = after_stat(density)), alpha = .3) +
  geom_density() +
  geom_vline(xintercept = mean(jobs$Spread), linetype = 2) +
  labs(x = "NFP – ADP (thousand)", y = "Density") +
  theme_minimal()

p4 <- jobs %>%
  mutate(NFP_bin = cut(NFP, breaks = c(-Inf,140,150,Inf),
                       labels = c("≤140m","140–150m","≥150m"))) %>%
  ggplot(aes(NFP_bin, Spread)) +
  geom_boxplot() +
  labs(x = "NFP level", y = "Spread (millions)") +
  theme_minimal(base_size = 12)

(p1+p2)/(p3+p4)
ggsave("adp_nfp_2.png", width = 7.5, height = 6.3)
