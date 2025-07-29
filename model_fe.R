### model_fe.R
# ---------------------------
# Fixed Effects Panel Model – GIGF19

library(tidyverse)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(readxl)
library(stargazer)
library(ggplot2)
library(car)
library(broom)

df <- read_excel("data_GIGF19.xlsx", 
                          sheet = "panel_data")
View(df)
###

#glimpse(df)
#summary(df)

#write.csv(df, "df.csv", row.names = FALSE)

# Core period: 1850–1914
df_core <- df %>% filter(year >= 1850, year <= 1914)
#glimpse(df_core)
#summary(df_core)

# Extended period: 1825–1914
df_full <- df %>% filter(year >= 1825, year <= 1914)
#glimpse(df_full)
#summary(df_full)

#str(df_core)
#str(df_full)

na_summary_core <- df_core %>%
  summarise(across(where(is.numeric), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_share_core")

na_summary_full <- df_full %>%
  summarise(across(where(is.numeric), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_share_full")

na_compare <- left_join(na_summary_core, na_summary_full, by = "variable") %>%
  mutate(diff = na_share_full - na_share_core) %>%
  arrange(desc(diff))
View(na_compare)

good_vars <- na_compare %>%
  filter(na_share_core <= 0.3) %>%
  pull(variable)


df_core <- df_core %>%
  select(country, year, all_of(good_vars))

df_full <- df_full %>% 
  select(country, year, all_of(good_vars))

#summary(df_core)
#summary(df_full)

#write.csv(df_core, "df_core_1850_1914.csv", row.names = FALSE)
#write.csv(df_full, "df_full_1825_1914.csv", row.names = FALSE)

df_full <- df_full %>%
  mutate(price_ratio = cpi_col / wpi,
         ratio_govexp_gdppc = gov_exp / gdppc_mpd,
         ratio_trade = ext_trade_agg_curr_val_exp / ext_trade_agg_curr_val_imp,
         ratio_wheat_pc = wheat_output / pop,
         ratio_wage_share = labourers_real_wage / gdppc_mpd,
         ratio_bn_pc = bn_circ / pop,
         ratio_rail_pc = rail_length / pop
  )

df_core <- df_core %>%
  mutate(price_ratio = cpi_col / wpi,
         ratio_govexp_gdppc = gov_exp / gdppc_mpd,
         ratio_trade = ext_trade_agg_curr_val_exp / ext_trade_agg_curr_val_imp,
         ratio_wheat_pc = wheat_output / pop,
         ratio_wage_share = labourers_real_wage / gdppc_mpd,
         ratio_bn_pc = bn_circ / pop,
         ratio_rail_pc = rail_length / pop
  )




##### MODEL A

df_full <- df_full %>%
  mutate(tariff_dummy = ifelse(country == "germany" & year >= 1879, 1, 0))


model_vars_fe_base <- c(
  "gdppc_mpd",
  "indices_prod_index",
  "rail_length",
  "gov_exp",
  "exchange_rates_to_uk_pound",
  "long_term_government_bond_yield",
  "labourers_real_wage"
)

model_vars_fe_ratios <- c(
  "gdppc_mpd",
  "price_ratio",
  "ratio_govexp_gdppc",
  "ratio_trade",
  "ratio_wheat_pc",
  "ratio_wage_share",
  "ratio_bn_pc",
  "ratio_rail_pc"
)

model_vars_fe_extended <- c(
  "gdppc_mpd",
  "indices_prod_index",
  "rail_length",
  "gov_exp",
  "exchange_rates_to_uk_pound",
  "long_term_government_bond_yield",
  "labourers_real_wage",
  "price_ratio",
  "ratio_govexp_gdppc",
  "ratio_trade",
  "ratio_wheat_pc",
  "ratio_wage_share",
  "ratio_bn_pc",
  "ratio_rail_pc",
  "tariff_dummy"
)


pdata_A_base <- pdata.frame(df_full %>% select(country, year, 
                            all_of(model_vars_fe_base)) %>% drop_na(),
                            index = c("country", "year"))

pdata_A_rat <- pdata.frame(df_full %>% select(country, year, 
                           all_of(model_vars_fe_ratios)) %>% drop_na(),
                           index = c("country", "year"))

pdata_A_ext <- pdata.frame(df_full %>% select(country, year,
                           all_of(model_vars_fe_extended)) %>% drop_na(),
                           index = c("country", "year"))


# Est

#fe_A_ext <- plm(gdppc_mpd ~ . - country - year, data = pdata_A_ext, model = "within")

#robust_se_A_ext <- coeftest(fe_A_ext, vcov. = vcovHC(fe_A_ext, type = "HC1", cluster = "group"))
#print(robust_se_A_ext)

#vif(fe_A_ext)

#df_vif <- as.data.frame(pdata_A_ext)
#lm_model <- lm(gdppc_mpd ~ ., data = df_vif)
#vif(lm_model)

alias(lm_model)

lm_model_clean <- lm(gdppc_mpd ~ . - country - year, data = df_vif)
vif(lm_model_clean)

model_vars_fe_clean <- c(
  "gdppc_mpd",
  "indices_prod_index",
  "exchange_rates_to_uk_pound",
  "long_term_government_bond_yield",
  "price_ratio",
  "ratio_trade",
  "ratio_wheat_pc",
  "tariff_dummy"
)

pdata_clean <- pdata.frame(df_full %>% select(country, year, all_of(model_vars_fe_clean)) %>% drop_na(), 
                           index = c("country", "year"))
fe_clean <- plm(gdppc_mpd ~ . - country - year, data = pdata_clean, model = "within")
robust_se_clean <- coeftest(fe_clean, vcov. = vcovHC(fe_clean, type = "HC1", cluster = "group"))
robust_se_clean

# # #

model_vars_fe_clean_tariff <- c(
  "gdppc_mpd",
  "indices_prod_index",
  "exchange_rates_to_uk_pound",
  "long_term_government_bond_yield",
  "price_ratio",
  "ratio_trade",
  "ratio_wheat_pc",
  "tariff_dummy"  # Re-added
)

pdata_clean_tariff <- pdata.frame(
  df_full %>% select(country, year, all_of(model_vars_fe_clean_tariff)) %>% drop_na(),
  index = c("country", "year")
)

fe_clean_tariff <- plm(
  gdppc_mpd ~ . - country - year,
  data = pdata_clean_tariff,
  model = "within"
)


robust_se_clean_tariff <- coeftest(
  fe_clean_tariff,
  vcov. = vcovHC(fe_clean_tariff, type = "HC1", cluster = "group")
)
robust_se_clean_tariff


summary(fe_clean_tariff)

# # #

ols_clean_tariff <- lm(
  gdppc_mpd ~ . - country - year,
  data = df_full %>% select(country, year, all_of(model_vars_fe_clean_tariff)) %>% drop_na()
)

coeftest(ols_clean_tariff, vcov. = vcovHC(ols_clean_tariff, type = "HC1"))


pwartest(fe_clean_tariff)

plmtest(fe_clean_tariff, type = "bp")

pcdtest(fe_clean_tariff, test = "cd")

pFtest(fe_clean_tariff, ols_clean_tariff)


# Driscoll–Kraay SEs
robust_se_dk <- coeftest(fe_clean_tariff,vcovSCC(fe_clean_tariff, type = "HC1", maxlag = 2))
print(robust_se_dk)


dk_se_fe <- sqrt(diag(vcovSCC(fe_clean_tariff, type = "HC1", maxlag = 2)))

hc1_se_ols <- sqrt(diag(vcovHC(ols_clean_tariff, type = "HC1")))

stargazer(
  fe_clean_tariff, ols_clean_tariff,
  se = list(dk_se_fe, hc1_se_ols),
  type = "latex",
  title = "Comparison of Fixed Effects and Pooled OLS Models (Clean Specification with Tariff Dummy)",
  label = "tab:fe_ols_tariff",
  column.labels = c("Fixed Effects", "Pooled OLS"),
  dep.var.labels = "GDP per capita (gdppc\\_mpd)",
  covariate.labels = c(
    "Industrial Production Index",
    "Exchange Rate (to GBP)",
    "Bond Yield",
    "Consumer-Producer Price Ratio",
    "Trade Ratio (Exp/Imp)",
    "Wheat Output per Capita",
    "Tariff Dummy (DE 1879+)"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  no.space = TRUE,
  header = FALSE,
  font.size = "small"
)


df_plot <- data.frame(
  fitted = fitted(fe_clean_tariff),
  actual = pdata_clean_tariff$gdppc_mpd,
  country = pdata_clean_tariff$country,
  year = pdata_clean_tariff$year
)

ggplot(df_plot, aes(x = fitted, y = actual, shape = country)) +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_shape_manual(values = c("france" = 17, "germany" = 1)) +  # triangle, square
  labs(
    title = "Fixed Effects Model: Fitted vs. Actual GDP per Capita",
    x = "Fitted Values",
    y = "Actual GDP per Capita",
    shape = "Country"
  ) +
  theme_bw() -> p_fe_plot

ggsave("fig_fe_fitted_vs_actual.png", plot = p_fe_plot, width = 6, height = 4, dpi = 300)


#####

res_df <- data.frame(
  fitted = fitted(fe_clean_tariff),
  residuals = resid(fe_clean_tariff),
  country = pdata_clean_tariff$country
)

ggplot(res_df, aes(x = fitted, y = residuals, shape = country)) +
  geom_point(size = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_shape_manual(values = c("france" = 17, "germany" = 1)) +
  labs(
    title = "Residuals vs. Fitted Values",
    x = "Fitted Values",
    y = "Residuals",
    shape = "Country"
  ) +
  theme_bw() -> p_resid_plot

ggsave("fig_residuals_vs_fitted.png", plot = p_resid_plot, width = 6, height = 4, dpi = 300)


#####

df_ts <- data.frame(
  year = pdata_clean_tariff$year,
  country = pdata_clean_tariff$country,
  actual = pdata_clean_tariff$gdppc_mpd,
  fitted = fitted(fe_clean_tariff)
)

df_ts_long <- df_ts %>%
  pivot_longer(cols = c("actual", "fitted"), names_to = "type", values_to = "gdppc")
df_ts_long$year <- as.numeric(as.character(df_ts_long$year))

ggplot(df_ts_long, aes(x = year, y = gdppc, linetype = type, group = interaction(type, country))) +
  geom_line() +
  facet_wrap(~country) +
  scale_x_continuous(breaks = seq(1850, 1914, by = 10)) +
  scale_linetype_manual(values = c("actual" = "solid", "fitted" = "dashed")) +
  labs(
    title = "Actual vs. Fitted GDP per Capita Over Time",
    x = "Year",
    y = "GDP per Capita",
    linetype = "Series"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p_ts

ggsave("fig_ts_actual_vs_fitted.png", plot = p_ts, width = 7, height = 4.5, dpi = 300)


#####

coef_df <- tidy(fe_clean_tariff, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  labs(
    title = "Fixed Effects Model Coefficient Estimates",
    x = "Coefficient Estimate",
    y = "Variable"
  ) +
  theme_bw() -> p_coef

ggsave("fig_coef_plot.png", plot = p_coef, width = 6, height = 4.5, dpi = 300)

#####

vif_values <- vif(lm_model_clean)
vif_df <- data.frame(
  variable = names(vif_values),
  VIF = as.numeric(vif_values)
)

ggplot(vif_df, aes(x = reorder(variable, VIF), y = VIF)) +
  geom_col(fill = "grey50") +
  geom_hline(yintercept = 10, linetype = "longdash", color = "black", linewidth = 0.6) +
  coord_flip() +
  labs(
    title = "Variance Inflation Factors (VIF)",
    x = "Variable",
    y = "VIF"
  ) +
  theme_bw() -> p_vif

ggsave("fig_vif_plot.png", plot = p_vif, width = 6, height = 4.5, dpi = 300)



































