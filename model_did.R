### model_did.R
# ---------------------------
# Difference-in-Differences (DiD) model – GIGF19

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

#write.csv(df_core, "df_core[].csv", row.names = FALSE)

df_did <- df_core

# DiD variables
df_did$treated <- ifelse(df_did$country == "germany", 1, 0)
df_did$post <- ifelse(df_did$year >= 1871, 1, 0)
df_did$did <- df_did$treated * df_did$post

# known shocks
df_did$fr_third_republic <- ifelse(df_did$country == "france" 
                                   & df_did$year >= 1870, 1, 0)

df_did$crisis_1873 <- ifelse(df_did$year >= 1873 & df_did$year <= 1875, 1, 0)



# # #
did_model <- plm(
  gdppc_mpd ~ treated + post + did + fr_third_republic + crisis_1873,
  data = df_did,
  model = "within",
  index = c("country", "year")
)

summary(did_model)

coeftest(did_model, vcov = vcovHC(did_model, type = "HC1"))

#OLS
did_model_ols <- lm(
  gdppc_mpd ~ treated + post + did + fr_third_republic + crisis_1873,
  data = df_did
)

summary(did_model_ols)



# # #
did_model_extended <- plm(
  gdppc_mpd ~ treated + post + did +
    fr_third_republic + crisis_1873 +
    inflation + armed_conflicts_internal,
  data = df_did,
  model = "within",
  index = c("country", "year")
)

summary(did_model_extended)

coeftest(did_model_extended, vcov = vcovHC(did_model_extended, type = "HC1"))


did_model_extended_ols <- lm(
  gdppc_mpd ~ treated + post + did +
    fr_third_republic + crisis_1873 +
    inflation + armed_conflicts_internal,
  data = df_did
)

summary(did_model_extended_ols)























# # # # #
# # # A NEW IDEA

df_did <- df_did %>%
  select(
    country, year, gdppc_mpd,
    treated, post, did,
    fr_third_republic, crisis_1873,
    inflation, 
    armed_conflicts_internal,
    armed_conflicts_international,
    ratio_rail_pc,
    ratio_trade
  )

#write.csv(df_did, "df_did_clean.csv", row.names = FALSE)


library(readxl)
df_did_au_uk <- read_excel("df_did_au_uk.xlsx")
View(df_did_au_uk)

#write.csv(df_did_au_uk, "df_did_au_uk.csv", row.names = FALSE)


df_did_au_uk$treated <- ifelse(df_did_au_uk$country == "germany", 1, 0)

df_did_au_uk$post <- ifelse(df_did_au_uk$year >= 1871, 1, 0)

df_did_au_uk$did <- df_did_au_uk$treated * df_did_au_uk$post

df_did_au_uk$crisis_1873 <- ifelse(df_did_au_uk$year >= 1873 
                                   & df_did_au_uk$year <= 1875, 1, 0)

df_did_au_uk$fr_third_republic <- ifelse(df_did_au_uk$country == "france" 
                                         & df_did_au_uk$year >= 1870, 1, 0)

df_did_au_uk$de_post_tariff_1879 <- ifelse(df_did_au_uk$country == "germany" 
                                           & df_did_au_uk$year >= 1879, 1, 0)

df_did_au_uk$uk_trade_depression <- ifelse(df_did_au_uk$country == "uk" 
                                           & df_did_au_uk$year >= 1875 
                                           & df_did_au_uk$year <= 1895, 1, 0)

df_did_au_uk$austria_constitution <- ifelse(df_did_au_uk$country == "austria" 
                                            & df_did_au_uk$year >= 1867, 1, 0)


#str(df_did_au_uk)

df_did_au_uk$gdppc_mpd <- as.numeric(df_did_au_uk$gdppc_mpd)
df_did_au_uk$inflation <- as.numeric(df_did_au_uk$inflation)
df_did_au_uk$ratio_rail_pc <- as.numeric(df_did_au_uk$ratio_rail_pc)
df_did_au_uk$ratio_trade <- as.numeric(df_did_au_uk$ratio_trade)

#str(df_did_au_uk)

# # #
did_model_4c <- plm(
  gdppc_mpd ~ treated + post + did +
    crisis_1873 + fr_third_republic +
    inflation + armed_conflicts_internal,
  data = df_did_au_uk,
  model = "within",
  index = c("country", "year")
)

summary(did_model_4c)
coeftest(did_model_4c, vcov = vcovHC(did_model_4c, type = "HC1", cluster = "group"))




# # # # #

did_model_interact <- plm(
  gdppc_mpd ~ treated + post + did + 
    inflation + did:inflation +
    crisis_1873 + fr_third_republic + 
    armed_conflicts_internal,
  data = df_did_au_uk,
  model = "within",
  index = c("country", "year")
)

coeftest(did_model_interact, vcov = vcovHC(did_model_interact, type = "HC1", cluster = "group"))

# parallel trends 
ggplot(df_did_au_uk, aes(x = year, y = gdppc_mpd, group = country)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_vline(xintercept = 1871, linetype = "dashed") +
  labs(
    title = "GDP per Capita Trends in Europe (1850–1914)",
    subtitle = "Dashed line = German unification (1871)",
    x = "Year", y = "GDP per capita (MPD)"
  ) +
  facet_wrap(~country) +
  theme_minimal()



# fitted-actual
fitted_vals <- data.frame(
  fitted = fitted(did_model_4c),
  country = index(did_model_4c)$country,
  year = as.numeric(as.character(index(did_model_4c)$year))  # convert factor → character → numeric
)

df_plot <- df_did_au_uk %>%
  inner_join(fitted_vals, by = c("country", "year"))

df_plot$gdppc_mpd <- as.numeric(df_plot$gdppc_mpd)


ggplot(df_plot, aes(x = fitted, y = gdppc_mpd)) +
  geom_point(alpha = 0.5, color = "black", size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1) +
  facet_wrap(~country) +
  labs(
    title = "Fitted vs Actual GDP per Capita by Country (1850–1914)",
    x = "Fitted values",
    y = "Actual GDP per capita (MPD)"
  ) +
  theme_minimal()


# # # # # # # # # #

stargazer(
  did_model_ols, did_model_extended_ols,
  type = "latex",
  title = "Difference-in-Differences Regression Results (OLS)",
  column.labels = c("Basic OLS", "Extended OLS"),
  dep.var.labels = "GDP per Capita (MPD)",
  covariate.labels = c(
    "Treated", "Post", "DiD Interaction", 
    "Third Republic (FR)", "Crisis 1873", 
    "Inflation", "Internal Conflict"
  ),
  out = "ols_did_table.tex"
)

stargazer(
  did_model, did_model_extended, did_model_4c, did_model_interact,
  type = "latex",
  title = "Difference-in-Differences Regression Results (Fixed Effects)",
  column.labels = c("Basic FE", "Extended FE", "FE 4 Countries", "FE Interaction"),
  dep.var.labels = "GDP per Capita (MPD)",
  covariate.labels = c(
    "Treated", "Post", "DiD Interaction", 
    "Third Republic (FR)", "Crisis 1873", 
    "Inflation", "Internal Conflict", "DiD × Inflation"
  ),
  out = "fe_did_table.tex"
)

# # #
se_ols_basic <- sqrt(diag(vcovHC(did_model_ols, type = "HC1")))
se_ols_ext   <- sqrt(diag(vcovHC(did_model_extended_ols, type = "HC1")))

se_fe_4c     <- sqrt(diag(vcovHC(did_model_4c, type = "HC1", cluster = "group")))
se_fe_inter  <- sqrt(diag(vcovHC(did_model_interact, type = "HC1", cluster = "group")))

stargazer(
  did_model_ols,
  did_model_extended_ols,
  did_model_4c,
  did_model_interact,
  type = "latex",
  se = list(se_ols_basic, se_ols_ext, se_fe_4c, se_fe_inter),
  title = "Difference-in-Differences Regression Results (Cluster-Robust SEs)",
  column.labels = c("OLS Basic", "OLS Extended", "FE 4C", "FE w/ Interaction"),
  dep.var.labels = "GDP per Capita (MPD)",
  covariate.labels = c(
    "Treated", "Post", "DiD Interaction",
    "Third Republic (FR)", "Crisis 1873",
    "Inflation", "Internal Conflict", "DiD × Inflation"
  ),
  model.names = FALSE,
  no.space = TRUE,
  out = "did_clustered_table.tex"
)



#######

if (!dir.exists("figures")) dir.create("figures")

fitted_vals <- data.frame(
  fitted = fitted(did_model_4c),
  country = index(did_model_4c)$country,
  year = as.numeric(as.character(index(did_model_4c)$year))
)

df_plot <- df_did_au_uk %>%
  inner_join(fitted_vals, by = c("country", "year")) %>%
  mutate(gdppc_mpd = as.numeric(gdppc_mpd))

plot_c1 <- ggplot(df_plot, aes(x = fitted, y = gdppc_mpd)) +
  geom_point(alpha = 0.5, color = "black", size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1) +
  facet_wrap(~country) +
  labs(
    title = "Fitted vs Actual GDP per Capita by Country (1850–1914)",
    x = "Fitted values",
    y = "Actual GDP per capita (MPD)"
  ) +
  theme_minimal()

ggsave("figures/Figure_C1_fitted_vs_actual_modelC.png", plot = plot_c1, width = 8, height = 5, dpi = 300)



#####


resid_vals <- data.frame(
  residuals = residuals(did_model_4c),
  fitted = fitted(did_model_4c),
  country = index(did_model_4c)$country,
  year = as.numeric(as.character(index(did_model_4c)$year))
)

plot_c2 <- ggplot(resid_vals, aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.5, color = "black", size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
  facet_wrap(~country) +
  coord_cartesian(ylim = c(-2000, 2000)) +
  labs(
    title = "Residuals vs Fitted by Country (Model 4C)",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave("figures/Figure_C2_residuals_by_country_modelC.png", plot = plot_c2, width = 8, height = 5, dpi = 300)

#####

plot_c3 <- ggplot(df_did_au_uk, aes(x = year, y = gdppc_mpd, group = country)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_vline(xintercept = 1871, linetype = "dashed") +
  labs(
    title = "GDP per Capita Trends in Europe (1850–1914)",
    subtitle = "Dashed line = German unification (1871)",
    x = "Year", y = "GDP per capita (MPD)"
  ) +
  facet_wrap(~country) +
  theme_minimal()

ggsave("figures/Figure_C3_parallel_trends_modelC.png", plot = plot_c3, width = 8, height = 5, dpi = 300)



####

plot_c4 <- ggplot(resid_vals, aes(x = residuals)) +
  geom_histogram(color = "black", fill = "gray80", bins = 30) +
  labs(
    title = "Histogram of Residuals (Model 4C)",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave("figures/Figure_C4_residuals_hist_modelC.png", plot = plot_c4, width = 6, height = 4, dpi = 300)


#####


plot_c5 <- ggplot(resid_vals, aes(x = year, y = residuals)) +
  geom_line(color = "black", linewidth = 0.8) +
  facet_wrap(~country) +
  labs(
    title = "Residuals over Time by Country (Model 4C)",
    x = "Year",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave("figures/Figure_C5_residuals_time_modelC.png", plot = plot_c5, width = 8, height = 5, dpi = 300)

