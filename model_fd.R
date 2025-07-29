### model_fd.R
# ---------------------------
# First Difference (FD) model – GIGF19

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
library(purrr)
library(tibble)

df_core <- df_core %>%
  mutate(ratio_rail_passenger_pc = rail_passenger_km / pop)

#df_core

# Is it okay to include both?
df_core %>%
  select(ratio_rail_pc, ratio_rail_passenger_pc) %>%
  cor(use = "pairwise.complete.obs")

vars_fd_model <- c(
  "country", "year", "gdppc_mpd",
  "ratio_govexp_gdppc",
  "ratio_rail_pc",
  "ratio_rail_passenger_pc",
  "ratio_bn_pc",
  "ratio_wage_share",
  "ratio_wheat_pc",
  "ratio_trade",
  "armed_conflicts_internal",
  "armed_conflicts_international"
)

fd_df <- df_core %>% select(all_of(vars_fd_model))
fd_df_clean <- fd_df %>% drop_na()
pdata_fd <- pdata.frame(fd_df_clean, index = c("country", "year"))

fd_model <- plm(
  gdppc_mpd ~ ratio_govexp_gdppc +
    ratio_rail_pc +
    ratio_rail_passenger_pc +
    ratio_bn_pc +
    ratio_wage_share +
    ratio_wheat_pc +
    ratio_trade +
    armed_conflicts_internal +
    armed_conflicts_international,
  data = pdata_fd,
  model = "fd"
)

coeftest(fd_model, vcov. = function(x) vcovHC(x, type = "HC1", method = "arellano"))

# Robustness check, likely endogeneity
fd_model_nowage <- plm(
  gdppc_mpd ~ ratio_govexp_gdppc +
    ratio_rail_pc +
    ratio_rail_passenger_pc +
    ratio_bn_pc +
    ratio_wheat_pc +
    ratio_trade +
    armed_conflicts_internal +
    armed_conflicts_international,
  data = pdata_fd,
  model = "fd"
)

coeftest(fd_model_nowage, vcov. = function(x) vcovHC(x, type = "HC1", method = "arellano"))


# Robustness check, ratio_rail_pc
fd_model_norailkm <- plm(
  gdppc_mpd ~ ratio_govexp_gdppc +
    ratio_rail_passenger_pc +
    ratio_bn_pc +
    ratio_wage_share +
    ratio_wheat_pc +
    ratio_trade +
    armed_conflicts_internal +
    armed_conflicts_international,
  data = pdata_fd,
  model = "fd"
)

coeftest(fd_model_norailkm, vcov. = function(x) vcovHC(x, type = "HC1", method = "arellano"))


df_core %>%
  select(ratio_rail_pc, armed_conflicts_internal) %>%
  cor(use = "complete.obs")

lm_model <- lm(
  gdppc_mpd ~ ratio_govexp_gdppc +
    ratio_rail_pc +
    ratio_rail_passenger_pc +
    ratio_bn_pc +
    ratio_wage_share +
    ratio_wheat_pc +
    ratio_trade +
    armed_conflicts_internal +
    armed_conflicts_international,
  data = fd_df_clean
)

vif(lm_model)


# Clean FD
fd_model_clean <- plm(
  gdppc_mpd ~ ratio_govexp_gdppc +
    ratio_rail_passenger_pc +
    ratio_bn_pc +
    ratio_wheat_pc +
    ratio_trade +
    armed_conflicts_internal +
    armed_conflicts_international,
  data = pdata_fd,
  model = "fd"
)

coeftest(fd_model_clean, vcov. = function(x) vcovHC(x, type = "HC1", method = "arellano"))

# # #

stargazer(
  fd_model, fd_model_nowage, fd_model_norailkm, fd_model_clean,
  type = "latex",
  title = "First Difference Models: Robustness Checks",
  column.labels = c("Full", "No Wage", "No Rail", "Clean"),
  model.numbers = FALSE,
  dep.var.labels = "Δ GDP per capita (Maddison)",
  digits = 2,
  omit.stat = c("f", "adj.rsq", "ser"),
  style = "default",
  header = FALSE
)


# # #

mf <- model.frame(fd_model_clean)
actual_vals <- model.response(mf)

# Build X matrix directly from model frame
X <- model.matrix(formula(fd_model_clean), data = mf)
beta <- coef(fd_model_clean)
fitted_vals <- as.vector(X %*% beta)

length(actual_vals)
length(fitted_vals)

plot_df <- data.frame(Fitted = fitted_vals, Actual = actual_vals)


ggplot(plot_df, aes(x = Fitted, y = Actual)) +
  geom_point(shape = 16, colour = "black", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linetype = "dashed") +
  labs(
    title = "Fitted vs. Actual Δ GDP per capita",
    x = "Fitted Values",
    y = "Observed First Differences"
  ) +
  theme_minimal() -> p_fitted_actual

ggsave("fig_fd_fitted_vs_actual.png", plot = p_fitted_actual, width = 6, height = 4, dpi = 300)



# # #
# Homoskedasticity, misspecification...

residuals <- actual_vals - fitted_vals

resid_df <- data.frame(
  Fitted = fitted_vals,
  Residuals = residuals
)

p2 <- ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_point(shape = 16, alpha = 0.7, colour = "black") +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "black") +
  labs(
    title = "Residuals vs. Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave("residuals_vs_fitted.png", p2, width = 8, height = 6, dpi = 300)


# # #
# France / Germany

country_vec <- attr(resid_df$Residuals, "index")$country

resid_df <- resid_df %>%
  mutate(Country = country_vec)

str(resid_df)


p3 <- ggplot(resid_df, aes(x = Fitted, y = Residuals, shape = Country)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_point(alpha = 0.7, colour = "black", size = 1.9) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "black") +
  labs(
    title = "Residuals vs. Fitted by Country",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave("residuals_by_country.png", p3, width = 8, height = 6, dpi = 300)

p4 <- ggplot(resid_df, aes(x = Residuals)) +
  geom_histogram(aes(y = ..density..), fill = "gray80", color = "black", bins = 20) +
  geom_density(color = "black", linetype = "dashed") +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals",
    y = "Density"
  ) +
  theme_minimal()

ggsave("residuals_histogram.png", p4, width = 8, height = 6, dpi = 300)

p5 <- ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(color = "black", alpha = 0.7, size = 1.9) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linetype = "dashed") +
  facet_wrap(~Country) +
  labs(
    title = "Residuals vs. Fitted by Country",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave("residuals_facet_country.png", p5, width = 10, height = 6, dpi = 300)


######


vif_df <- data.frame(
  Variable = names(vif(lm_model)),
  VIF = as.numeric(vif(lm_model))
)

p6 <- ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "gray60", color = "black") +
  geom_hline(yintercept = 5, linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Variance Inflation Factors (VIF) – Clean FD Model",
    x = NULL,
    y = "VIF"
  ) +
  theme_minimal()

ggsave("vif_clean_fd_model.png", p6, width = 8, height = 6, dpi = 300)



#####

p7 <- coef_df %>%
  filter(term %in% key_vars) %>%
  mutate(term = case_when(
    term == "ratio_govexp_gdppc" ~ "Gov. Exp / GDP",
    term == "ratio_rail_passenger_pc" ~ "Rail Usage / Capita",
    term == "ratio_wheat_pc" ~ "Wheat Output / Capita",
    term == "armed_conflicts_internal" ~ "Internal Conflict",
    TRUE ~ term
  )) %>%
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, linetype = model, shape = model)) +
  geom_point(color = "black", position = position_dodge(width = 0.8), size = 3.5) +
  geom_errorbar(position = position_dodge(width = 0.8), width = 0.25, size = 0.8, color = "black") +
  coord_flip() +
  labs(
    title = "Coefficient Stability Across Specifications",
    x = NULL,
    y = "Coefficient Estimate",
    linetype = "Model",
    shape = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave("coef_stability_specs.png", p7, width = 10, height = 6.5, dpi = 300)




