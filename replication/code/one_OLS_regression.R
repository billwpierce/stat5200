## Garbe, Maerz & Freyburg (2026), Democratization 33(1): 85-108
## Here, I reproduce the six OLS regression models.
## The reproduced models are outlined in Appendix 4 of the Supplemental Material.
## Graphic results are displayed in Figures
library(ggeffects)
library(ggplot2)
library(dplyr)
library(broom)
library(patchwork)
library(modelsummary)
library(tinytable)
library(sandwich)
library(lmtest)


d <- readRDS("data/replication_data.rds")

d$rf <- as.numeric(scale(d$rep_fariss))
d$log_pop_total <- as.numeric(scale(log(d$pop_total)))
d$rural_percent_total <- as.numeric(scale(d$pop_rural / d$pop_total))
d$military_control <- as.numeric(scale(d$v2x_ex_military))
d$hereditary_control <- as.numeric(scale(d$v2x_ex_hereditary))
d$log_gdp <- as.numeric(scale(log(d$gdppc)))
d$gdp_growth <- as.numeric(scale(d$gdppc_growth))
d$natres_percent_gdp <- as.numeric(scale(d$natres))
d$t <- as.numeric(scale(d$trade))
d$leg_trans <- as.numeric(scale(d$v2cltrnslw))
d$state_isp <- as.numeric(scale(d$state))
d$host_factor <- as.factor(d$host)
d$year_factor <- as.factor(d$year) # change this to as.numeric() to replicate their error.

## 1. Share of Foreign ISPs ~ Repression (Fariss)
model1 <- lm(share_foreign ~ rf + host_factor + year_factor, data = d)

## 2. Share of Foreign ISPs ~ All variables.
model2 <- lm(
  share_foreign ~ rf + log_pop_total + rural_percent_total + military_control +
    hereditary_control + log_gdp + gdp_growth + natres_percent_gdp + t +
    leg_trans + state_isp + host_factor + year_factor,
  data = d
)

## 3. Share of Foreign Autocratic ISPs ~ Repression (Fariss)
model3 <- lm(share_foreign_aut ~ rf + host_factor + year_factor, data = d)

## 4. Share of Foreign Autocratic ISPs ~ All variables.
model4 <- lm(
  share_foreign_aut ~ rf + log_pop_total + rural_percent_total + military_control +
    hereditary_control + log_gdp + gdp_growth + natres_percent_gdp + t +
    leg_trans + state_isp + host_factor + year_factor,
  data = d
)

## 5. Share of Foreign Democratic ISPs ~ Repression (Fariss)
model5 <- lm(share_foreign_dem ~ rf + host_factor + year_factor, data = d)

## 6. Share of Foreign Democratic ISPs ~ All variables.
model6 <- lm(
  share_foreign_dem ~ rf + log_pop_total + rural_percent_total + military_control +
    hereditary_control + log_gdp + gdp_growth + natres_percent_gdp + t +
    leg_trans + state_isp + host_factor + year_factor,
  data = d
)

## Figure 2A
# Calculate adjusted predictions for Petal.Length
pred4 <- ggeffect(model4, terms = "rf", ci_level = 0.9) %>% mutate(investor_type = "Foreign Autocratic")
pred6 <- ggeffect(model6, terms = "rf", ci_level = 0.9) %>% mutate(investor_type = "Foreign Democratic")
combined <- bind_rows(pred4, pred6)
p2a <- ggplot(combined, aes(x = x, y = predicted, color = investor_type, fill = investor_type)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  labs(
    x = "Repression (Fariss)",
    y = "Predicted Share of ISPs",
    color = "Investor Type",
    fill = "Investor Type"
  ) +
  ylim(0, 0.5)

## Figure 2B
# The original graph contained an error, as it excluded some non-factor regression coefficients.
tidy_model4 <- tidy(model4, conf.int = TRUE, conf.level = 0.9) %>%
  filter(
    term != "(Intercept)",
    !grepl("factor", term), # Excludes any term containing "factor"
    term != "log_pop_total",
    term != "hereditary_control"
  ) %>% 
  mutate(investor_type = "Foreign Autocratic")

tidy_model6 <- tidy(model6, conf.int = TRUE, conf.level = 0.9) %>%
  filter(
    term != "(Intercept)",
    !grepl("factor", term), # Excludes any term containing "factor"
    term != "log_pop_total",
    term != "hereditary_control"
  ) %>% 
  mutate(investor_type = "Foreign Democratic")

combined_tidy <- bind_rows(tidy_model4, tidy_model6)

custom_labels <- c(
  "rural_percent_total" = "Rural Pop. (%)",
  "t" = "Trade (% GDP)",
  "log_gdp" = "GDP pc (log)",
  "leg_trans" = "Legal Transp.",
  "rf" = "Repression (Fariss)",
  "natres_percent_gdp" = "Nat. Res. (% GDP)",
  "gdp_growth" = "GDP pc growth",
  "military_control" = "Military Cont.",
  "state_isp" = "State-Owned ISP",
  "hereditary_control" = "Hereditary Cont."
)

p2b <- ggplot(combined_tidy, aes(x = estimate, y = reorder(term, estimate), color = investor_type, fill = investor_type)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.5), size = 0.8) +
  scale_y_discrete(labels = custom_labels) +
  labs(
    title = "Regression Coefficients",
    x = "Estimate",
    y = "",
    color = "Investor Type",
    fill = "Investor Type"
  )

p2b <- p2b + guides(colour = "none", fill = "none")

investor_cols <- c("Foreign Autocratic" = "#2c7bb6", "Foreign Democratic" = "#d7191c")  # blue / red, as in the article
shared_scales <- list(scale_colour_manual(values = investor_cols, name = "Investor Type"),
                      scale_fill_manual(values = investor_cols, name = "Investor Type"))

final_plot <- (p2a + shared_scales) + (p2b + shared_scales) +
  plot_layout(widths = c(1, 1.4), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

ggsave("results/figure2.pdf", final_plot, width = 11, height = 5.5)   # inches; try 10–12 wide

## Appendix 4, Table
models <- list("(1)" = model1, "(2)" = model2, "(3)" = model3,
               "(4)" = model4, "(5)" = model5, "(6)" = model6)

labels <- c("rf" = "Repression (Fariss)", "log_pop_total" = "Total Population (log)",
            "rural_percent_total" = "Rural Population (% of total)", "military_control" = "Military Control",
            "hereditary_control" = "Hereditary Control", "log_gdp" = "GDP pc (log)",
            "gdp_growth" = "GDP pc growth", "natres_percent_gdp" = "Natural Resources (% of GDP)",
            "t" = "Trade (% of GDP)", "leg_trans" = "Legal Transparency",
            "state_isp" = "State-owned ISP", "(Intercept)" = "Constant")

fe_rows <- data.frame(term = c("Year FE", "Country FE"),
                      matrix("Yes", nrow = 2, ncol = 6))

tab <- modelsummary(models,
  coef_map  = labels,                                  # order and names; drops the FE dummies
  estimate  = "{estimate} ({std.error}){stars}",       # SE beside the estimate, as in the paper
  statistic = NULL,
  stars     = c("+" = 0.1, "*" = 0.05, "**" = 0.01, "***" = 0.001),
  notes = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  add_rows  = fe_rows,
  fmt       = 3,
  title     = "Regression Analysis (Fariss)") |>
  group_tt(j = list("Share of Foreign ISPs" = 2:3,
                    "Share of Foreign Autocratic ISPs" = 4:5,
                    "Share of Foreign Democratic ISPs" = 6:7))

save_tt(tab, "results/appendix4_fariss.png", overwrite = TRUE)   # or .html, .tex, .png

## Checking OLS Assumptions
## OLS.1: Covariation
models_to_plot <- list(model4 = model4, model6 = model6)

for (name in names(models_to_plot)) {
  m  <- models_to_plot[[name]]
  mf <- model.frame(m)
  xs <- setdiff(attr(terms(m), "term.labels"), c("factor(host)", "factor(year)"))

  png(paste0("results/", name, "_resid_vs_x.png"), width = 11, height = 8, units = "in", res = 300)
  par(mfrow = n2mfrow(length(xs)))
  for (x in xs) {
    plot(mf[[x]], resid(m), xlab = x, ylab = "Residual", pch = 20, col = "grey60")
    abline(h = 0, lty = 2)
    lines(lowess(mf[[x]], resid(m)), col = "red")
  }
  dev.off()
}

## OLS.3, Optional: Uncomment the following lines to get clustered standard errors for models 4 and 6.
# coeftest(model4, vcov = vcovCL(model4, cluster = ~ host))
# coeftest(model6, vcov = vcovCL(model6, cluster = ~ host))