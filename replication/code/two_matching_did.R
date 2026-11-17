## Garbe, Maerz & Freyburg (2026) -- Part 2: effect of foreign-autocratic investment on online repression
## Reproduces Figure 4 with PanelMatch (Imai, Kim & Wang 2023). Requires PanelMatch >= 3
## (install the CRAN binary: install.packages("PanelMatch")).
## Input: data/replication_data.rds from appendix2_simple.R

library(PanelMatch)
library(vdemdata)    # only needed if offline_repression = "vdem"
library(ggplot2)

## ---- Settings the paper leaves open ----
# Covariate history: 1:2 = the two pre-treatment years, as in the Figure 4 caption; 0:2 adds the treatment year
cov_lags <- "1:2"
se_method <- "bootstrap"   # authors' code; "unconditional" = analytical SEs

## ---- Data ----
d <- readRDS("data/replication_data.rds")
d$log_gdp <- log(d$gdppc)
d$log_pop <- log(d$pop_total)
d$offline_rep <- d$rep_fariss
d$unit_id <- as.integer(factor(d$host))   # PanelMatch needs integer unit and time IDs
d$year    <- as.integer(d$year)
d_all <- d   # full sample, for the Appendix 8 grid

## ---- Covariates: listed on p. 97 and in Appendices 11-12, z-standardized ----
# State-owned ISPs, GDP (log), GDP growth, population (log), EDI, autocratization (ERT),
# coups, conflict, offline repression, number of ISPs. (Appendix 11 says coups were left out of
# its balance plot "due to little variation"; Appendix 12 lists them as controls, so they are kept.)
covs <- c("state", "log_gdp", "gdppc_growth", "log_pop", "v2x_polyarchy", "aut_ep",
          "e_pt_coup", "conflict_intra", "offline_rep", "n_isp")
d <- d[complete.cases(d[c(covs, "treatment", "online_rep")]), ]   # as the authors' drop_na()
d[covs] <- lapply(d[covs], function(x) as.numeric(scale(x)))

pd <- PanelData(panel.data = d, unit.id = "unit_id", time.id = "year",
                treatment = "treatment", outcome = "online_rep")

## ---- Matching ----
covs_formula <- as.formula(paste("~", paste0("I(lag(", covs, ", ", cov_lags, "))", collapse = " + ")))

pm <- PanelMatch(panel.data = pd,
                 lag = 2,                         # L = 2: two-year treatment and covariate history
                 lead = 0:3,                      # F = 3: effects up to three years after treatment
                 qoi = "att",
                 refinement.method = "ps.weight", # propensity-score weighting
                 covs.formula = covs_formula,
                 placebo.test = TRUE)             # needed for the t-1 estimate

## ---- Estimates: asymptotic standard errors, with placebo test ----
set.seed(1)
pe <- PanelEstimate(sets = pm, panel.data = pd,
                    se.method = se_method, number.iterations = 1000,
                    confidence.level = 0.95,
                    include.placebo.test = TRUE)
s95 <- summary(pe)
s90 <- summary(pe, confidence.level = 0.90)
print(s95); print(s90)
print(names(pe$placebo.test$estimates))

## ---- Covariate balance (Appendix 11) ----
bal <- get_covariate_balance(pm, panel.data = pd, covariates = covs)
plot(bal)

## ---- Figure 4 ----
pl <- pe$placebo.test
p_est <- unname(tail(pl$estimates, 1)); p_se <- unname(tail(pl$standard.errors, 1))
fig4 <- data.frame(period = c(-1, 0:3),
                   est  = c(p_est, s95[, 1]),
                   lo95 = c(p_est - qnorm(0.975) * p_se, s95[, 3]), hi95 = c(p_est + qnorm(0.975) * p_se, s95[, 4]),
                   lo90 = c(p_est - qnorm(0.95)  * p_se, s90[, 3]), hi90 = c(p_est + qnorm(0.95)  * p_se, s90[, 4]))

p4 <- ggplot(fig4, aes(x = period, y = est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_linerange(aes(ymin = lo95, ymax = hi95), colour = "black", linewidth = 0.5) +  # 95%
  geom_linerange(aes(ymin = lo90, ymax = hi90), colour = "grey60", linewidth = 2) +   # 90%
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = -1:3) +
  labs(x = "Years around treatment", y = "Estimated effect",
       title = "Effects of foreign-autocratic investment on online repression") +
  theme_classic()
print(p4)
ggsave("results/figure4.pdf", p4, width = 7, height = 4.5)

ms   <- PanelMatch::extract(pm, qoi = "att")
lags <- 1:2   # must match the lags in covs.formula

# one row per treated country-year and per matched control (same year)
key  <- do.call(rbind, strsplit(names(ms), ".", fixed = TRUE))
rows <- do.call(rbind, lapply(seq_along(ms), function(i)
  data.frame(unit_id = c(as.integer(key[i, 1]), as.integer(ms[[i]])),
             year    = as.integer(key[i, 2]),
             treated = c(1, rep(0, length(ms[[i]]))))))

# lagged covariates, looked up by country and year
X <- sapply(as.vector(outer(covs, lags, paste, sep = "|")), function(vk) {
  v <- sub("\\|.*", "", vk); k <- as.integer(sub(".*\\|", "", vk))
  d[[v]][match(paste(rows$unit_id, rows$year - k), paste(d$unit_id, d$year))] })

# as PanelMatch does: missing -> 0 plus a missing indicator, duplicates and constant columns removed
miss <- is.na(X) * 1; colnames(miss) <- paste0(colnames(X), "_NA")
X[is.na(X)] <- 0
pooled <- unique(data.frame(rows, X, miss, check.names = FALSE))
pooled <- pooled[, c(rep(TRUE, 3), sapply(pooled[-(1:3)], function(z) length(unique(z)) > 1))]

fit <- glm(treated ~ ., family = binomial, data = pooled[, -(1:2)])
pooled$ps <- fitted(fit)

ggplot(pooled, aes(x = ps, fill = factor(treated))) +
  geom_density(alpha = 0.4) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(x = "Propensity Score", y = "density", fill = "Treatment Status") +
  theme_minimal()

# country codes as in the paper (TOSCO's 'cc' column uses the same three-letter codes)
tosco_cc <- unique(read.csv("data/tosco2.csv")[c("country", "cc")])
fix <- c("Congo Brazzaville" = "Republic of the Congo", "Congo Kinshasa" = "Democratic Republic of the Congo",
         "Cote D'Ivoire" = "Ivory Coast", "Gambia" = "The Gambia", "Swaziland" = "Eswatini")
tosco_cc$host <- ifelse(tosco_cc$country %in% names(fix), fix[tosco_cc$country], tosco_cc$country)
d8 <- merge(d_all, tosco_cc[c("host", "cc")])
d8 <- d8[d8$year <= 2018, ]                    # the paper's figure covers 2000-2018

# most-treated countries at the top, as in the paper
row_order <- names(sort(tapply(d8$treatment, d8$cc, mean)))

ggplot(d8, aes(x = year, y = factor(cc, levels = row_order), fill = factor(treatment))) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("0" = "#21908C", "1" = "#CC79A7"),
                    labels = c("Untreated", "Treated"), name = NULL) +
  scale_x_continuous(breaks = 2000:2018, expand = c(0, 0)) +
  labs(x = NULL, y = NULL, title = "Distribution of Treatment Variable") +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))