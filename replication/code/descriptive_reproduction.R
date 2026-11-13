## Garbe, Maerz & Freyburg (2026), Democratization 33(1): 85-108
## Reproduce Appendix 2 (Descriptive statistics) of the Supplemental Material.
## Unit: country-year, African autocracies (V-Dem RoW 0/1), 2000-2022.

library(vdemdata)   # V-Dem (vdem data frame)
library(ERT)        # Episodes of Regime Transformation (episodes data frame)
options(scipen = 999)

## ---- File paths: edit to match your downloads ----
tosco_file  <- "data/tosco2.csv"
fariss_file <- "data/HumanRightsProtectionScores_v4.01.csv"
ucdp_file   <- "data/UcdpPrioConflict_v26_1.csv"

## ---- 1. TOSCO 2.0 ----
# TOSCO country names that differ from V-Dem's country_name
to_vdem <- c("Congo Brazzaville" = "Republic of the Congo", "Congo Kinshasa" = "Democratic Republic of the Congo",
             "Cote D'Ivoire" = "Ivory Coast", "Gambia" = "The Gambia", "Swaziland" = "Eswatini",
             "USA" = "United States of America", "Hongkong" = "Hong Kong")
rn <- function(x) ifelse(x %in% names(to_vdem), to_vdem[x], x)

tosco <- read.csv(tosco_file)
tosco$host <- rn(tosco$country)
tosco$hq   <- rn(tosco$sh_group_hq)   # country where the shareholder group is registered

# Rows repeat once per parent group / individual owner: keep one row per ISP x shareholder x year
# (the row for the shareholder's largest parent, used for state ownership below)
tosco <- tosco[order(-tosco$sh_mother_share_r), ]
tosco <- tosco[!duplicated(tosco[c("year", "company_id", "sh_group_name", "sh_share_r")]), ]

# Regime type of the shareholder's home country (RoW: 0/1 = autocracy, 2/3 = democracy)
tosco <- merge(tosco, setNames(vdem[c("country_name", "year", "v2x_regime")], c("hq", "year", "hq_regime")),
               all.x = TRUE)

foreign  <- !is.na(tosco$hq) & tosco$hq != "unknown" & tosco$hq != tosco$host
state    <- !is.na(tosco$hq) & tosco$hq == tosco$host &
  (tosco$sh_majority_type %in% "state" |
     (tosco$sh_mother_type %in% "state" & tosco$sh_mother_share_r > 50))
tosco$f  <- tosco$sh_share_r * foreign
tosco$fa <- tosco$sh_share_r * (foreign & tosco$hq_regime %in% 0:1)
tosco$fd <- tosco$sh_share_r * (foreign & tosco$hq_regime %in% 2:3)
tosco$st <- tosco$sh_share_r * state

# ISP-year: majority (> 50%) held by each owner group, summed across shareholders
isp <- aggregate(cbind(f, fa, fd, st) ~ host + year + company_id, data = tosco,
                 FUN = sum, na.rm = TRUE, na.action = na.pass)
isp[c("f", "fa", "fd", "st")] <- isp[c("f", "fa", "fd", "st")] > 50
isp$n <- 1

# Country-year
cy <- aggregate(cbind(f, fa, fd, st, n) ~ host + year, data = isp, FUN = sum)
cy$share_foreign     <- cy$f  / cy$n
cy$share_foreign_aut <- cy$fa / cy$n
cy$share_foreign_dem <- cy$fd / cy$n
cy$treatment         <- as.numeric(cy$fa > 0)  # >= 1 ISP majority foreign-autocratic
cy$state             <- as.numeric(cy$st > 0)  # >= 1 ISP majority state-owned
cy$n_isp             <- cy$n

## ---- 2. V-Dem: sample definition + V-Dem variables ----
v <- vdem[vdem$year %in% 2000:2022,
          c("country_name", "country_text_id", "COWcode", "year", "v2x_regime",
            "v2smgovsmmon", "v2smgovfilprc", "v2smgovshut", "v2cltrnslw",
            "v2x_ex_hereditary", "v2x_ex_military", "v2x_polyarchy", "e_pt_coup")]
names(v)[1:3] <- c("host", "iso3", "cow")
d <- merge(cy, v, by = c("host", "year"))
d <- d[d$v2x_regime %in% 0:1, ]   # African autocracies only

# Online repression = maximum of the three DSP items, inverted so higher = more repression
d$online_rep <- pmax(-d$v2smgovsmmon, -d$v2smgovfilprc, -d$v2smgovshut)

## ---- 3. ERT: autocratization episodes ----
# `episodes` ships with the ERT package: it is get_eps() run with default settings
# (see data-raw/episodes.R in the ERT repo), so no need to recompute it.
d <- merge(d, setNames(episodes[, c("country_text_id", "year", "aut_ep")], c("iso3", "year", "aut_ep")),
           all.x = TRUE)

## ---- 4. Fariss et al. Latent Human Rights Protection Scores (v4), inverted ----
fariss <- read.csv(fariss_file); names(fariss) <- tolower(names(fariss))
# max(theta) - theta: reproduces Appendix 2 exactly (3.43, 7.89, 5.87, 0.90, N = 696)
fariss$rep_fariss <- max(fariss$theta_mean, na.rm = TRUE) - fariss$theta_mean
d <- merge(d, fariss[, c("cow", "year", "rep_fariss")], all.x = TRUE)

## ---- 5. UCDP/PRIO: number of active conflicts the country is a primary party to ----
ucdp <- read.csv(ucdp_file)
parties <- do.call(rbind, lapply(c("gwno_a", "gwno_b"), function(col) {   # side A / side B governments
  cc <- strsplit(as.character(ucdp[[col]]), ",\\s*")                   # GW codes = COW codes for these countries
  data.frame(cow = suppressWarnings(as.integer(unlist(cc))),
             year = rep(ucdp$year, lengths(cc)), id = rep(ucdp$conflict_id, lengths(cc)))
}))
parties <- unique(parties[!is.na(parties$cow), ])                       # each conflict counted once per country-year
parties$conflict <- 1
ucdp <- aggregate(conflict ~ cow + year, data = parties, FUN = sum)
d <- merge(d, ucdp, all.x = TRUE)
d$conflict[is.na(d$conflict)] <- 0

## ---- 6. World Bank WDI (bulk file, no API) ----
# Full WDI database as one zip (~270 MB) from the World Bank's DataBank server.
# Downloaded once; the six indicators are then saved to wdi_cache/ and the zip is no longer needed.
ind <- c(gdppc = "NY.GDP.PCAP.CD", gdppc_growth = "NY.GDP.PCAP.KD.ZG",
         natres = "NY.GDP.TOTL.RT.ZS", pop_rural = "SP.RUR.TOTL",
         pop_total = "SP.POP.TOTL", trade = "NE.TRD.GNFS.ZS")
dir.create("data/wdi_cache", showWarnings = FALSE)
cache <- file.path("data/wdi_cache", paste0(ind, ".rds"))
if (!all(file.exists(cache))) {
  zip_file <- "WDI_CSV.zip"
  if (!file.exists(zip_file)) {
    options(timeout = 3600)   # large file: allow up to an hour
    download.file("https://databank.worldbank.org/data/download/WDI_CSV.zip", zip_file, mode = "wb")
  }
  files <- unzip(zip_file, list = TRUE)
  con <- unz(zip_file, files$Name[which.max(files$Length)])   # main data file (largest CSV)
  open(con)
  header <- readLines(con, n = 1)
  keep <- character(0)
  repeat {   # read in chunks, keeping only rows for our six indicators
    x <- readLines(con, n = 50000)
    if (length(x) == 0) break
    keep <- c(keep, x[Reduce(`|`, lapply(ind, function(code) grepl(code, x, fixed = TRUE)))])
  }
  close(con)
  wdi <- read.csv(text = c(header, keep), check.names = FALSE)
  yrs <- grep("^(2000|20(0[1-9]|1[0-9]|2[0-2]))", names(wdi), value = TRUE)   # 2000-2022 columns
  for (i in seq_along(ind)) {
    w <- wdi[wdi[["Indicator Code"]] == ind[[i]], ]
    saveRDS(data.frame(iso3  = rep(w[["Country Code"]], times = length(yrs)),
                       year  = rep(as.integer(substr(yrs, 1, 4)), each = nrow(w)),
                       value = as.numeric(unlist(w[yrs]))), cache[i])
  }
}
for (i in seq_along(ind)) {
  nm <- names(ind)[i]
  d[[nm]] <- NULL   # safe to re-run this section: drop the column if already merged
  d <- merge(d, setNames(readRDS(cache[i]), c("iso3", "year", nm)), all.x = TRUE)
}

## ---- 7. Appendix 2 table ----
vars <- c("Share foreign" = "share_foreign", "Share foreign-authoritarian" = "share_foreign_aut",
          "Share foreign-democratic" = "share_foreign_dem", "Online repression" = "online_rep",
          "Repression (Fariss)" = "rep_fariss", "Treatment indicator" = "treatment",
          "GDP pc" = "gdppc", "GDP pc growth" = "gdppc_growth",
          "Nat. resources rents (% GDP)" = "natres", "Population rural" = "pop_rural",
          "Population total" = "pop_total", "Trade (% of GDP)" = "trade",
          "Legal transparency" = "v2cltrnslw", "Hereditary" = "v2x_ex_hereditary",
          "Military" = "v2x_ex_military", "Autocratization" = "aut_ep",
          "Conflict" = "conflict", "Coup" = "e_pt_coup", "Polyarchy" = "v2x_polyarchy",
          "Number of ISPs" = "n_isp", "State involvement" = "state")

desc <- t(sapply(vars, function(v) {
  x <- as.numeric(d[[v]])
  c(Minimum = min(x, na.rm = TRUE), Maximum = max(x, na.rm = TRUE),
    Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), N = sum(!is.na(x)))
}))
cat("Country-years:", nrow(d), "| Countries:", length(unique(d$host)), "\n")
print(round(desc, 2))

saveRDS(d, file.path("data", "replication_data.rds"))