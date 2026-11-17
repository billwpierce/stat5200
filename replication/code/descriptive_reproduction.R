## Garbe, Maerz & Freyburg (2026), Democratization 33(1): 85-108
## Descriptive statistics (Supplemental Material, Appendix 2) -- simplified version.
## Sample: African autocracies (V-Dem Regimes of the World codes 0-1), by country-year.

library(vdemdata)   # vdem: V-Dem v16
library(ERT)        # episodes: Episodes of Regime Transformation
options(scipen = 999)

years <- 2000:2021   # Appendix 2's Ns fit 2000-2021 (765 with V-Dem v14, 766 with v16); the text says 2000-2022

## ---- 1. TOSCO: ISP ownership by country-year ----
tosco <- read.csv("data/tosco2.csv")
pre_entry <- tosco$year < tosco$company_market_entry        # ISP listed before its recorded market entry
tosco <- tosco[!(pre_entry %in% TRUE), ]                    # drop those (missing entry years are kept)

# TOSCO country names -> V-Dem country names
fix <- c("Congo Brazzaville" = "Republic of the Congo", "Congo Kinshasa" = "Democratic Republic of the Congo",
         "Cote D'Ivoire" = "Ivory Coast", "Gambia" = "The Gambia", "Swaziland" = "Eswatini",
         "USA" = "United States of America", "Hongkong" = "Hong Kong", "Lybia" = "Libya")
rename <- function(x) ifelse(x %in% names(fix), fix[x], x)
tosco$host <- rename(tosco$country)              # country the ISP operates in
tosco$hq   <- rename(tosco$sh_group_hq)          # country the shareholder is registered in
tosco$owner_hq <- rename(tosco$sh_mother_group_hq)   # country of a second-level shareholder (an owner of the shareholder)

# TOSCO repeats each (first-level) shareholder once per second-level shareholder, i.e. per owner of that
# shareholder. First add up how much of each shareholder the host government owns through those owners,
# then keep one row per shareholder.
key <- do.call(paste, c(tosco[c("year", "company_id", "sh_group_name", "sh_share_r")], sep = "|"))
host_govt_owner <- tosco$sh_mother_type %in% "state" & !is.na(tosco$owner_hq) & tosco$owner_hq == tosco$host
owners <- unique(data.frame(key, owner = tosco$sh_mother_group_name, share = tosco$sh_mother_share_r,
                            govt = host_govt_owner))   # each owner once (rows also repeat per 3rd/last-level owner)
govt_share <- tapply(ifelse(owners$govt, owners$share, 0), owners$key, sum, na.rm = TRUE)
tosco$govt_owned <- as.vector(govt_share[key])   # % of the shareholder held by the host government

# Majority (> 50%) second-level owner of each shareholder (per year), i.e. the owner controlling its voting
# rights -- used for shareholders registered where V-Dem has no data
top_owner <- tosco[order(-tosco$sh_mother_share_r), c("year", "sh_group_name", "owner_hq", "sh_mother_share_r")]
top_owner <- top_owner[!duplicated(top_owner[c("year", "sh_group_name")]) &
                       (top_owner$sh_mother_share_r > 50) %in% TRUE, ]

tosco <- tosco[!duplicated(key), ]

# Country whose regime classifies the shareholder: its own, or -- if V-Dem doesn't cover it (e.g. British
# Virgin Islands) -- that of its majority second-level owner (one level up only, which matches Appendix 8);
# without a majority owner the shareholder stays unclassified (foreign, but neither autocratic nor democratic)
tosco$regime_country <- tosco$hq
need <- !is.na(tosco$hq) & !tosco$hq %in% c(vdem$country_name, "unknown")
tosco$regime_country[need] <- top_owner$owner_hq[match(paste(tosco$year, tosco$sh_group_name),
                                                        paste(top_owner$year, top_owner$sh_group_name))[need]]

# Regime of the shareholder's home country (0-1 = autocracy, 2-3 = democracy)
tosco <- merge(tosco, setNames(vdem[c("country_name", "year", "v2x_regime")], c("regime_country", "year", "hq_regime")),
               all.x = TRUE)

# Each shareholder's stake (%), counted towards the owner groups it belongs to
foreign <- tosco$hq != tosco$host & !tosco$hq %in% c("unknown", NA)
state   <- tosco$hq == tosco$host &          # host government itself, or a shareholder it owns > 50% of
  (tosco$sh_majority_type %in% "state" | tosco$govt_owned > 50)
tosco$f  <- tosco$sh_share_r * foreign
tosco$fa <- tosco$sh_share_r * (foreign & tosco$hq_regime %in% 0:1)
tosco$fd <- tosco$sh_share_r * (foreign & tosco$hq_regime %in% 2:3)
tosco$st <- tosco$sh_share_r * state

# ISP-year: is more than 50% held by each owner group?
isp <- aggregate(cbind(f, fa, fd, st) ~ host + year + company_id, tosco, sum, na.rm = TRUE, na.action = na.pass)
isp[c("f", "fa", "fd", "st")] <- isp[c("f", "fa", "fd", "st")] > 50
isp$n <- 1

# Country-year: shares of ISPs, and the two dummies
cy <- aggregate(cbind(f, fa, fd, st, n) ~ host + year, isp, sum)
d <- with(cy, data.frame(host, year, n_isp = n,
                         share_foreign = f / n, share_foreign_aut = fa / n, share_foreign_dem = fd / n,
                         treatment = as.numeric(fa > 0),   # at least one ISP majority foreign-autocratic
                         state     = as.numeric(st > 0)))  # at least one ISP majority state-owned

## ---- 2. V-Dem: sample and V-Dem variables ----
v <- vdem[vdem$year %in% years & vdem$v2x_regime %in% 0:1,
          c("country_name", "country_text_id", "COWcode", "year", "v2smgovsmmon", "v2smgovfilprc", "v2smgovshut",
            "v2cltrnslw", "v2x_ex_hereditary", "v2x_ex_military", "v2x_polyarchy", "e_pt_coup_attempts", "e_pt_coup")]
names(v)[1:3] <- c("host", "iso3", "cow")
d <- merge(d, v)   # keeps autocratic country-years only
d$online_rep <- pmax(-d$v2smgovsmmon, -d$v2smgovfilprc, -d$v2smgovshut)   # higher = more repression

## ---- 3. ERT: autocratization episodes ----
d <- merge(d, setNames(episodes[c("country_text_id", "year", "aut_ep")], c("iso3", "year", "aut_ep")), all.x = TRUE)

## ---- 4. Fariss human rights scores, inverted (higher = more repression) ----
fariss <- read.csv("data/HumanRightsProtectionScores_v4.01.csv")
fariss <- data.frame(cow = fariss$COW, year = fariss$YEAR,
                     rep_fariss = max(fariss$theta_mean, na.rm = TRUE) - fariss$theta_mean)
d <- merge(d, fariss, all.x = TRUE)

## ---- 5. UCDP/PRIO: number of active conflicts the government is a primary party to ----
ucdp <- read.csv("data/UcdpPrioConflict_v26_1.csv")
sides <- do.call(rbind, lapply(c("gwno_a", "gwno_b"), function(col) {
  codes <- strsplit(as.character(ucdp[[col]]), ",\\s*")   # a few cells list several countries
  data.frame(cow = as.integer(unlist(codes)), year = rep(ucdp$year, lengths(codes)),
             conflict = rep(ucdp$conflict_id, lengths(codes)))
}))
d <- merge(d, aggregate(conflict ~ cow + year, unique(sides), length), all.x = TRUE)
d$conflict[is.na(d$conflict)] <- 0
# Authors' version (replication code): intrastate conflicts only, government as side A
intra <- ucdp[ucdp$type_of_conflict == 3, ]
intra <- aggregate(conflict_id ~ gwno_a + year, intra, length)
names(intra) <- c("cow", "year", "conflict_intra")
intra$cow <- as.integer(intra$cow)
d <- merge(d, intra, all.x = TRUE)
d$conflict_intra[is.na(d$conflict_intra)] <- 0

## ---- 6. World Bank indicators (bulk WDI file; saved to data/wdi_cache after the first run) ----
ind <- c(gdppc = "NY.GDP.PCAP.CD", gdppc_growth = "NY.GDP.PCAP.KD.ZG", natres = "NY.GDP.TOTL.RT.ZS",
         pop_rural = "SP.RUR.TOTL", pop_total = "SP.POP.TOTL", trade = "NE.TRD.GNFS.ZS")
dir.create("data/wdi_cache", showWarnings = FALSE)
cache <- file.path("data/wdi_cache", paste0(ind, ".rds"))
if (!all(file.exists(cache))) {
  zip <- "data/WDI_CSV.zip"   # ~270 MB; can be deleted once the cache exists
  if (!file.exists(zip)) {
    options(timeout = 3600)
    download.file("https://databank.worldbank.org/data/download/WDI_CSV.zip", zip, mode = "wb")
  }
  files <- unzip(zip, list = TRUE)
  wdi <- read.csv(unz(zip, files$Name[which.max(files$Length)]), check.names = FALSE)   # the main data file
  yrs <- grep("^20(0[0-9]|1[0-9]|2[0-2])", names(wdi), value = TRUE)                    # columns 2000-2022
  for (i in seq_along(ind)) {
    w <- wdi[wdi[["Indicator Code"]] == ind[i], ]
    saveRDS(data.frame(iso3 = rep(w[["Country Code"]], times = length(yrs)),
                       year = rep(as.integer(substr(yrs, 1, 4)), each = nrow(w)),
                       value = as.numeric(unlist(w[yrs]))), cache[i])
  }
}
for (i in seq_along(ind))
  d <- merge(d, setNames(readRDS(cache[i]), c("iso3", "year", names(ind)[i])), all.x = TRUE)

## ---- 7. Appendix 2 table ----
vars <- c("Share foreign" = "share_foreign", "Share foreign-authoritarian" = "share_foreign_aut",
          "Share foreign-democratic" = "share_foreign_dem", "Online repression" = "online_rep",
          "Repression (Fariss)" = "rep_fariss", "Treatment indicator" = "treatment",
          "GDP pc" = "gdppc", "GDP pc growth" = "gdppc_growth", "Nat. resources rents (% GDP)" = "natres",
          "Population rural" = "pop_rural", "Population total" = "pop_total", "Trade (% of GDP)" = "trade",
          "Legal transparency" = "v2cltrnslw", "Hereditary" = "v2x_ex_hereditary", "Military" = "v2x_ex_military",
          "Autocratization" = "aut_ep", "Conflict" = "conflict", "Conflict (intrastate, side A)" = "conflict_intra",
          "Coup" = "e_pt_coup_attempts", "Coup (successful)" = "e_pt_coup",
          "Polyarchy" = "v2x_polyarchy", "Number of ISPs" = "n_isp", "State involvement" = "state")
desc <- t(sapply(d[vars], function(x) c(Minimum = min(x, na.rm = TRUE), Maximum = max(x, na.rm = TRUE),
                                        Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), N = sum(!is.na(x)))))
rownames(desc) <- names(vars)

options(scipen = 999, width = 200)
cat("Country-years:", nrow(d), "| Countries:", length(unique(d$host)), "\n")
print(round(desc, 2))

saveRDS(d, "data/replication_data.rds")