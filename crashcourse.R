library(ggplot2)
library(tidyquant)
library(tidyverse)
library(dplyr)
library(GGally)
library(quadprog)



# -------------------------------------------------
## Visualization Code- Adding the visualization code here, in case you want to mess around with it 
## (just add each companys ticker to the tickers list and have at it)
# -------------------------------------------------

tickers <- c("GOOG","JNJ","WMT")

# Get monthly returns for all stocks
returns_data <- tq_get(tickers,get="stock.prices",from="2005-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(
    select=adjusted,
    mutate_fun=periodReturn,
    period="monthly",
    col_rename="monthly_return"
  ) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from=symbol,values_from=monthly_return)

# Remove rows with missing data
returns_data_clean <- na.omit(returns_data)

# View a correlation scatterplot matrix
ggpairs(
  returns_data_clean[,-1],
  title="Monthly Return Correlation Matrix: GOOG, JNJ, WMT"
)


# -------------------------------------------------
## Sharpe_Ratio/Jensen_Alpha
## I used chat (sinful i know ) to spew out a list of 15 random companies, added them below,then edited the code from the worksheet with them 
## if you have any others ones you want to add, just add them to the list
# -------------------------------------------------

organizations <- c("AAPL", "CHWY", "DIS", "GRMN", "COST","MSFT", "NKE", "SBUX", "TGT", "F", "NFLX", "KO", "MCD", "DAL", "SPOT", "CMG", "HD", "WMT", "TSLA", "ADBE")
## Feel free to add any additional organizations here

company_returns <- tq_get(
  organizations,
  get = "stock.prices",
  from = "2010-01-01"
) %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "monthly_return"
  ) %>%
  ungroup()

sp500 <- tq_get(
  "^GSPC",
  get = "stock.prices",
  from = "2010-01-01"
) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    col_rename = "market_return"
  )


returns <- company_returns %>%
  left_join(sp500, by = "date")

rf_annual <- 0.0432


rf_monthly <- rf_annual / 12


returns <- returns %>%
  mutate(
    excess_company = monthly_return - rf_monthly,
    excess_market = market_return - rf_monthly
  )

company_metrics <- returns %>%
  group_by(symbol) %>%
  group_modify(~ {
    
    model <- lm(
      excess_company ~ excess_market,
      data = .x
    )
    
    alpha <- coef(model)[1]
    beta <- coef(model)[2]
    
    sharpe_ratio <- mean(
      .x$excess_company,
      na.rm = TRUE
    ) /
      sd(
        .x$monthly_return,
        na.rm = TRUE
      )
    
    tibble(
      Sharpe_Ratio = sharpe_ratio,
      Beta = beta,
      Jensen_Alpha_Monthly = alpha,
      Jensen_Alpha_Annualized = (1 + alpha)^12 - 1,
    )
  }) %>%
  ungroup()

company_metrics <- company_metrics %>%
  mutate(
    Sharpe_Ratio = round(Sharpe_Ratio, 3),
    Beta = round(Beta, 3),
    Jensen_Alpha_Monthly = round(Jensen_Alpha_Monthly, 5),
    Jensen_Alpha_Annualized = round(Jensen_Alpha_Annualized, 4),
  ) %>%
  as.data.frame()


print(company_metrics)

company_returns_clean <- na.omit(company_returns)

# -------------------------------------------------
## Final Picker
## Based on the Sharpe ratio, we choose the following stocks to optimize 
## the weights of: AAPL, COST, HD, MSFT, NFLX
## These are the ones with a sharpe above .2, except for Tesla
# -------------------------------------------------

tickers <- c("AAPL","COST","HD", "MSFT", "NFLX")

# Get monthly returns for all stocks
returns_data <- tq_get(tickers,get="stock.prices",from="2005-01-01") %>%
  group_by(symbol) %>%
  tq_transmute(
    select=adjusted,
    mutate_fun=periodReturn,
    period="monthly",
    col_rename="monthly_return"
  ) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from=symbol,values_from=monthly_return)

# Remove rows with missing data
returns_data_clean <- na.omit(returns_data)

# View a correlation scatterplot matrix
ggpairs(
  returns_data_clean[,-1],
  title="Monthly Return Correlation Matrix"
)

portfolio_returns <- returns_data_clean %>%
  select(-date)
R <- as.matrix(portfolio_returns)

# Mean returns & covariance (monthly)
mu <- colMeans(R)
Sigma <- cov(R)

# Risk-free rate (monthly)
rf <- rf_monthly

# Sharpe ratio function (negative for minimization)
neg_sharpe <- function(w, mu, Sigma, rf) {
  w <- w / sum(w) # enforce sum of weights = 1
  port_ret <- sum(w * mu)
  port_sd <- sqrt(t(w) %*% Sigma %*% w)
  - (port_ret - rf) / port_sd
}

# Initial guess (equal weights)
n_assets <- ncol(R)
w0 <- rep(1 / n_assets, n_assets)

# Optimize (no short-selling)
opt <- optim(
  par = w0,
  fn = neg_sharpe,
  mu = mu,
  Sigma = Sigma,
  rf = rf,
  method = "L-BFGS-B",
  lower = rep(0, n_assets),
  upper = rep(1, n_assets)
)

# Optimal weights
weights <- opt$par / sum(opt$par)
names(weights) <- colnames(R)
print("Optimal Portfolio Weights (Max Sharpe Ratio):")
round(weights, 4)

# Portfolio performance
portfolio_return <- sum(weights * mu)
portfolio_sd <- sqrt(t(weights) %*% Sigma %*% weights)
portfolio_sharpe <- (portfolio_return - rf) / portfolio_sd
cat("\nPortfolio Expected Monthly Return:", round(portfolio_return, 4))
cat("\nPortfolio Monthly Volatility:", round(portfolio_sd, 4))
cat("\nPortfolio Sharpe Ratio:", round(portfolio_sharpe, 4))
