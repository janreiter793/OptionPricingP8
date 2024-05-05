library(tidyverse)
library(tidyquant)
library(magrittr)

# Parameters
files <- c("~/data/AMZN_options.csv",
           "~/data/BRK-B_options.csv",
           "~/data/GOOG_options.csv",
           "~/data/GOOGL_options.csv",
           "~/data/TSLA_options.csv")
r    <- 0.01711 # US treasury bond yield length one year minus inflation rate as
                # of 29-04-2024.

# Takes an option symbol and returns the associated underlying asset symbol
getUnderlyingSymbol <- function(symbol) {
  temp <- substr(symbol, 1, nchar(symbol) - 15)
  if(temp == "BRKB") { return("BRK-B") }
  return(temp)
}

# Returns the strike price of an asset
getStrikePrice <- function(symbol) {
  return(as.numeric(substr(symbol, nchar(symbol) - 7, nchar(symbol))) / 1000)
}

# d1 for calculating the BS price
d1 <- function(x, K, v_sqrd, tau, r) {
  (log(x / K) + (r + 0.5 * v_sqrd) * tau) / (sqrt(v_sqrd * tau))
}

# d2 for calculating the BS price
d2 <- function(x, K, v_sqrd, tau, r) {
  (log(x / K) + (r - 0.5 * v_sqrd) * tau) / (sqrt(v_sqrd * tau))
}

# The black and scholes price
C <- function(x, K, v_sqrd, tau, r) {
  x * pnorm(d1(x, K, v_sqrd, tau, r)) - K * exp(- r * tau) * pnorm(d2(x, K, v_sqrd, tau, r))
}

# Calculates the time in minutes from last trading day to maturity time
TTM <- function(maturity, last.traded) {
  date <- as.POSIXct(maturity)
  time_difference <- difftime(date, last.traded, units = "mins") %>%
    as.numeric %>%
    return
}

# Converts a date format YYMMDD to 20YY-MM-DD.
convertToDate <- function(symbol) {
  string <- substr(symbol, nchar(symbol) - 14, nchar(symbol) - 9)
  temp <- paste0("20", string)
  paste0(substr(temp, 1, 4), "-", substr(temp, 5, 6), "-", substr(temp, 7, 8)) %>%
    as.Date %>%
    return
}

# Detect if symbol is put
isPut <- function(symbols) {
  res <- symbols %>% length %>% logical

  for(i in 1:length(symbols)) {
    if(substr(symbols[i], nchar(symbols[i]) - 8, nchar(symbols[i]) - 8) == "P") {
      res[i] <- TRUE
    } else {
      res[i] <- FALSE
    }
  }

  return(res)
}

# Uses the put call parity to convert put price to call price
putToCall <- function(p, K, r, tau, x) {
  return(p - K * exp(-r * tau) + x)
}

# Takes a file address, where option opening prices are stored, and returns the
# Black and Scholes RMSE, MAE, and mean open price (MEAN).
obtainBSMetrics <- function(file) {
  # Load the data and count for each date the amount of rows where we have avail-
  # able open prices for the options
  data <- read.csv(file)

  count.data.for.dates <-
    data %>%
    group_by(date) %>%
    summarise(count = sum(!is.na(open))) %>%
    as.data.frame

  # Find the row with the highest number of available data
  max.data <-
    count.data.for.dates[which.max(count.data.for.dates$count), ] %>%
    mutate(date = as.Date(date))

  # Retrieve opening prices for the associated stock
  stock.data <-
    data[1,1] %>%
    getUnderlyingSymbol %>%
    tq_get(get = "stock.prices") %>%
    select(date, open) %>%
    mutate(date = as.Date(date))

  # Only use stock date from one year back to now
  stock.data %<>%
    filter(date <= max.data$date, date >= (max.data$date - 365))

  # Calculate the volatility of the stock based on Hull section 15.4
  n <- nrow(stock.data)
  u <- log(stock.data$open[2:n] / stock.data$open[1:(n - 1)])
  s <- sd(u)
  vol <- s / sqrt(1 / n)  # tau is time interval, which averages to 1 / n

  # Join the data
  data %<>%
    filter(date == max.data$date) %>%
    na.omit %>%
    mutate(date = as.Date(date)) %>%
    left_join(stock.data, by = "date") %>%
    cbind(vol, r) %>%
    mutate(tau = TTM(convertToDate(symbol), date) / 52560)

  # Convert open prices for put to call prices
  data %<>%
    mutate(open.x = ifelse(isPut(symbol),
                           putToCall(open.x,
                                     getStrikePrice(symbol),
                                     r,
                                     tau,
                                     open.y),
                           open.x))

  # Calculate the BS-prices
  BS_prices <-
    data %>%
    mutate(BS.price = C(open.y, getStrikePrice(symbol), vol^2, tau, r)) %>%
    select(open.x, BS.price)

  # Calculate sum of squared errors
  sum.sqrd.errors <-
    BS_prices %>%
    mutate(sqrd.errors = (open.x - BS.price)^2) %>%
    select(sqrd.errors) %>%
    sum

  # Mean absolute errors
  MAE <-
    BS_prices %>%
    mutate(abs.errors = abs(open.x - BS.price)) %>%
    select(abs.errors) %>%
    unlist %>% unname %>%
    mean

  RMSE <- sqrt(sum.sqrd.errors / nrow(BS_prices))

  MEAN <- mean(data$open.x)

  return(c(RMSE, MAE, MEAN))
}

# Obtain and store the metrics
metrics <- c()
i <- 1
n <- length(files)
for(file in files) {
  print(paste("Running:",
              i,
              "out of",
              n))
  metrics <- c(metrics, obtainBSMetrics(file))
  i <- i + 1
}

# Create a neat data frame displaying the results
results <- data.frame(RMSE = metrics[1:n * 3 - 2],
                      MAE  = metrics[1:n * 3 - 1],
                      MEAN = metrics[1:n * 3])
rownames(results) <- c("AMZN", "BRK-B", "GOOG", "GOOGL", "TSLA")

# The results
results %>% print
