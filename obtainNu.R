library(tidyverse)
library(magrittr)

# Parameters
files <- c("~/data/AMZN_options.csv",
           "~/data/BRK-B_options.csv",
           "~/data/GOOG_options.csv",
           "~/data/GOOGL_options.csv",
           "~/data/TSLA_options.csv")

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
putToCall <- function(p, K, tau, x) {
  return(p - K * exp(-r * tau) + x)
}

obtainVega <- function(file) {
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

  # Join the data
  data %<>%
    filter(date == max.data$date) %>%
    na.omit

  # Add the strike prices.
  data %<>%
    mutate(strike = getStrikePrice(symbol))

  # Calculate a fitting vega
  k_min <- data$strike %>% min %>% log
  k_max <- data$strike %>% max %>% log
  return(pi / max(c(abs(k_min), abs(k_max))))
}

res <- numeric(length(files))
for(i in 1:length(files)) {
  res[i] <- obtainVega(files[i])
}
res %<>% as.data.frame()
rownames(res) <- c("AMZN", "BRK-B", "GOOG", "GOOGL", "TSLA")
colnames(res) <- "nu"
res
