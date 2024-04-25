library(rvest)
library(tidyverse)
library(magrittr)
library(jsonlite)
library(RQuantLib)
library(quantmod)
library(plotly)

# Parameters
address         <- "C:/Users/janre/Documents/uni/8. Semester/Projekt/data/WebSnapshots_SP500_optionsChains/" # Address for the options chain of SP500
path.data       <- "C:/Users/janre/Documents/uni/8. Semester/Projekt/data/SP500_data.csv" # Path to data with options
load.from.file <- TRUE # If true, then data is loaded from path.data. If false, then
                        # data is downloaded from polygon.io for each symbol in address.
                        # Note, if true, then address parameter is unused.
KEY                 <- "JGWTBya3mqdbU16w43ImnqjPMP7AjrOt"

# Converts a date format YYMMDD to 20YY-MM-DD.
convertToDate <- function(string) {
  temp <- paste0("20", string)
  paste0(substr(temp, 1, 4), "-", substr(temp, 5, 6), "-", substr(temp, 7, 8)) %>%
    return
}

# Converts a date format YYYYMMDD to YYYY-MM-DD.
convertToDate2 <- function(string) {
  paste0(substr(string, 1, 4), "-", substr(string, 5, 6), "-", substr(string, 7, 8)) %>%
    return
}

# Calculates the time in minutes from last trading day to maturity time
TTM <- function(maturity, last.traded) {
  date <- as.POSIXct(maturity)
  time_difference <- difftime(date, last.traded, units = "mins") %>%
    as.numeric %>%
    return
}

# Obtains the data of a given option symbol
getOptionData <- function(symbol, from, to, key = KEY) {
  # Build the API-call
  API <- paste("https://api.polygon.io/v2/aggs/ticker",
               paste("O", symbol, sep = ":"),
               "range/1/minute",
               from,
               to, sep = "/")
  API %<>% paste("?adjusted=true&sort=asc&limit=50000&apiKey=",
                 key, sep = "")

  API %>% fromJSON %>% return
}

if(load.from.file) {
  # Load the data from path.data
  data <- path.data %>% read.csv
} else {
  # Retrieve the options chain
  files <- list.files(address, pattern="*.html")
  optionsChain <- c()
  for(file in files) {
    temp <-
      paste0(address, file) %>%
      read_html %>%
      html_nodes("table.svelte-12t6atp") %>%
      html_table %>%
      lapply(function(df) { df %>% select("Contract Name") })

    optionsChain <- c(optionsChain, temp[[1]] %>% unlist %>% unname)
  }

  # Find data for each call symbol
  #data <- data.frame()
  i <- 262
  n <- optionsChain %>% length
  for(symbol in optionsChain[262:n]) {
    print(paste("Downloading data for:",
                symbol,
                paste0("(", i, "/", n, ")")
    )
    )

    temp <- getOptionData(symbol, "2010-01-01", "2024-04-23")

    # Determine the maturity date, the time where the option was traded, and
    # calculate the time to maturity in minutes.
    maturity <-
      symbol %>%
      {substr(., nchar(.) - 14, nchar(.) - 9)} %>%
      convertToDate %>%
      as.Date

    if(temp$resultsCount == 0) {
      print(paste0(symbol,
                   " havent been traded. Skipping."))
      Sys.sleep(12)
      i <- i + 1
      next
    }

    tradingDays <-
      as.POSIXct(temp$results$t / 1000, origin = "1970-01-01")
      time.to.maturity <- sapply(tradingDays, function(t) { TTM(maturity, t) })

    data %<>% bind_rows(data.frame(type = "call",
                                   stock.symbol = substr(symbol, 1, nchar(symbol) - 15),
                                   option.symbol = symbol,
                                   strike.price = (substr(symbol, nchar(symbol) - 7, nchar(symbol)) %>%
                                                     as.numeric) / 1000,
                                   price = temp$results$vw,
                                   time.to.maturity.in.minutes = time.to.maturity,
                                   trading.date = as.POSIXct(temp$results$t / 1000,
                                                             origin = "1970-01-01") %>%
                                     as.Date
    )
    )
    i <- i + 1
    Sys.sleep(12)
  }

  # Save downloaded data, and return
  write.csv(data,
            file = path.data,
            row.names = FALSE)
}
# Retrieve SP500 price data
getSymbols("^SPX", to = "2024-04-23", from = "2024-01-01")
SPX %<>% as.data.frame
SPX$date <- rownames(SPX)
rownames(SPX) <- 1:nrow(SPX)
SPX %<>% select(SPX.Adjusted, date)
SPX$date %<>% as.Date

# Join the two tables by date
data %<>%
  mutate(date = as.Date(trading.time)) %>%
  left_join(SPX, by = "date")

MINUTES_PR_YEAR <- 60 * 24 * 365
subset_data <- data
IV <- subset_data %>% nrow %>% numeric
sigma <- subset_data$SPX.Adjusted %>% diff %>% sd
for(i in 1:nrow(subset_data)) {
  tryCatch({
    IV[i] <- EuropeanOptionImpliedVolatility(type = subset_data[i,]$type,
                                             value = subset_data[i,]$price,
                                             underlying = subset_data[i,]$SPX.Adjusted,
                                             strike = subset_data[i,]$strike.price,
                                             dividendYield = 0,
                                             riskFreeRate = 0.021,
                                             maturity = subset_data[i,]$time.to.maturity.in.minutes / MINUTES_PR_YEAR,
                                             volatility = sigma)
  },
  error = function(e) {})
}
subset_data
subset_data %<>%
  cbind(IV) %>%
  filter(IV != 0) %>%
  select(strike.price,
         TTM = time.to.maturity.in.minutes,
         IV) %>%
  mutate(TTM = TTM / MINUTES_PR_YEAR)

# Plot the implied volatility surface
fig <- plot_ly(subset_data, x = ~strike.price, y = ~TTM, z = ~IV,
               type = "scatter3d",
               color = ~IV)
fig

strike.price <- c(0, 6, 6, 0)
TTM <- c(12000, 12000, 0, 0)
hist.volatility <- outer(strike.price, TTM,
                         function(strike.price, TTM) {
                           rep(sd(SPX$SPX.Adjusted), length(strike.price))
                          })

plot_ly(x = ~strike.price, y = ~TTM, z = ~hist.volatility) %>%
  add_surface()
