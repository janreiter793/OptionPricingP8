library(tidyverse)
library(tidyquant)
library(magrittr)
library(future.apply)
library(RQuantLib)

# Parameters
r          <- 0.01711 # US treasury bond yield length one year minus inflation rate as
# of 29-04-2024.
files <- c("/srv/scratch/jrsa20/P8-Projekt/data/AMZN_options.csv",
           "/srv/scratch/jrsa20/P8-Projekt/data/BRK-B_options.csv",
           "/srv/scratch/jrsa20/P8-Projekt/data/GOOG_options.csv",
           "/srv/scratch/jrsa20/P8-Projekt/data/GOOGL_options.csv",
           "/srv/scratch/jrsa20/P8-Projekt/data/TSLA_options.csv")
names <- c('AMZN', 'BRK-B', 'GOOG', 'GOOGL', 'TSLA')
foldername <- "test"

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
  
heston_charfunc <- function(phi, S0, v0, kappa, theta, sigma, rho, lambd, tau, r) {
  # constants
  a <- kappa * theta
  b <- kappa + lambd
  
  # common terms w.r.t phi
  rspi <- rho * sigma * phi * 1i
  
  # define d parameter given phi and b
  d <- sqrt((rho * sigma * phi * 1i - b)^2 + (phi * 1i + phi^2) * sigma^2)
  
  # define g parameter given phi, b and d
  g <- (b - rspi + d) / (b - rspi - d)
  
  # calculate characteristic function by components
  exp1 <- exp(r * phi * 1i * tau)
  term2 <- S0^(phi * 1i) * ((1 - g * exp(d * tau)) / (1 - g))^(-2 * a / sigma^2)
  exp2 <- exp(a * tau * (b - rspi + d) / sigma^2 + v0 * (b - rspi + d) * ((1 - exp(d * tau)) / (1 - g * exp(d * tau))) / sigma^2)
  
  return(exp1 * term2 * exp2)
}

heston_price_rec <- function(S0, K, v0, kappa, theta, sigma, rho, lambd, tau, r) {
  args <- c(S0, v0, kappa, theta, sigma, rho, lambd, tau, r)
  
  P <- 0
  umax <- 100
  N <- 10000
  dphi <- umax / N # dphi is width
  
  for (i in 1:(N-1)) {
    # rectangular integration
    phi <- dphi * (2 * i + 1) / 2 # midpoint to calculate height
    numerator <- exp(-r * tau) * do.call(heston_charfunc, as.list(c(phi - 1i, args))) - K * exp(-r * tau)* do.call(heston_charfunc, as.list(c(phi, args)))
    denominator <- 1i * phi * K^(1i * phi)
    
    if(is.nan(numerator)) {
      break
    }
    
    P <- P + dphi * numerator / denominator
  }
  
  return(Re((S0 - K * exp(-r * tau)) / 2 + P / pi))
}

heston_price_rec_vectorized <- Vectorize(heston_price_rec, vectorize.args = c('S0', 'K', 'tau'))

getParams <- function(file) {
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
  
  stock.data <-
    left_join(max.data, stock.data) %>%
    select(date, open)
  S_0 <- stock.data$open
  
  # Join the data
  data %<>%
    filter(date == max.data$date) %>%
    na.omit %>%
    mutate(date = as.Date(date)) %>%
    mutate(tau = TTM(convertToDate(symbol), date) / 52560,
           S_0 = S_0) %>% 
    filter(tau <= 0.5)
  
  # Convert open prices for put to call prices using put-call parity. Also,
  # add the strike prices.
  data %<>%
    mutate(open = ifelse(isPut(symbol),
                         putToCall(open,
                                   getStrikePrice(symbol),
                                   tau,
                                   S_0),
                         open),
           strike = getStrikePrice(symbol))
  
  
  # Parameters to test model
  S0 = S_0 # initial asset price
  K = data$strike # strike
  tau = data$tau # time to maturity
  lambd = 0#0.575 # risk premium of variance
  
  errorfunc <- function(params, minimize = T) {
    v0 <- params[1] %>% abs
    kappa <- params[2] %>% abs
    sigma = params[3] %>% abs
    theta = params[4] %>% abs
    rho = params[5] %>% {. / (1 + abs(.))}
    
    err <- heston_price_rec_vectorized(S_0, K, v0, kappa, theta, sigma, rho, lambd, tau, r) - data$open
    mse <- mean(err^2)
    if(minimize) {
      return(mse)
    } else {
      return(list(sqrt(mse), mean(abs(err)))) 
    }
  }
  
  res <- optim(par = c(0.0654, 0.6067, 0.0707, 0.2928, -0.7571),
               fn = errorfunc)
  
  transformed_parameters <- res$par
  transformed_parameters[1:4] %<>%  abs
  transformed_parameters[5] %<>% {. / (1 + abs(.))}
  return(list(errorfunc(res$par, minimize = F), 
              transformed_parameters
  )
  )
}

plan(multisession(workers = 5))
conc <- future_lapply(files, getParams)
saveRDS(conc, file = "~/projekt/dumps/NonFFTMethod2.RData")
conc
