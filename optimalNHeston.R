library(tidyverse)
library(tidyquant)
library(magrittr)
library(future.apply)

# Parameters
r          <- 0.01711 # US treasury bond yield length one year minus inflation rate as
# of 29-04-2024.
files <- c("~/data/AMZN_options.csv",
           "~/data/BRK-B_options.csv",
           "~/data/GOOG_options.csv",
           "~/data/GOOGL_options.csv",
           "~/data/TSLA_options.csv")
alpha = 1.5

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

obtainHestonMetrics <- function(N, file) {
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
           S_0 = S_0)
  
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
  
  # Calculate a fitting vega, lambda, k, b, and v
  k_min <- data$strike %>% min %>% log
  k_max <- data$strike %>% max %>% log
  vega <- pi / max(c(abs(k_min), abs(k_max)))
  lambda <- 2 * pi / (N * vega)
  k <- -pi / vega + lambda * (1:N - 1)
  b <- pi / vega
  v <- vega * (0:(N - 1))
  
  # Find all different maturity times
  maturity.times <- data$tau %>% unique
  
  # The characteristic function of the Heston model
  phi <- function(v, sigma_0_sqrd, kappa, eta, theta, rho, M) {
    d <- sqrt((rho * theta * v * 1i - kappa)^2 - theta^2 * (-1i * v - v^2))
    g <- (kappa - rho * theta * v * 1i - d) / (kappa - rho * theta * v * 1i + d)
    
    return(
      exp(1i * v * (log(S_0) + r * M)) *
        exp(eta * kappa / theta^2 * ((kappa - rho * theta * v * 1i - d) * M -
                                       2 * log((1 - g * exp(-d * M)) /
                                                 (1 - g)))) *
        exp(sigma_0_sqrd / theta^2 * (kappa - rho * theta * 1i * v - d) *
              (1 - exp(-d * M)) / (1 - g * exp(-d * M)))
      
    )
  }
  
  # psi function
  psi <- function(v, sigma_0_sqrd, kappa, eta, theta, rho, M) {
    return(
      exp(-r * M) * phi(v - (alpha + 1) * 1i, sigma_0_sqrd, kappa, eta,
                        theta, rho, M) / (alpha^2 + alpha - v^2 +
                                            (2 * alpha + 1) * v * 1i)
    )
  }
  
  # The kronecker delta function
  kronecker_delta <- function(n) {
    return(n == 0)
  }
  
  # Calculate the sequence for which to perform fft on
  x <- function(sigma_0_sqrd, kappa, eta, theta, rho, M) {
    return(exp(1i * b * v) * psi(v, sigma_0_sqrd, kappa, eta, theta, rho, M) *
             vega / 3 * (3 + (-1)^(1:N) - kronecker_delta(0:(N - 1))))
  }
  
  # Calculates the option surface based on parameters
  optionSurface <- function(sigma_0_sqrd, kappa, eta, theta, rho) {
    #cat(paste("Calculating option surface using parameters:", "\n",
    #          "sigma_0_sqrd =", sigma_0_sqrd, "\n",
    #          "kappa =", kappa, "\n",
    #          "eta =", eta, "\n",
    #          "theta =", theta, "\n",
    #          "rho =", rho,  "\n"))
    surface <- matrix(nrow = length(maturity.times), ncol = N)
    for(i in 1:length(maturity.times)) {
      surface[i,] <- abs(exp(-alpha * k) / pi * fft(x(sigma_0_sqrd, kappa, eta,
                                                      theta, rho, maturity.times[i])))
    }
    return(surface)
  }
  
  # For all maturity times and all prices find the error
  ErrorFunction <- function(params, minimize = T) {
    sigma_0_sqrd <- abs(params[1])
    kappa <- params[2]
    eta <- params[3]
    theta <- params[4]
    rho <- params[5] / (1 + abs(params[5]))
    
    surface <- optionSurface(sigma_0_sqrd, kappa, eta, theta, rho)
    C <- surface[cbind(closest.maturity.index, closest.strike.index)]
    err <- {abs(data$open - C)^2} %>% sum
    #cat(paste("MSE:", err / nrow(data), "\n"))
    if(minimize) {
      {err/nrow(data)} %>% return
    } else {
      err_abs <- {abs(data$open - C)} %>% sum
      c(RMSE = sqrt(err/nrow(data)), MAE = err_abs / nrow(data)) %>% return
    }
  }
  
  initial_params <- c(sigma_0_sqrd = 0.0654, 
                      kappa = 0.6067, 
                      eta = 0.0707,
                      theta = 0.2928,
                      rho = -0.7571)
  
  # Find the closes k_u to the real strike price
  closest.maturity.index <- data %>% nrow %>% numeric
  closest.strike.index <- data %>% nrow %>% numeric
  for(i in 1:nrow(data)) {
    closest.maturity.index[i] <- which(maturity.times == data$tau[i])
    closest.strike.index[i] <- which.min(abs(k - log(data$strike[i])))
  }
  
  res <- optim(initial_params, ErrorFunction, method = "SANN",
               control = list(
                 temp = 10,
                 tmax = 10
               ))
  
  transformed_parameters <- res$par
  transformed_parameters[1] %<>%  abs
  transformed_parameters[5] %<>% {. / (1 + abs(.))}
  return(c(ErrorFunction(res$par, minimize = F)))
}

N <- 2^(8:16)
res <- list()
for(i in 1:length(N)) {
  cat(paste("Conducting multisession for N =", N[i], "\n"))
  plan(multisession(workers = 5))
  res[[i]] <- future_lapply(files, obtainHestonMetrics, N = N[i])
  cat("\nDone!\n")
}
