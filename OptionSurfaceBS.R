library(tidyverse)
library(plotly)
library(magrittr)

d1 <- function(x, t) {
  (log(x / K) + (r + 0.5 * v_sqrd) * (t_star - t)) / (sqrt(v_sqrd * (t_star - t)))
}

d2 <- function(x, t) {
  (log(x / K) + (r - 0.5 * v_sqrd) * (t_star - t)) / (sqrt(v_sqrd * (t_star - t)))
}

C <- function(x, t) {
  x * pnorm(d1(x, t)) - K * exp(r * (t - t_star)) * pnorm(d2(x, t))
}

# Option parameters
t_star <- 1 # Maturity time
K <- 15       # Strike price
v_sqrd <- 5   # variance rate of return
r <- 0.07     # short term interest rate

# Generate the surface points
x <- seq(0, K*2,    by = 6)
t <- seq(0, t_star, by = 0.2)
df <- expand.grid(x, t)
colnames(df) <- c("x", "t")
df %<>% mutate(C = C(x, t))

C <- xtabs(C ~ t + x, data = df)

# Plot the points in surface plot
plot_ly(x = ~x, y = ~t, z = ~C, type = "surface") %>% layout(
  scene = list(
    xaxis = list(title = "s"),
    yaxis = list(title = "t"),
    zaxis = list(title = "C(s,t)")
  )
)
