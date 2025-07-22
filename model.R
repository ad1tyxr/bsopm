compute_d1_d2 <- function(S0, K, T, r, sigma) {
  d1 <- (log(S0 / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  list(d1 = d1, d2 = d2)
}

bs_price <- function(type, S0, K, T, r, sigma) {
  ds <- compute_d1_d2(S0, K, T, r, sigma)
  d1 <- ds$d1; d2 <- ds$d2

  if (tolower(type) == "call") {
    S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (tolower(type) == "put") {
    K * exp(-r * T) * pnorm(-d2) - S0 * pnorm(-d1)
  } else {
    stop("Option type must be 'call' or 'put'.")
  }
}

cat("Enter option type ('call' or 'put'): ")
opt_type <- readline()

cat("Enter S0 (current stock price): ")
S0 <- as.numeric(readline())

cat("Enter K (strike price): ")
K <- as.numeric(readline())

cat("Enter T (time to maturity, in years): ")
T <- as.numeric(readline())

cat("Enter r (risk-free rate, decimal): ")
r <- as.numeric(readline())

cat("Enter sigma (volatility, decimal): ")
sigma <- as.numeric(readline())

price <- bs_price(opt_type, S0, K, T, r, sigma)
cat(sprintf("\nBlack–Scholes %s price: %.4f\n", tolower(opt_type), price))
