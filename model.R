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

# Quick tests:
cat("Call:", bs_price("call", 100, 100, 1, 0.05, 0.2), "\n")
cat("Put: ", bs_price("put",  100, 100, 1, 0.05, 0.2), "\n")
